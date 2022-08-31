#![allow(clippy::just_underscores_and_digits)]
#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub template);

mod ast;
use ast::{Expr, Opcode};

extern crate serde;
use serde::{Deserialize, Serialize};

use ::std::collections::HashMap;
use regex::Regex;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
pub type YamlMap = HashMap<String, YamlObject>;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Scalar {
    String(String),
    Number(i64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum YamlObject {
    Scalar(Scalar),
    Array(Vec<YamlObject>),
    Map(YamlMap),
}

macro_rules! yaml_unwrap_scalar {
    ($e:expr, $T:tt) => {{
        let expr = $e;
        match expr {
            YamlObject::Scalar(object) => match object {
                Scalar::$T(e) => Ok(e),
                _ => Err(format!("expected {}  - got {:?}", stringify!($T), object)),
            },
            _ => Err(format!("expected {}  - got {:?}", stringify!($T), expr)),
        }
    }};
}

pub fn eval_array(arr: &Vec<Box<Expr>>, env: &YamlMap) -> Result<Vec<YamlObject>> {
    let mut yaml_array: Vec<YamlObject> = Vec::new();
    for sub_expr in arr.iter() {
        yaml_array.push(eval_expr(sub_expr, env)?);
    }
    Ok(yaml_array)
}

pub fn eval_map(map: &Vec<(Box<Expr>, Box<Expr>)>, env: &YamlMap) -> Result<YamlMap> {
    let mut yaml_map: YamlMap = HashMap::new();
    for sub_expr in map.iter() {
        let key = yaml_unwrap_scalar!(eval_expr(&sub_expr.0, env)?, String)?;
        yaml_map.insert(key, eval_expr(&sub_expr.1, env)?);
    }
    Ok(yaml_map)
}

pub fn yaml_array_get_expr(array: &Vec<YamlObject>, index: &YamlObject) -> Result<YamlObject> {
    let i = yaml_unwrap_scalar!(index, Number)?;
    if array.len() <= *i as usize {
        return Err("index exceeds array bounds")?;
    }
    Ok(array[*i as usize].clone())
}

pub fn yaml_map_get_expr(map: &YamlMap, key: &YamlObject) -> Result<YamlObject> {
    let k = yaml_unwrap_scalar!(key, String)?;
    if !map.contains_key(k) {
        return Err("key not found")?;
    }
    Ok(map[k].clone())
}

macro_rules! merge_yaml_objects {
    ($yaml_type:tt, $e0:expr, $e1:expr, $return_type:ty) => {{
        let e1 = $e1;
        if let YamlObject::$yaml_type(obj1) = e1 {
            Ok(YamlObject::$yaml_type(
                $e0.into_iter()
                    .chain(obj1.into_iter())
                    .collect::<$return_type>(),
            ))
        } else {
            Err(format!(
                "cannot append {} with {:?}",
                stringify!($yaml_type),
                e1
            ))
        }
    }};
}

pub fn eval_binary_operands(
    expr0: &Expr,
    opcode: &Opcode,
    expr1: &Expr,
    env: &YamlMap,
) -> Result<YamlObject> {
    let val0 = eval_expr(expr0, env)?;
    let val1 = eval_expr(expr1, env)?;
    let result = match opcode {
        Opcode::Plus => match val0 {
            YamlObject::Array(array0) => merge_yaml_objects!(Array, array0, val1, Vec<YamlObject>)?,
            YamlObject::Map(map0) => merge_yaml_objects!(Map, map0, val1, YamlMap)?,
            YamlObject::Scalar(scalar0) => match scalar0 {
                Scalar::String(string0) => {
                    let string1 = yaml_unwrap_scalar!(val1, String)?;
                    YamlObject::Scalar(Scalar::String(string0 + &string1))
                }
                Scalar::Number(num0) => {
                    let num1 = yaml_unwrap_scalar!(val1, Number)?;
                    YamlObject::Scalar(Scalar::Number(num0 + num1))
                }
                Scalar::Bool(bool0) => {
                    let bool1 = yaml_unwrap_scalar!(val1, Bool)?;
                    YamlObject::Scalar(Scalar::Bool(bool0 && bool1))
                }
            },
        },
    };
    Ok(result)
}

pub fn eval_expr(expr: &Expr, env: &YamlMap) -> Result<YamlObject> {
    let result = match expr {
        Expr::String(e) => YamlObject::Scalar(Scalar::String(e.to_string())),
        Expr::Num(e) => YamlObject::Scalar(Scalar::Number(*e)),
        Expr::Bool(e) => YamlObject::Scalar(Scalar::Bool(*e)),
        Expr::Ident(i) => {
            if env.contains_key(i) {
                let mut new_env = env.clone();
                new_env.remove(i);
                expand(&new_env, &env[i])?
            } else {
                return Err(format!("undefined variable {}", i))?;
            }
        }
        Expr::Array(arr) => YamlObject::Array(eval_array(arr, env)?),
        Expr::Map(map) => YamlObject::Map(eval_map(map, env)?),
        Expr::TermRef((term_expr, index_expr)) => {
            let object = eval_expr(term_expr, env)?;
            let index = eval_expr(index_expr, env)?;
            match object {
                YamlObject::Array(array) => yaml_array_get_expr(&array, &index)?,
                YamlObject::Map(map) => yaml_map_get_expr(&map, &index)?,
                _ => {
                    return Err(format!("can not index {:?}", object))?;
                }
            }
        }
        Expr::Op(e0, op, e1) => eval_binary_operands(e0, op, e1, env)?,
    };
    Ok(result)
}

pub fn evaluate_template(env: &YamlMap, string: &String) -> Result<YamlObject> {
    let parser = template::ExprParser::new();
    let expr = parser.parse(string).unwrap();
    Ok(eval_expr(&expr, env)?)
}

pub fn expand_string(env: &YamlMap, string: &String) -> Result<YamlObject> {
    let re = Regex::new(r"\{\{(.*)\}\}").unwrap();
    let shortest_match = re.shortest_match(string);
    let capture = match shortest_match {
        Some(pos) => re.captures(&string[..pos]).unwrap(),
        None => return Ok(YamlObject::Scalar(Scalar::String(string.clone()))),
    };
    let captured_string = capture.get(1).unwrap().as_str().to_string();
    let evaluated_template = evaluate_template(env, &captured_string)?;
    /* match evaluated template.
       if scalar, replace and expand string.
       else if dict or map, return dict or map IFF string drained
       else error
    */
    let mut is_string_result = false;
    let result = match evaluated_template {
        YamlObject::Scalar(scalar) => match scalar {
            Scalar::String(templated_str) => {
                let expanded_string = string.replacen(
                    &capture.get(0).unwrap().as_str().to_string(),
                    &templated_str,
                    1,
                );
                is_string_result = true;
                expand_string(env, &expanded_string)?
            }
            _ => YamlObject::Scalar(scalar.clone()),
        },
        YamlObject::Map(map) => expand_map(env, &map)?,
        YamlObject::Array(array) => expand_array(env, &array)?,
    };
    if captured_string.len() + 4 != string.len() && !is_string_result {
        // this means we evaluated as a non-string yaml object
        // and there were trailing characters in the string
        return Err("can not template non-string YamlObjects into string")?;
    }
    Ok(result)
}

pub fn expand_map(env: &YamlMap, map: &YamlMap) -> Result<YamlObject> {
    let mut new_map: YamlMap = HashMap::new();
    for (k, v) in map {
        new_map.insert(k.to_string(), expand(env, v)?);
    }
    Ok(YamlObject::Map(new_map))
}

pub fn expand_array(env: &YamlMap, array: &Vec<YamlObject>) -> Result<YamlObject> {
    let mut new_array: Vec<YamlObject> = Vec::new();
    for v in array.iter() {
        new_array.push(expand(env, v)?);
    }
    Ok(YamlObject::Array(new_array))
}

pub fn expand(env: &YamlMap, yaml: &YamlObject) -> Result<YamlObject> {
    let result = match yaml {
        YamlObject::Scalar(scalar) => match scalar {
            Scalar::String(string) => expand_string(env, string)?,
            _ => yaml.clone(),
        },
        YamlObject::Map(map) => expand_map(env, map)?,
        YamlObject::Array(array) => expand_array(env, array)?,
    };
    Ok(result)
}

pub fn map_get_vars(obj: &YamlObject, vars_key: &str) -> Result<YamlMap> {
    let vars = match &obj {
        YamlObject::Map(map) => {
            if map.contains_key(vars_key) {
                match &map[vars_key] {
                    YamlObject::Map(vars) => vars.clone(),
                    _ => return Err("expected variable definitions to be of type Map")?,
                }
            } else {
                HashMap::new()
            }
        }
        _ => return Err("expected yaml map")?,
    };
    Ok(vars)
}

pub fn reserialize<T: serde::de::DeserializeOwned>(obj: &YamlObject) -> Result<T> {
    Ok(serde_yaml::from_str(&serde_yaml::to_string(&obj)?)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    pub const YAML: &str = r#"
dictionary:
  one: 1
  two: 2
  three: 2
list:
  - four
  - five
  - six
  - "{{ number }} {{ letter }}"
  - "[8,9,10]"
  - "{{ map }}"
vars:
  number: seven
  letter: B
  letter2: "{{ letter }}"
  map:
    key1: val1
    key2: val2
  list2:
    - seven
    - eight
    - nine
"#;

    #[test]
    fn test_eval_expr() -> Result<()> {
        let obj: YamlObject = serde_yaml::from_str(YAML).expect("could not parse");
        let env = match &obj {
            YamlObject::Map(map) => match &map["vars"] {
                YamlObject::Map(vars) => vars,
                _ => panic!("no vars section"),
            },
            _ => panic!("no vars section"),
        };
        let tests = vec![
            // Test binary operands and identifiers
            (r#""hello""#, r#"Scalar(String("hello"))"#),
            (r#"3"#, r#"Scalar(Number(3))"#),
            (r#"letter"#, r#"Scalar(String("B"))"#),
            (
                r#"[1,2,3]"#,
                r#"Array([Scalar(Number(1)), Scalar(Number(2)), Scalar(Number(3))])"#,
            ),
            (r#"[1,2,3][2]"#, r#"Scalar(Number(3))"#),
            (r#"list2[1]"#, r#"Scalar(String("eight"))"#),
            (
                r#"{ "key0": "val0", "key1": "val1"}["key1"]"#,
                r#"Scalar(String("val1"))"#,
            ),
            (r#"map["key2"]"#, r#"Scalar(String("val2"))"#),
            (
                r#"[1,2,3] + [4,5,6]"#,
                r#"Array([Scalar(Number(1)), Scalar(Number(2)), Scalar(Number(3)), Scalar(Number(4)), Scalar(Number(5)), Scalar(Number(6))])"#,
            ),
            (
                r#"( {"key0": "val0"} + {"key1" : "val1"})["key0"]"#,
                r#"Scalar(String("val0"))"#,
            ),
            (r#""hello" + " world""#, r#"Scalar(String("hello world"))"#),
            (r#"3 + 5"#, r#"Scalar(Number(8))"#),
            (r#"true + true"#, r#"Scalar(Bool(true))"#),
            (r#"false + false"#, r#"Scalar(Bool(false))"#),
            (r#"true + false"#, r#"Scalar(Bool(false))"#),
            (r#"false + true"#, r#"Scalar(Bool(false))"#),
            (r#"letter2"#, r#"Scalar(String("B"))"#),
            (r#""''""#, r#"Scalar(String("''"))"#),
            (r#"'""'"#, r#"Scalar(String("\"\""))"#),
            //(r#""#, r#""#),
        ];
        let parser = template::ExprParser::new();
        for test in tests {
            let expr = parser.parse(test.0).unwrap();
            let evaluated_expr = eval_expr(&expr, &env);
            match evaluated_expr {
                Ok(result) => {
                    eprintln!("{}", test.0);
                    eprintln!("{:?}\n", result);
                    assert_eq!(format!("{:?}", result), test.1);
                }
                Err(_) => {
                    assert_eq!("Error", test.1);
                }
            }
        }
        Ok(())
    }

    #[test]
    fn test_parser() -> Result<()> {
        let tests = vec![
            // Test binary operands and identifiers
            (
                r#"ident_a + ident_b + ident_c"#,
                r#"Op(Op(Ident("ident_a"), Plus, Ident("ident_b")), Plus, Ident("ident_c"))"#,
            ),
            (
                r#"ident_a + (ident_b + ident_c)"#,
                r#"Op(Ident("ident_a"), Plus, Op(Ident("ident_b"), Plus, Ident("ident_c")))"#,
            ),
            // Test string literals
            (
                r#""hello" + " world""#,
                r#"Op(String("hello"), Plus, String(" world"))"#,
            ),
            // Test arrays and integers
            (r#"[1,2,3]"#, r#"Array([Num(1), Num(2), Num(3)])"#),
            // Test maps
            (
                r#"{ "key0": "val0", "key1": "val1"}"#,
                r#"Map([(String("key0"), String("val0")), (String("key1"), String("val1"))])"#,
            ),
            // Test nested structures
            (
                r#"{ "array0" : [1,2,3], "array1": [4,5,6]}"#,
                r#"Map([(String("array0"), Array([Num(1), Num(2), Num(3)])), (String("array1"), Array([Num(4), Num(5), Num(6)]))])"#,
            ),
            // TermRef literal
            (
                r#"[1,2,3][1]"#,
                r#"TermRef((Array([Num(1), Num(2), Num(3)]), Num(1)))"#,
            ),
            // TermRef
            (r#"array[1]"#, r#"TermRef((Ident("array"), Num(1)))"#),
            // Mapref
            (
                r#"{ "key0": "val0", "key1": "val1"}["val0"]"#,
                r#"TermRef((Map([(String("key0"), String("val0")), (String("key1"), String("val1"))]), String("val0")))"#,
            ),
            (
                r#"[1,2,3] + [4,5,6]"#,
                r#"Op(Array([Num(1), Num(2), Num(3)]), Plus, Array([Num(4), Num(5), Num(6)]))"#,
            ),
            (r#"true"#, r#"Bool(true)"#),
            (r#"false"#, r#"Bool(false)"#),
            //(r#""#, r#""#),
        ];
        let parser = template::ExprParser::new();
        for test in tests {
            let expr = parser.parse(test.0).unwrap();
            eprintln!("{}", test.0);
            eprintln!("{:?}\n", expr);
            assert_eq!(format!("{:?}", expr), test.1);
        }
        Ok(())
    }

    #[test]
    fn it_works() -> Result<()> {
        let obj: YamlObject = serde_yaml::from_str(YAML).expect("could not parse");
        let vars = map_get_vars(&obj, "vars")?;
        eprintln!("{}", serde_yaml::to_string(&expand(&vars, &obj)?).unwrap());
        Ok(())
    }
}
