extern crate pest;
extern crate pest_derive;

mod ast;
mod functions;
mod parser;

fn main() {
    // let tree = parser::parse(r#""\a\b\f\n\r\t\v\\\" - \xFF\377\u1234\U00010111\U0001011111☺""#)
    let tree = parser::parse(r#"`abc`"#).unwrap_or_else(|e| panic!("{}", e));
    println!("{:#?}", tree);
}
