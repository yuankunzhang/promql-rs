extern crate pest;
extern crate pest_derive;

mod ast;
mod functions;
mod parser;

fn main() {
    let tree = parser::parse("foo + bar or bla and blub").unwrap_or_else(|e| panic!("{}", e));
    println!("{:#?}", tree);
}
