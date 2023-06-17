extern crate pest;
extern crate pest_derive;

mod ast;
mod functions;
mod parser;

fn main() {
    let tree = parser::parse("up[10s5ms]").unwrap_or_else(|e| panic!("{}", e));
    println!("{:#?}", tree);
}
