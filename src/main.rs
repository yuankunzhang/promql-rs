extern crate pest;
extern crate pest_derive;

mod ast;
mod functions;
mod parser;

fn main() {
    let tree = parser::parse("bar + on(foo) bla / on(baz, buz) group_right(test) blub")
        .unwrap_or_else(|e| panic!("{}", e));
    println!("{:#?}", tree);
}
