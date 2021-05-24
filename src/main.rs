extern crate nom;
extern crate nom_supreme;

mod parser;

fn main() {
    let input = dbg!(include_str!("test.txt"));
    match parser::expression(input) {
        Ok(t) => println!("{:#?}", t),
        Err(e) => eprintln!("{}", e),
    }
}
