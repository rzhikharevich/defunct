extern crate num;
extern crate unicode_segmentation;

#[macro_use] mod utils;

mod tok;
mod parser;
mod exec;

fn main() {
    let maybe_fns = parser::parse_str(
        "f a =\n    let b = 42\n    g x =\n        return 2*x\n    return a*b\n"
    );

    match maybe_fns {
        Ok(fns) => {
            for (id, f) in fns.iter() {
                println!("{} {:?}", id, f);
            }
        }
        
        Err(errors) => {
            for (loc, err) in errors {
                println!("{}:{}: {}", loc.line, loc.col, err);
            }
        }
    }
}
