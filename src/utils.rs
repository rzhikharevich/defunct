use std::fmt;

macro_rules! opt {
    ($x:expr) => (
        match $x {
            Some(y) => y,
            None => return None,
        }
    )
}

macro_rules! opt_err {
    ($x:expr) => (
        match $x {
            Some(y) => y,
            None => return Err(()),
        }
    )
}

pub fn write_with_commas_and_or<T: fmt::Display>(
    f: &mut fmt::Formatter,
    slice: &[T],
)
    -> fmt::Result
{
    if slice.len() == 0 {
        Result::Ok(())
    } else {
        write!(f, "expected {}", slice[0])?;

        if slice.len() > 1 {
            for item in &slice[1..slice.len()-1] {
                write!(f, ", {}", item)?;
            }

            write!(f, " or {}", slice.last().unwrap())
        } else {
            Result::Ok(())
        }
    }
}
