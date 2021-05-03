fn :fib 0 = 0
fn :fib 1 = 1
fn :fib (n::Num) = (
    let a = 0
    let b = 1
    while { n > 1 } {
        let tmp = b
        b = a + b
        a = tmp
        n = n - 1
    }
    b
)