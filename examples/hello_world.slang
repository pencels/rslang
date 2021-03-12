let main = { -- comment
    print "hello world!" -- comment
} -- comment

fn ([]) :len = 0
fn (Cons _ t) :len = 1 + t :len

main