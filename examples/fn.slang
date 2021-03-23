fn f = 0
fn f =
    0

fn f x = x
fn x :f = x
fn (x) :f = x
fn
    f 
    x
=
    ()

fn f x y = x y
fn x :f y = x y
fn (x) :f (y) = x y

fn ~ x = x
fn ~ (x) = x
fn
    ~
    x
=
    x

fn x + y = x y
fn (x) + y = x y
fn x + (y) = x y
fn (x) + (y) = x y
fn
    x
    +
    y
=
    x y

fn x ! = x
fn (x) ! = x
fn
    x
    !
=
    x

fn f x = (fn x + y = x)

