operator infix left + -
operator infix left * / % `mod` above +
operator infix left == != above *
operator infix nonassoc < > <= >= above ==

operator prefix -

operator prefix !
operator postfix !

fn :unwrap {x} = x
fn :unravel {!x} = x

fn :panic msg = :__panic__ msg
let panic = :panic

fn :print x = :__print__ x
let print = :print

fn :if :true x = x
fn :if :false _ = nothing
fn :if :true x _ = x
fn :if :false _ x = x
let if = :if

fn :while cond body = (
  let loop = {
    let {!cond} = cond
    :if cond { body; loop }
  }
  loop
)
let while = :while

fn (start::Num) :to (end::Num) = start :to end :by 1
fn (start::Num) :to (end::Num) :by (step::Num) = (
    let ret = []
    while { start != end } {
        ret = ret :push start
        start = start + step
    }
    ret
)

fn (xs::List) :len = :__list_len__ xs
fn (xs::List) :push item = :__list_push__ xs item
fn (xs::List) :with [i::Num] := item = :__list_where__ xs i item
fn (xs::List) :each f = (
    let n = xs :len
    let i = 0
    while { i < n } {
        f (xs i)
        i = i + 1
    }
)
fn (xs::List) :map f = (
    let n = xs :len
    let i = 0
    let ret = []
    while { i < n } {
        ret = ret :push (f (xs i))
        i = i + 1
    }
    ret
)

fn :invoke (args::List) = :invoke args :with nothing
fn :invoke (args::List) :with v = (
    let n = args :len
    let i = 0
    let hole_i = args :index_of :_
    let new_args = []
    while { i < n } {
        new_args = new_args :push (:invoke (args i) :with v)
        i = i + 1
    }
    if (hole_i == nothing) {
        ...new_args
    } {
        ...(new_args :with [hole_i] := v)
    }
)
fn :invoke :_ :with v = v
fn :invoke x :with _ = x


fn (xs::List) :index_of item = (
    let n = xs :len
    let i = 0
    let idx = nothing
    while { i < n } {
        if ((xs i) == item) {
            idx = i
            i = n
        }
        i = i + 1
    }
    idx
)

fn - (x::Num) = :__neg__ x
fn (x::Num) + (y::Num) = :__add__ x y
fn (x::Num) - (y::Num) = :__sub__ x y
fn (x::Num) < (y::Num) = :__lt__ x y
fn (x::Num) > (y::Num) = :__lt__ y x

fn :not :true = :false
fn :not :false = :true
fn ! :true = :false
fn ! :false = :true
let not = :not

fn x == y = :__eq__ x y
fn x != y = :not (x == y)
fn x >= y = :not (x < y)
fn x <= y = :not (x > y)


-- Conversions (_ :as _)

fn nothing :as :Str = "nothing"
fn (x::Num) :as :Str = :__intrinsic_as_str__ x
fn (x::Atom) :as :Str = :__intrinsic_as_str__ x
fn (x::Str) :as :Str = x
fn (x::List) :as :Str = :__intrinsic_as_str__ x
fn (x::Lazy) :as :Str = "<Lazy>"