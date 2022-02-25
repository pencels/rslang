operator infix left + -
operator infix left * / % `mod` above +
operator infix nonassoc < > <= >= below +
operator infix left == != below <

operator prefix -

operator prefix !
operator postfix !

fn :unwrap {x} = x
fn :unravel {!x} = x
let unwrap = :unwrap
let unravel = :unravel

fn :panic msg = :__panic__ msg
let panic = :panic

fn :print x = :__print__ x
let print = :print

struct True <: Bool
struct False <: Bool

fn :if True x = x
fn :if False _ = nothing
fn :if {!cond} x = :if cond x

fn :if True x _ = x
fn :if False _ x = x
fn :if {!cond} x y = :if cond x y

let if = :if

fn :while cond body = (
    let loop = {
        :if cond { body; loop }
    }
    loop
)
let while = :while

-- Subtyping
operator infix nonassoc <:
fn (x::Atom) <: (y::Atom) = :__is_subtype_of__ x y
fn x :is_instance_of (y::Atom) = :__is_instance_of__ x y

-- Option
struct None <: Option
struct Some value <: Option

fn None :get = panic "Tried to get value but got None"
fn (Some v) :get = v

fn None :get_or v = v
fn (Some v) :get_or _ = v

fn None :map _ = None
fn (Some v) :map f = Some (f v)

-- Range
struct Range start end step
fn (start::Num) :to (end::Num) = start :to end :by 1
fn start :to end :by step = Range start end step

struct RangeIter curr end step <: Iter
fn (Range start end step) :iter = RangeIter start (end + step) step
fn (RangeIter curr end step) :next = :unravel (
    if (curr == end) {
        [RangeIter curr end step, None]
    } {
        [RangeIter (curr + step) end step, Some curr]
    }
)

-- Lists
fn (xs::List) :len = :__list_len__ xs
fn (xs::List) :push item = :__list_push__ xs item
fn (xs::List) :with [i::Num] := item = :__list_where__ xs i item
fn (xs::List) :index_of item = (
    let n = xs :len
    let i = 0
    let idx = None
    while { i < n } {
        if (xs i == item) {
            idx = Some i
            i = n
        }
        i = i + 1
    }
    idx
)

struct ListIter list idx len <: Iter
fn (x::List) :iter = ListIter x 0 (x :len)
fn (ListIter x i n) :next = :unravel (
    if (i >= n) {
        [ListIter x i n, None]
    } {
        [ListIter x (i + 1) n, Some (x i)]
    }
)

fn (it::Iter) :collect_as List = (
    let list = []
    let loop = {
        let [it_next, curr] = it :next
        if (curr != None) {
            let Some v = curr
            list = list :push v
            it = it_next
            loop
        }
    }
    loop
    list
)

-- Iter adapters
fn (it::Iter) :for_each f = (
    let loop = {
        let [it_next, curr] = it :next
        if (curr != None) {
            let Some v = curr
            f v
            it = it_next
            loop
        }
    }
    loop
)

struct MapIter (f::Atom) (inner::Iter) <: Iter
fn (it::Iter) :map (f::Atom) = MapIter f it
fn (MapIter f it) :next = (
    let [it_next, curr] = it :next
    [MapIter f it_next, curr :map f]
)

fn (it::Iter) :inspect f = it :map (\x -> (f x; x))

struct FilterIter f inner <: Iter
fn (it::Iter) :filter f = FilterIter f it
fn (FilterIter f it) :next = :unravel (
    let [it_next, curr] = it :next
    if (curr == None) {
        [FilterIter f it_next, None]
    } {
        let Some v = curr
        if (f v) {
            [FilterIter f it_next, Some v]
        } {
            (FilterIter f it_next) :next
        }
    }
)

struct FilterMapIter f inner <: Iter
fn (it::Iter) :filter_map f = FilterMapIter f it
fn (FilterMapIter f it) :next = :unravel (
    let [it_next, curr] = it :next
    let result = curr :map f
    if (result == None) {
        [FilterMapIter f it_next, None]
    } {
        let Some v = result
        if (v == None) {
            (FilterMapIter f it_next) :next
        } {
            [FilterMapIter f it_next, v]
        }
    }
)

fn (it::Iter) :reduce f = (
    let [it, curr] = it :next
    let acc = curr
    let loop = {
        let [it_next, curr] = it :next
        if (curr != None) {
            let Some v = curr
            acc = acc :map (\acc -> f acc v)
            it = it_next
            loop
        }
    }
    loop
    acc
)

-- Basic boolean logic and arithmetic
fn - (x::Num) = :__neg__ x
fn (x::Num) + (y::Num) = :__add__ x y
fn (x::Num) - (y::Num) = :__sub__ x y
fn (x::Num) < (y::Num) = :__lt__ x y
fn (x::Num) > (y::Num) = :__lt__ y x

fn :not True = False
fn :not False = True
fn ! True = False
fn ! False = True
let not = :not

fn x == y = :__eq__ x y
fn x != y = :not (x == y)
fn x >= y = :not (x < y)
fn x <= y = :not (x > y)

-- Conversions (_ :as _)
fn nothing :as Str = "nothing"
fn (x::Num) :as Str = :__intrinsic_as_str__ x
fn (x::Atom) :as Str = :__intrinsic_as_str__ x
fn (x::Str) :as Str = x
fn (x::List) :as Str = :__intrinsic_as_str__ x
fn (x::Lazy) :as Str = "<Lazy>"

fn x + (s::Str) = "\(x)\(s)"
fn (s::Str) + x = "\(s)\(x)"
fn (a::Str) + (b::Str) = "\(a)\(b)"