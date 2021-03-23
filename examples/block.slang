{}

{
}

{
    print x
    print y
    print z
}

{
    when x -> x
    when x y -> x y
}

{
    when
        x
    ->
        x
    when
        x
        y
    ->
        x y
}

operator infix left ==

let test = {
    when x | x == 0 -> 1
    when x y | x == y -> 2
}

let test = {
    when
        x
    |
        x == 0
    ->
        1
    when
        x
        y
    |
        x == y
    ->
        2
}