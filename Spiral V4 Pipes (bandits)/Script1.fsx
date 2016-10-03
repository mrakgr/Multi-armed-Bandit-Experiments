type A<'a> =
    {
    mutable x: 'a
    }

let a = {x = 1}
{a with x="Hello"} // Type error