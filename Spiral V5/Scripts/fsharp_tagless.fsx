// An example of how to use the finally tagless style with F#.
// For more info: http://okmij.org/ftp/tagless-final/

type RegularInterpreter = RegularInterpreter with
    static member Val(_: RegularInterpreter, x: int) = x
    static member Add(_: RegularInterpreter, x: int, y: int) = x+y
    static member Mult(_: RegularInterpreter, x: int, y: int) = x*y

type StringInterpreter = StringInterpreter with
    static member Val(_: StringInterpreter, x: string) = x
    static member Val(_: StringInterpreter, x: int) = string x
    static member Add(_: StringInterpreter, x: string, y: string) = sprintf "(%s + %s)" x y
    static member Mult(_: StringInterpreter, x: string, y: string) = sprintf "(%s * %s)" x y

// The `or` keywords instructs the constraint solver to keep looking.
let inline val_ v interpreter =
    ((^x or ^b or ^c): (static member Val: ^x * ^b -> ^c) interpreter, v)
let inline add x y interpreter =
    ((^x or ^b or ^c): (static member Add: ^x * ^b * ^b -> ^c) interpreter, x interpreter, y interpreter)
let inline mult x y interpreter =
    ((^x or ^b or ^c): (static member Mult: ^x * ^b * ^b -> ^c) interpreter, x interpreter, y interpreter)

let inline r1 in_ = val_ 5 in_
let inline r2 in_ = add (val_ 1) (val_ 3) in_

let int_r1 = r1 RegularInterpreter // Returns an int.
let string_r1 = r1 StringInterpreter // Returns a string.
let int_r2 = r2 RegularInterpreter
let string_r2 = r2 StringInterpreter

let inline r3 in_ = mult (add (val_ 1) (val_ 3)) (val_ 10) in_

let int_r3 = r3 RegularInterpreter
let string_r3 = r3 StringInterpreter

let inline r4 in_ = mult (val_ 3) (val_ "3") in_

//let int_r4 = r4 RegularInterpreter // As expected this gives a type error.
let string_r4 = r4 StringInterpreter

