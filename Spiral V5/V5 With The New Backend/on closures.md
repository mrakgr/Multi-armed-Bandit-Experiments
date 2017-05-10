In short, they are impossible.

The explanation goes as follows.

The first step is to stop thinking of various hacks to get the individual closures working somehow and focus on the principles instead. Since Spiral has pattern matching on named tuples at compile time, imagine the following fragment.

```
fun foo () ->
    inl x = 2
    inl y = 3.3
    inl z = false
    fun c ->
        typecase c with
        | GetX -> x
        | GetY -> y
        | GetZ -> z
```

To make the above work, what the foo function would have to do is essentially return its whole scope, and much like with higher order function now, optimize away the unused arguments at a later point. This would require memoization tricks and would be hard to get right, maybe it would require converting the whole expression tree to CPS in order to properly propagate the arguments. But that is not why it is impossible.

```
fun bar cond ->
    if cond then
        inl x = 2
        inl y = 3.3
        fun w ->
            match w with
            | GetX -> x
            | GetY -> y
    else
        inl z = false
        fun e ->
            typecase c with
            | GetZ -> z
```

What exactly is the language supposed to return here? What would be the type of the function? What the type level closures would do is essentially export the whole scope to make it work, but here there are two different scopes. This example shows that type level functions, while being much more expressive than term level ones are fundamentally different tools. Before thinking of this example, I thought there might be ways of making type level closures, but now I think that function returns might be best thought of as type bottlenecks, much like If expressions.

They are not 100% impossible - multiple type returns from If expressions can be expressed with Sum types - but there is value in type bottlenecks as well. Efficiency for one, the way to do type level closures would be to convert everything to CPS and that would lead to enormous code bloat. Given that it would be acceptable to just write in CPS explicitly when such a tool is needed.