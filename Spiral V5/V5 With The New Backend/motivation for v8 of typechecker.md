I've figured out everything. EVERYTHING.

The way it started is interesting. As usual I was spinning around in circles going up and down the Env, being dragged around be an endless chain of examples and counter examples when I decided to try a different tack.

In pseudocode, the way functions are desugared now is something like this:

`fun a b c -> expr` into `inl a -> inl b -> fun c -> expr`.

Now, both inlineables and functions have only one argument, but if you think about it, that does not really make sense. What does not make sense, is why give the function an argument at all. I could desugar it just as well into a function with no arguments.

`inl a b c -> fun -> expr`

This essentially proves that handling any arguments at all in the function is a complete waste. I should have just left it to Inlinleabes. I had to make so many special cases for just for explicit arguments in it. And it does not really make sense – right now I am passing term environments into the function and yet the function's argument is a type instead of a term.

There is absolutely no way I can memoize functions such as that.

There is an enormous amount of friction right now between different phases of the compiler. Free argument propagation stack overflows on recursive functions, and type inference for recursive functions is not even correct, not by a long shot.

It does not mean that what I have done is useless, it is just that I have made a lot of pieces which make sense on an individual basis, but not the way they've been brought together.

For example, I am absolutely sure of the way to do type inference, the friction right now is how that type inference clashes with the need to track variable names in the type. That kills me.

Moving on, my breakout came a few hours ago when I started thinking about variable pools.

Think about it, why does the type checker diverge?

Imagine having a function that takes an Env and produces a list of TyVs from that. That list is the pool. What happens during divergence is that the variable pool grows without bound as on each step new variables are added.

There is no doubt about it, there are cases where that can't be avoided, but if a program is non-diverging during typechecking phase, that is, its variable pool is constant or shrinking, what can I do to take advantage of that?

Forget all about the language that existed before and imagine something akin to lambda calculus. Except that idealized language will be closer to what Spiral is now – it will have to kinds of function the standard Inlinleabes and Methods. Inlinleabes take in one argument and Methods take none. So rather than call them Method, it would be better to call them MemoizedExpressions, and promote Inlineables into Functions.

What is going to happen in such a language when the MemoizedExpression's case is called is this:

```
// This is intended to be pseudocode.
let call_as_method expr env =
    env_used_variable_elimination env expr 
    |> pool_make_from_env
    |> pool_duplicate_variable_elimination 
    // The variable pools are necessary for the renaming phase.
    // What gave me so much trouble in the previous version is the tracking of variables.
    // If it was only types that needed to be accounted for it would be quite doable.
    // The solution is renaming.
    // After the two renames the resulting env and pool the typechecker that compares terms
    // directly would be equivalent to the one that only compared just types.
    |> env_rename
    |> pool_rename
    // Apart from memoizing, the memoize function also needs to look for the
    // shortest match in its list based on the env passed to it. Or that would
    // be the case if expr_used_variable_elimination was not called at the start.
    |> memoize (eval expr)
	// I do not want to allow closures or local arrays being returned from 
	// non-Inlineable expressions.
	|> typed_expr_guard_type
    // pool_typedexpr_used_variable_elimination is essentially what closure_conversion 
    // was in the previous version.
    // It also performs the role of tuple deforestation. I made the mistake in
    // the previous version in the order I was running closure conversion. Instead of
    // passing in nothing I should have passed in every variable in the env and later
    // eliminated them instead.
    // Interesting note, this actually performs the role of not just tuple deforestation,
    // but it also removes unused curried arguments in a function.
    |> pool_typedexpr_used_variable_elimination 
    |> env_rename
    |> pool_rename
    // This second memoize is because it is using the variable pool for the argument list.
    // The typedexpr won't change, but the unused arguments won't be needed to be passed along.
    |> memoize
```

All the numerious optimizations that have given me so much trouble will come together and work in tandem with typechecking to produce absolutely perfect code. Yesterday, I made a hard decision to not put in type level closures, but now I feel like they would be easy to add. Not trivial, since I would have to make a specific pass to optimize their arguments, but quite doable whereas yesterday I thought they would be impossible.

In addition I will remove the complex type level arrays that I have now and replace them with named tuples and let those unused variable optimizations do the trick of eliminating duplicate size arguments. It already occured to me to do that previous, but I was too busy trying to figure out...well, this.

The benefit of doing it like the above is not just the incredible consiseness or the efficiencies it would unlock. It is also a matter of the mind, the saying is how well you can compress something is how well you can understand it. The above represents a change in mindset for me, no longer am I going up and down the chain trying to make the pieces fit, but instead thinking about Methods in terms of optimizations done on expressions.

This is absolutely remarkable.

My plan from here will be to redo the typechecker for the start.

Yeah, it is 1k lines long right now, but how complex something is is never about absolute size. If size was all that mattered, this typechecker would have been done in half a week. It will take me a long time, maybe until the end of the month, but it will be worth it.

By the time it comes to update the Simulacrum blog again not only will I be finished with the Spiral library, but the Language as well.