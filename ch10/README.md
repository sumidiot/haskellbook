## Chapter 10: Folding lists

### 10.1 Folds

Folds extend to many other types beyond lists, and the general case is called a **catamorphism**.
This comes from "cata-" meaning "down", they are a means of deconstructing data.

### 10.2 Bringing you into the fold

`foldr` is short for "fold right". It has type `(a -> b -> b) -> b -> [a] -> b`
(or, in newer GHC, a type constraint for `Foldable t`, and then `t a` as the third argument).
Think of the second argument, the `b`, as a seed for an accumulator, and the first argument as a way to
take a thing from the list and combine it with the accumulator to get a new value.

You can actually implement `map` as a `foldr`:

    map f as = foldr mc [] as
        where mc a b = (f a) : b

### 10.3 Recursive patterns

Each of `sum`, `length`, `product`, and `concat` are relatively easy to write recursively,
and when you do so they all have the same pattern: evaluate the head of a list, combine it with
some result of repeating your way on down the list.

### 10.4 Fold right

`foldr` is "right fold" because it associates to the right. This means it processes the tail
of the list first, the combines with the head. Perhaps more correctly, the recursion is
of the form `foldr f b a:as = f b (foldr f b as)`. If `f` doesn't use the second argument in
some case, `foldr` doesn't evaluate on down the list. We'll see an example with `const`
(of type `a -> b -> a`) soon.
