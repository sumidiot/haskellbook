## Chapter 10: Folding lists

### My Reading Notes

#### 10.1 Folds

Folds extend to many other types beyond lists, and the general case is called a **catamorphism**.
This comes from "cata-" meaning "down", they are a means of deconstructing data.

#### 10.2 Bringing you into the fold

`foldr` is short for "fold right". It has type `(a -> b -> b) -> b -> [a] -> b`
(or, in newer GHC, a type constraint for `Foldable t`, and then `t a` as the third argument).
Think of the second argument, the `b`, as a seed for an accumulator, and the first argument as a way to
take a thing from the list and combine it with the accumulator to get a new value.

You can actually implement `map` as a `foldr`:

    map f as = foldr mc [] as
        where mc a b = (f a) : b

#### 10.3 Recursive patterns

Each of `sum`, `length`, `product`, and `concat` are relatively easy to write recursively,
and when you do so they all have the same pattern: evaluate the head of a list, combine it with
some result of repeating your way on down the list.

#### 10.4 Fold right

`foldr` is "right fold" because it associates to the right. This means it processes the tail
of the list first, the combines with the head. Perhaps more correctly, the recursion is
of the form `foldr f b a:as = f b (foldr f b as)`. If `f` doesn't use the second argument in
some case, `foldr` doesn't evaluate on down the list. We'll see an example with `const`
(of type `a -> b -> a`) soon.

You have to think a little about what `foldr` forces, it depends on the data and the function.
It does always require the `[a]` argument to not be undefined.

#### 10.5 Fold left

`foldl` is very similar to `foldr`, it just associates to the left. It has type
`(b -> a -> b) -> b -> [a] -> b`.

A **scan** function is similar to the folds (`foldr` and `foldl`), but retuns a list of the
intermediate values at each stage of the fold. Note that `foldr = head . scanr` while
`foldl = last . scanl`.

Thinking about what order things are done in takes some care, but is required.

With `foldl` recursion of the spine is unconditional, you have to go into it, because
the definition is `foldl f a (x:xs) = foldl f (f a x) xs`, vs for `foldr`
`foldr f z (x:xs) = f x (foldr f z xs)`. The difference is that `foldr` replaces itself
with a call to the `f`, while `foldl` replaces itself with a call to `foldl` again. It can't
stop that until it gets to the empty list as the final argument. Thus, **`foldl` is generally
inappropriate for infinite lists**, or even just long lists. There's a `foldl'` that is
strict, forcing evaluation of the values in the cons cells as it traverses, so it is less of
a performance hit for long lists. (`foldl'` seems to not be in my Prelude).

##### Exercises

1. `foldr (*) [1..5]` returns the same as `foldl (flip (*)) 1 [1..5]` and `foldl (*) 1 [1..5]`
2. `foldl (flip (*)) 1 [1..3]` is evaluated as:

    ```
    foldl (flip (*)) 1 [1..3]
      = foldl (flip (*)) ((flip (*)) 1 1) [2,3]
      = foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
      = foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
      = ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
      = ((flip (*)) ((flip (*)) ((*) 1 1) 2) 3)
      = ((flip (*)) ((flip (*)) 1 2) 3)
      = ((flip (*)) ((*) 2 1) 3)
      = ((flip (*)) 2 3)
      = (*) 3 2
      = 6
    ```

3. `foldr` is different than `foldl` because it associates to the right
4. Folds are catamorphisms, which means they are used to reduce structure
5.
    1. `foldr (++) "" ["woot", "WOOT", "woot"]`
    2. `foldr max '' "fear is the little death"`
    3. `foldr (&&) True [False, True]`
    4. Not sure what the problem is, or what the question is asking
    5. `foldl ((++) . show) "" [1..5]`
    6. `foldr const 0 [1..5]`
    7. `foldr const '0' "tacos"`
    8. `foldl (flip const) '0' "burritos"`
    9. `foldl (flip const) 0 [1..5]`

#### 10.6 How to write fold functions

Generally start by thinking about the start value, then the function.

Somewhat amusingly, `"" == []` is `True`, even though the types are different.

##### [Exercises](s10_5.hs)

The exercises introduce the `Data.Time` module, with `UTCTime`, which has type
`Day -> DiffTime -> UTCTime`.

#### 10.7 Folding and evaluation

`foldr f z xs = foldl (flip f) z (reverse xs)` for finite `xs`

#### 10.8 Summary

`foldl` directly self-calls, so eats up the whole list. `foldr` ends up alternating
between `foldr` and the `f` calls, only eating up the bits of the list that `f` keeps
asking for.

`foldr` works for infinite lists, and is a good default choice.

`foldl` is nearly useless, and you should almost always use `foldl'` instead.

#### 10.9 Scans

The only difference in the type signature between `foldX` and `scanX` (where X=l or X=r)
is the final output is a list in the case of `scanX`.

`fibs = 1 : scanl (+) 1 fibs` defines a list recursively, entertainingly. You can then get
the N-th value as `fibsN x = fibs !! x` (or, point-free: `fibsN = (fibs !!)`).

##### Exercises

1. `fibsFirst20 = take 20 fibs`
2. `fibsLessThan100 = filter (<100) $ take 20 fibs`. Without the `take` Haskell can't tell that this
    list isn't infinite.
3. `factorial = scanl (*) 1 [1..]` (with this, `factorial !! 0` represents "0!" accurately)

#### 10.10 [Chapter Exercises](chEx.hs)

#### 10.11 Definitions

A **fold** is a higher function that accumulates results to build up the final value.

A **catamorphism** generalizes folds from lists to arbitrary datatypes. There's definitely a pattern
in the following catamorphisms associated with each data type, but I can't quite see it right now:

1. `bool   :: a        -> a        -> Bool       -> a`
2. `maybe  :: b        -> (a -> b) -> Maybe a    -> b`
3. `either :: (a -> c) -> (b -> c) -> Either a b -> c`

All the data declarations are 2-case. The first argument to the catamorphism is "maps from the left
case to a new type", the second is "maps from the right case to a new type", the third is "the type",
and the fourth is "the new type". Does this play out with list, `foldr`? Seems wonky.

    data List a = [] | (a : List a)
    foldr  :: (a -> b -> b) -> b -> List a -> b

A **tail call** is is the final result of a function

**tail recursion** is when the tail call is a recursive self invocation.

### Meetup topic seeds

1. I found the `foldr` "here's how the expression is re-written" and, in particular "and evaluated"
    with the sum example (`foldr (+) 0 [1..5]`) a little mis-leading. The re-writing I'm fine with.
    The evaluation "we evaluate the inner parens first" is more deceptive/subtle. Consider the `const`
    example: `foldr const 0 [1..5]`, which returns 1. The rewrite tree is the same, just putting
    const inline roughly, and const's evaluation only looks at the left bit of the re-write tree,
    even though the parenthesized version would still look the same. Maybe I'm just confused about it
    all. I put some examples in the [section 4 source](s10_4.hs). Maybe it's the distinction between
    traversing and folding?
2. What's going on with the question in 10.5, exercise 5.d (my p. 558)?
3. Use equational reasoning to show
    1. `foldr = head . scanr`
    2. `foldr const _ = head`
    3. `foldr f z xs = foldl (flip f) z (reverse xs)` (for finite `xs`)
4. The exercises in section 10.6 are fun, let's compare answers.
5. Anybody read the [scans blog post](https://chrisdone.com/posts/twitter-problem-loeb)? Seems fun.
6. [Beautiful folds in scala](https://softwaremill.com/beautiful-folds-in-scala/) was a fun read also
7. Anybody have more interesting answers to the scan exercises, section 10.9
8. The final reference sounds fun, [A tutorial on the universality and expressiveness of fold](http://www.cs.nott.ac.uk/~gmh/fold.pdf).
9. Any idea what they're hinting at with the patterns of catamorphisms (my p.587) in 10.11?
