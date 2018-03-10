## Chapter 5: Types

### My Reading Notes

Haskell typing is static, checked at compile time.

Numbers (e.g., 13) we'd enter at the REPL are typically resolved to a _constrained polymorphic_ type.
For example, `:type 13` is `Num a => a`, meaning some type that has evidence of being in the Num typeclass.

Multiple typeclass constraints are gathered up with tuple notation,
`(Num a, Num b) => a -> b -> b`

#### 5.4 Currying 

All Haskell functions are curried. They always take one argument.

Curried functions can be partially applied. If `f :: a -> b -> c` then
`f x`, where `x` is an `a`, is a partially applied `f`, and has type `b -> c`.

You can uncurry functions. e.g., `(+)`, normally `Num a => a -> a -> a`, becomes `Num a => (a, a) -> a`.

"Higher-order" functions are functions that return functions. In Haskell, that's
basically any function of multiple arguments.

"Sectioning" is partial application of an infix operator

`elem` (type `Eq a => a -> [a] -> Bool`) checks if a thing is in a list.

You can use `;` to separate a `let` declaration from its definition in ghci

    let addStuff :: Integer -> Integer -> Integer ; addStuff a b = a + b + 5

##### 5.5 Polymorphism

"Polymorphic" means "made of many forms"

Type signatures are generally either concrete, constrained polymorphic, or parametrically polymorphic.
ad-hoc polymorphism is another name for constrained polymorphic, and is accomplished with typeclasses
in Haskell. Parametric polymorphism is broader, means that type variables (parameters) are fully
polymorphic.

`id :: a -> a` is a parametrically polymorphic function. It can be applied to any type of data.
Since it's so broad, the only thing it can do (basically) is return the thing you pass in.

If a variable can be anything, there's little you can do with it. If the variable is constrained
by a typeclass, there's more you can do with it, coming from the typeclass. If the variable is
concrete, you know all the things that can be done with it.

"Parametricity" means the behavior of a function on the unconstrained arguments is uniform.
(this has relationships to functors and/or natural transformations, if you get into the
category side of things).

##### 5.6 Type inference

We don't always have to tell haskell the types of things, generally it can figure it out itself.
That said, putting the types in is a good form of documentation.

If we hadn't seen it previously, list syntax that comes up in the exercises: `[1..10]`

##### End-notes

The "monomorphism restriction" is that all top-level declarations by default will have a
concrete type if it can be determined. Can add `{-# LANGUAGE NoMonomorphismRestriction #-}`
to avoid this though.

"Principal type" is the most generic type which still typechecks.

### Exercises

#### Type Matching

1. c, `:t not` is `Bool -> Bool`
2. d, `:t length` is `[a] -> Int`
3. b, `:t concat` is `[[a]] -> [a]`
4. a, `:t head` is `[a] -> a`
5. e, `:t (<)` is `Ord a => a -> a -> Bool`

#### Type Arguments

1. a
2. d
3. d
4. c
5. a
6. e
7. d
8. a
9. c

##### Parametricity

1. Tried. "terminates successfully" means I can't set `let i :: a -> a ; i = undefined`
2. Basically the two options are the uncurried `fst` and `snd` functions
3. `let p2 :: a -> b -> b ; p2 a b = b`

##### Apply Yourself

1. `[a]` resolves to `[Char]`, so `[Char] -> [Char]`
2. We apply `(/)`, so `Num` specifies to `Fractional` for `a`, so `Fractional a => a -> a`
3. Again, `[a]` resolves to `[Char]`, so `Int -> [Char]`
4. `length` gives `Int`, so `a` becomes `Int`, final type is `Int -> Bool`
5. `a` resolves to `Char`, so `Char -> Bool`

##### Chapter Exercises

Multiple Choice

1. c
2. a
3. a? b?
4. c

[Determine the type](chExDtT.hs)

1.
    1. `Num a => a`
    2. `Num a => (a, [Char])`
    3. `(Integer, [Char])`
    4. `Bool`
    5. `Int`
    6. `Bool`
2. `Num a => a`
3. `Num a => a -> a`
4. `Fractional a => a`
5. `[Char]`

[Does it compile?](chExDiC.hs)

1. Not sure what the answer they're aiming for is, but this doesn't compile. You can remove
    the `$ 10` from either line and it will compile, but as written, it's sort of like you're
    trying to apply a funciton too many times, or treating an integer as a function.
2. Looks funny, but it's fine. Basically `x` is just an alias for the `print` function.
3. Definitely fails. `c = b 10` becomes `c = 5 10`, but `5` isn't a function. We could do
    `c = a 10`, and then it'd work, or change `c` to `a b 10` and `d` to `a c 200`.
4. We need to define `c`, but otherwise this is fine.

Type variable or specific type constructor?

1. Answered for us in the text
2. `zed` ([0]) is fully poly, others are concrete (capitalization)
3. `a` is fully poly, `b` is constrained, `c` is concrete
4. `f` and `g` are fully poly, `C` is concreate

[Write a type signature](chExWaTS.hs)

1. `[a] -> a`
2. `Ord a => a -> a -> Bool`
3. `(a, b) -> b`

[Given a type, write the function](chExGtWf.hs)

1. `i = id`
2. `c a b = a`
3. `c'' a b = a` is actually still a valid thing to write, both are the same
4. `c a b = b`
5. Many, many options, here's a few
    1. `r = id`
    2. `r as = as ++ as
    3. `r as = [head as]`
    4. `r = []`
6. `co bc ab a = bc $ ab a`
7. `a ac = id`
8. `a' ab a = ab a`

Fix it

1. [sing](chExFixIt1.hs)
2. Just change the comparator to `<`
3. [Arith3Broken](chExFixIt3.hs)

Type-Kwon-Do

1. [`h = g . f`](chExTKD1.hs)
2. [`e = w . q`](chExTKD2.hs)
3. [`xform (x, y) = (xz x, yz y)`](chExTKD3.hs), though there are a few other similar solutions
4. [`munge xy ywz x = fst $ ywz $ xy x`](chExTKD4.hs)

### Meetup topic seeds

1. Tooling? vim/emacs?
2. Ran across [profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#cost-centres)
3. stack?, e.g., [profiling](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md#debugging)
4. In currying & uncurrying existing functions (p. 201 in my edition), aren't curry and uncurry backwards,
    at least relative to curried/uncurried at the top of p.199? -- No!
5. Anybody read Wadler's paper on making Ad-hoc polymorphism less ad-hoc?
6. In polymorphic constants (p. 214) all the examples are numeric. Any non-numeric examples? -- Yes! `[]` but not `()`
7. Chapter Exercises, Multiple Choice, 3
8. Phil mentioned in slack the impossibility of `f :: (Num a, Num b) => a -> b`. Compare, though,
    `fromIntegral :: (Integral a, Num b) => a -> b`. Does Haskell have a notion of a `sealed` typeclass?
    Compare, also, `f4 :: a -> [b] ; f4 a = []`.

