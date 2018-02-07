## Chapter 4: Basic datatypes

### My Reading Notes

The type constructor is name of type, and is capitalized.
Data constrctuors are the values that inhabit the type.
In `data Bool = False | True`, `Bool` is the type constructor
("we're making a type") and `False` and `True` are data constructors.
The pipe, `|`, can be read as "or". The whole thing is a
data declaration.

`:info` gives you the datatype definitions for build-in types.

When you define a function, you can use pattern matching:

    not :: Bool -> Bool
    not True = False
    not False = True

If you do, `_` serves as a catch-all.

Numeric types:
* `Int`: fixed precision integers
* `Integer`: arbitrarily large integers
* `Float`: single-precision floating point number
* `Double`: double-precision floating point
* `Rational`: ratio of two integers. arbitrarily precise, but less efficient than...
* `Scientific`: scientific notation, Integer coefficient with Int exponent.
    space efficient and "almost arbitrary precision".
    Available in the `scientific` library

All numeric types have typeclass instances for `Num`. This typeclass requires,
for example, standard numeric operations (`(+)`, `(-)`, `(*)`).
More on typeclasses soon.

Use `Integer` unless you know you're content with `Int` and want the performance.
There are `Int8` and `Int16` types also (if you `import GHC.Int`). You can get
the bounds of a `Bounded` thing (i.e., a thing with an instance of the `Bounded`
typeclass) with `minBound` and `maxBound`. These have somewhat entertaining types:
`:t minBound` is `:: Bounded a => a`. At a glance that looks like a function, but
it's not. `minBound :: Int8` helps the compiler figure out `a`, and then (presumably)
the evidence of being `Bounded` provides the way to get `minBound` (guessing that
`Bounded` basically means you have a `minBound` and `maxBound` function.

`:info` gives you typeclass instances for a type

Arguments before `=>` (when it appears), in the type signature of a function,
are _typeclass constraints_. For example, `:t (/)` is `(/) :: Fractional a => a -> a -> a`,
and `Fractional a` is the contraint. It limits the types of things `a` can be, to
those with an instance of the typeclass (in this case, `Fractional`).

Typeclasses come in hierarchies, in the sense that, for example, if you are `Fractional`
you must be `Num`.

`==` allows for equality checking. **Interestingly**, `/=` is for not equal to
(where most languages use `!=`).

* `Eq` is a typeclass for things that can be compared with `==` and `/=`.
* `Ord` is a typeclass for things that can be compared with thing like `<` and `>`

If `a` is `Ord`, then `[a]` is `Ord`, so you can compare `String`s, because `Char` is `Ord`

`(&&)` and `(||)` are the standard boolean functions for and and or.

`if` is an expression (returns a thing), not a statement. `if () then () else ()`.
Both things in the "then" and "else" sections have to have the same type. Note that
while some languages allow for numeric expressions in the condition of the `if`,
for example with the rule "non-zero is true", this is not valid in Haskell.

_Tuple_ types use syntax like `(,)`. "arity" is the number of things in the tuple.
`fst` and `snd` return the first and second elements of a two-tuple, respectively.
If you `import Data.Tuple`, you get the `swap` function.

Here's how to implement your own `fst`, showing both the pattern matching syntax
that's handy for defining functions, and the use of `'` in function names:

    fst' :: (a, b) -> a
    fst' (a, b) = a

Recommended to avoid large tuples, typically see 5-tuples or shorter.

Lists are like tuples except
1. In a list, all elements must be the same type
2. Lists use `[]` syntax, while tuples use `()`
3. The size of a list isn't part of the type

* `length` returns the length of a list
* `reverse` reverses a list
* `toInteger` converts other numeric types to `Integer`

You can define an anonymous function with something like `(\x -> x)`

`show` is similar to Java `toString`, applys to anything with instance of `Show`
typeclass.

In haskell, you can only have one instance of a typeclass for a given type.

### Exercises

#### Mood Swing

1. `Mood`
2. `Blah` or `Woot`
3. `Woot` is a data value, not a type
4. See [changeMood.hs](changeMood.hs), also for 5.

#### Find the Mistakes

1. `not True && True` (if we need to get `True` as the answer, change `&&` to `||`)
2. `not (x == 6)`
3. compiles. To get `True`, change `>` to `<`
4. ["Merry"] > ["Happy"]
5. ["1", "2", "3"] ++ "look at me!"

#### Chapter Exercises

1. `:t length` is `[a] -> Int`
2.
    1. 5
    2. 3
    3. 2
    4. 1
3. 6 / 3 is ok. The other is not because `length` returns `Int`, and there is no
    `(/)` method with second argument an `Int`. That is, `6 / (3::Int)` also fails
    (as does `(6 :: Int) / 3`).
4. Convert `(/)` to `div`
5. Bool, True
6. Bool, False
7.
    1. Works, True
    2. Fails, can't mix ints and chars in list
    3. Works, 5
    4. Works, False
    5. Fails, can't `&&` an int
8. `isPalindrome x = x == reverse x`
9. [abs.hs](abs.hs)
10. [chExTuple10.hs](chExTuple10.hs). This is entertaining because you change `::`
    and first `->` to spaces, and the second `->` to an `=`, and you get the definition.

##### Correcting syntax

1. `:t x` is `Integer` addition (why?). `length` return `Int`, and so applying `x` fails.
    You can apparently convert `toInteger`, so can put that on the `w`. That is,
    `f xs = toInteger w 'x' 1 where w = length xs`
2. (Did they tell us how to make anonymous functions?) Looks like you do `(\x -> x)`.
3. `f (a b) = a`

##### Match the function names to their types

1. c
2. b
3. a
4. d

### Meetup topic seeds

1. Shouldn't `:t length` be `[a] -> Integer`?
2. Multi-line input in ghci: Begin and end the block with `:{` and `:}` on lines by
    themselves. To define a function with a type signature, use `let` on the
    signature line, and just indent the definition.
3. "Correcting syntax" exercises were interesting. In (1), why is `:t x` for `Integers`
