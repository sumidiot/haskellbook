## Chapter 7: More Functional Patterns

### My Reading Notes

#### 7.1 Make it func-y

#### 7.2 Arguments and parameters

`let` expressions can shadow a function argument, because Haskell is "lexically scoped".
This means that resolving the value for a named entity depends on the location in the code
and the lexical content (like `let` and `where` clauses).

#### 7.3 Anonymous functions

`\x -> x * 3` is an anonymous function. To specify it's type signature, you need to wrap
it in parens to get the precedence - same for applying it: `(\x -> x * 3) 5`.

#### 7.4 Pattern matching

Order matters for pattern matching: they are attempted top to bottom. This tends to mean
you want to order your patterns from most to least specific.

`_` is a universal pattern that never fails to match, sort of a default, "anything else".

GHC will warn about patterns that overlap, or patterns that are "incomplete", meaning
they don't cover all the posibilities.

You can `:set -Wall` in ghci.

`newtype` is like `data` except it only permits one constructor and only one field.

#### 7.5 Case expressions

Case expressions can be used with any type that has "visible" data constructors.
A simple boolean example to show the shape/syntax is:

```
-- b some Bool
case b of
  True -> doTrue
  False -> doFalse
```

#### 7.6 Higher-order functions

Higher-order functions take functions as arguments.

`:t flip` is `(a -> b -> c) -> b -> a -> c`. The first argument is a function of two
arguments, and `flip` produces a new function of two arguments that is the same as
the input just with the arguments reversed.

#### 7.7 Guards

Guards provide another syntax for control-flow, similar to an if-then-else.
Guards are denoted with pipe, `|`, the condition, then an equals sign, and the result.
Note that `otherwise` is another name for `True`, but may help readability for guards.
Guards are evaluated sequentially, similar to case matches.

You can use a where declaration within a guard block, to use the same expression in
each guard, but only compute it (or at least declare it) once).

#### 7.8 Function composition

Function composition is denoted by the infix operator `.`. I tend to read it as
"following" because you do the function on the right first, when applying it.

#### 7.9 Pointfree style

Pointfree style means writing the function definition without writing the argument.

#### 7.10 Demonstrating composition

Recalling that the `Show` typeclass requires its members to have
`show :: Show a => a -> String`, we note that `print :: Show a => a -> IO ()` is
the composition `putStrLn . show`, where `putStrLn :: String -> IO ()`.

#### 7.11 Chapter Exercises

(in the exercises, below)

#### 7.12 Chapter Definitions

1. A parameter variable is **bound** to an argument variable. **binding** talks about
    what value a variable has.
2. An **anonymous function** is not bound to a name
3. **Currying** transforms function of multiple arguments to series of functions of
    one argument each, and is the default for functions in Haskell.
4. **Pattern matching** is a handy way to define functions based on patterns of the
    values passed as inputs.
5. **bottom** is non-value that denotes that the program cannot return a value. It is
    somewhat similar to using explicit `error` calls, though not entirely.
6. **higher-order** functions take functions as arguments.
7. **composition** means stacking two functions, to do one first and then do the second
    to the result of the first.
8. **pointfree** style doesn't write the argument by name.

### Exercises

Please note that my exercise solutions below, or in the linked source files, tend to
be pretty brief. However, [eolvwa](https://github.com/eolvwa) has kindly contributed
some much nicer exercise writeups. I've put them in their own [folder](niceSolutions).

#### 7.3 Anonymous functions

[Grab bag](s7_3GrabBag.hs)
1. all equivalent
2. d, because 3 is a polymorphic value
3.
    a. `f = \n -> n + 1`
    b. `addFix = \x -> \y -> (if x > y then y else x) + 5`
    c. `mflip f x y = f y x`

#### 7.4 Pattern matching

1.
    a. `:t k` is `(a, b) -> a`
    b. `:t k2` is `String`, while the type for `k1` and `k3` is `Num a => a`
    c. Both `k1` and `k2` will return the number 3
2. `f (a, b, c) (d, e, f) = ((a, d), (c, f))`

#### 7.5 Case expressions

1. `functionC x y = case x > y of True -> x; False -> y`
2. `ifEvenAdd2 n = case even n of True -> n+2; False -> n`
3. `nums x = case compare x 0 of LT -> -1; GT -> 1; EQ -> 0` (note that to test with a negative
    number you have to wrap the argument in parens, `nums (-5)`).

#### 7.6 Higher-order functions

1. 1
2. 11
3. 22
4. 21
5. 12
6. 11
7. 21
8. 21
9. 22
10. 31
11. 23

#### 7.7 Guards

1. F all the time if `otherwise` is first guard
2. It still typechecks, but doesn't work the same. Now you get the first score you're better than,
    rather than the best score you're better than.
3. b
4. `pal` takes `List a` for any `Eq a` (I forgot the `Eq` in my first answer)
5. `Eq a => List a -> Bool`
6. c
7. `Num a`
8. `(Num a, Ord a, Num b) => a -> b` (I missed the `Ord` constraint in my first answer)

#### Chapter Exercises

##### Multiple choice

1. d
2. b
3. d
4. b
5. a

##### Let's write code

1.
    a. It's not clear why you would, since you only use the `div` part of `divMod`
    b. Yes, using `divMod` is an implementation detail, doesn't affect the type signature
    c. Change the first 10 (the `div`) to 100

The rest of the exercises I actually [wrote](ChapterExercises.hs).

### Meetup topic seeds

1. In section 7.4 (p. 353-4 in my book), `Peng WherePenguinsLive` is said to be a "product type".
    What makes it a product type? I tend to think of tuples as product types.
2. I got the 7.7 Guards exercise #8 wrong, because it didn't occur to me that `Num` didn't imply `Ord`.
    All the "standard" `Num` classes also have `Ord`, but that's not the same thing.
3. [eolvwa](https://github.com/eolvwa) contributed his notes about
    [case over unit with guard](../topics/caseOverUnitWithGuardSytax.hs), from the first meetup,
    but it seems relevant here.
