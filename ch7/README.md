## Chapter 7: More Functional Patterns

### My Reading Notes

#### 7.1 Make it func-y

#### 7.2 Arguments and parameters

`let` expressions can shadow a function argument, because Haskell is "lexically scoped".
This means that resolving the value for a named entity depends on the location in the code
and the lexical content (like `let` and `where` clauses).

#### 7.3 Anonymouse functions

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

### Exercises

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

### Meetup topic seeds

1. In section 7.4 (p. 353-4 in my book), `Peng WherePenguinsLive` is said to be a "product type".
    What makes it a product type? I tend to think of tuples as product types.

