## Chapter 9: Lists

### My Reading Notes

#### 9.1 Lists

Lists can be either finite or infinite.

#### 9.2 The list datatype

List is `data [] a = [] | a : [a]`. `[]` is the type constructor, as well as the data
constructor for the empty list. The infix operator `:` is typically called __cons__,
short for __construct__. It is a __product__ data constructor, because it takes two
arguments (we'll learn more about sums and products later).

#### 9.3 Pattern matching on lists

You can mattern match on the head and tail of a list, as you would for other data
constructors: `let (h : t) = myList` pulls off the head element as `h` and the
tail list as `t`. You do have to be careful to match the empty list.

The `Maybe` type is useful for functions that __might__ return a result. It is
`data Maybe a = Nothing | Just a`. We __should__ think of `head : [a] -> a` as really
being `safeHead: [a] -> Maybe a`, because `head []` doesn't return anything sensible
(using `Maybe`, it would return `Nothing`). Later we'll learn more about `Maybe`,
as well as `NonEmpty`, which is a container with at least one thing in it.

#### 9.4 List's syntactic sugar

Instead of carefully "cons-ing" up a list, `1 : 2 : 3 : 4 : []`, Haskell lets you
just write `[1,2,3,4]`. We may talk about a "cons cell", being a space that values
may inhabit in the list, and the "spine" which is the thing that holds cons cells
together.

#### 9.5 Using ranges to construct lists

You can easily construct a list with a range syntax: `[1..10]`. Note that if you
specify the first two elements, it creates the range by stepping by the difference
each time. So `[1,3..10]` is the odd integers less than or equal to 10. You can
also make ranges of characters, `['a'..'z']`. You can have negative steps, but
if you ask for a negative step and the end of your range is greater than the beinning,
you'll get an empty list: `[1,-1,10]=[]`.

This syntax is supported by the functions

* `enumFrom`,
* `enumFromThen`,
* `enumFromTo`, and
* `enumFromThenTo`,

all of which require an `Enum`. For example, `enumFromThenTo 1 3 10` is the same
as `[1,3..10]`. Note that the first two methods, `enumFrom` and `enumFromThen`
produce infinite lists (assuming the underlying `Enum` type is infinite).

##### [Excercises](s9_5EnumFromTo.hs).

#### 9.6 Extracting portions of lists

* `take :: Int -> [a] -> [a]`. `take n l` is the first `n` things of `l`
* `drop :: Int -> [a] -> [a]`. `drop n l` is the list obtained from `l` by removing the first `n` things.
* `splitAt :: Int -> [a] -> ([a], [a])` splits a list in two.
* `takeWhile :: (a -> Bool) -> [a] -> [a]`
* `dropWhile :: (a -> Bool) -> [a] -> [a]`

##### [Exercises](s9_6.hs)

Note that we did exercise 1 [before](../ch4/ec/reverse.sh) but not with `takeWhile` and `dropWhile`.

#### 9.7 List Comprehensions

`[ expr | symb <- list]` is a list comprehension, and looks sort of like set constructions in
mathematics. For example, `[ x^2 | x <- [1..10]]` is the first 10 squares. You can add predicates
that basically filter, with syntax `[ expr | symb <- list, pred ]`, like `[ x^2 | x <- [1..10], rem x 2 == 0]`.
You can have multiple generating lists, `[expr | sym1 <- list1, sym2 <- list2]`, in which case all pairs
will be generated (vs, for example, walking through the two lists simultaneously).

`elem :: Eq a => a -> [a] -> Bool` tells you if an element is in a list or not

##### [Exercises](s9_7.hs)

#### 9.8 Spines and nonstrict evaluation

Data structures in haskell, particularly lists, sequences, and trees, have a "spine", the connective
structure that ties a collection of values together. For a list, the spine is cons, `(:)`.

Laziness means none of a list, like `4 : 3 : 2 : 1 : []`, is constructured until it's consumed.

"bottom", or undefined, ⊥ (unicode 22A5), can be put at the front of a list, and you can ask for
the tail, and not get any exceptions.

`:sprint` is a GHCi built-in to print variables and see what has been evaluated already. It has
some quirks, and you may have to force a polymorphic type sometimes, but it's still useful. For example,

    let l = undefined : undefined : undefined : undefined : []
    :sprint l -- prints _
    length l -- prints 4
    :sprint l -- prints [_,_,_,_]

That's fun, because you can have a list containing expressions that might fail or take a long time
to compute, but you can still quickly get the size of the list. Note, though, that the book example
of `:sprint blah` where `blah =  enumFromTo 'a' 'z'`, ends up forcing the values.

"Normal form" means an expression is fully evaluated. "Weak head normal form" (WHNF) means it is only
evaluated as far as necessary to reach a data constructor. Normal form implies weak head normal form.
A lambda expression, awaiting an argument to apply to, is in WHNF.

`(1, 1+1)` is in WHNF, because it's a tuple, while `"a" ++ "b"` is not because the outermost expression
is the `++` application, and both arguments are provided.

`length` only forces the spine to be computed, while `sum` forces the spine and the values.

When writing functions, think about what things your pattern matching forces. Use `_` when you can,
unless you have a good reason not to.

##### Exercises

Will it blow up?

1. `[x^y | x <- [1..5], y <- [2, undefined]]` is fine, you could even ask for `length`
2. Yes, you can `take 1` from the previous problem because it's computable (`1^2`)
3. No, you can't `sum` a list with `undefined` in it
4. Yes, you can get the `length` of a list with an `undefined` in it
5. No, `length $ [1,2,3] ++ undefined` breaks, because length requires computing at least
    the spine of `undefined`, which doesn't exist. Note that if you wrapped `undefined` up in
    a list, though, `[1,2,3] ++ [undefined]`, it'd work.
6. You can `take 1 $ filter even [1,2,3,undefined]` because it stops running when it finds 2.
7. You can't `take 1 $ filter even [1,3,undefined]`, though, because it doesn't succeed before
    getting to `undefined`, and so tries to evaluate `even undefined`.
8. You can `take 1 $ filter odd` on that list though
9. Or even `take 2`
10. But not 3

Is it in normal form?

1. `[1,2,3,4,5]` is fully evaluated, NF
2. `1 : 2 : 3 : 4 : _` is in WHNF, as a `:`
3. `enumFromTo 1 10` is in WHNF, also a cons
4. `length [1,2,3,4,5]` is no NF or WHNF, it's a function with all its arguments
5. `sum (enumFromTo 1 10)` is also not NF or WHNF
6. `['a'..'m'] ++ ['n'..'z']` same problem, `++` application, used as example in the text
7. `(_, 'b')` is WHNF, a tuple constructor

#### 9.9 Transforming lists of values

`map` and `fmap` let you apply a function to all elements of a list. The difference between the
two is that `fmap` is a more general function signature, appyling to any `Functor` (more on
this later), while `map` applies specifically to lists.

* `:t map` is `(a -> b) -> [a] -> [b]`
* `:t fmap` is `Functor f => (a -> b) -> f a -> f b`

Note that `map` is lazy, only applying the function to evaluate cells in the list that it has to.

A performance mantra for Haskell is "lazy in the spine, strict in the leaves." But the book
doesn't really provide an example.

##### Exercises

More Bottoms

1. `take 1 $ map (+1) [undefined, 2, 3]` fails, it has to `(+1) undefined`
2. `take 1 $ map (+1) [1, undefined, 3]` is fine though, only computes the first element
3. But `take 2` of that again fails because you hit the undefined
4. `itIsMystery xs = map (\x -> elem x "aeiou") xs` converts a `String` to a list of
    `Bool`, each element being "was the character a lowercase vowel?"
5.
    1. `map (^2) [1..10]` is `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]`
    2. `map minimum [[1..10], [10..20], [20..30]]` is `[1, 10, 20]`
    3. `map sum [[1..5], [1..5], [1..5]]` is `[15, 15, 15]`
6. I think this exercise is asking us to rewrite
    
    `map (\x -> if x == 3 then (-x) else (x)) [1..10]`

    using `bool`, from `Data.Bool`, with type `a -> a -> bool -> a`. If so, the following works

    `map (\x -> bool (-x) (x) (x == 3)) [1..10]`

    It might be somewhat more readable as

    `map (\x -> bool (-x) (x)) $ map (==3) [1..10]`

#### 9.10 Filtering lists of values

Recall `filter`, with type `(a -> Bool) -> [a] -> [a]`.

Recall, also, filtering within a list comprehension. For example, `[x | x <- [1..20], even x]`

##### Exercises

1. `let filterMultiplesOfThree = filter (\x -> rem x 3 == 0)`

    However, the following is somewhat more entertaining

    `let fmot = filter $ (==0) . (flip rem $ 3)`
2. `let numMultiplesOfThree = length . filterMultiplesOfThree`
3. `let noArticles = filter (\x -> not $ elem x ["the", "a", "an"]) . words`

#### 9.11 Zipping lists

Zipping lists combines elements from multiple lists. You can `zipWith` to specify how the elements
come together.

* `:t zip` is `[a] -> [b] -> [(a,b)]`
* `:t unzip` is `[(a,b)] -> ([a], [b])`
* `:t zipWith` is `(a -> b -> c) -> [a] -> [b] -> [c]`

Note that `zip` must stop on the shortest list.

##### [Exercises](s9_11.hs)

#### 9.12 [Chapter Exercises](chapEx.hs)

* `:t isUpper` is `Char -> Bool`
* `:t toUpper` is `Char -> Char`
* `:t chr` is `Int -> Char`
* `:t ord` is `Char -> Int`

#### 9.13 Definitions

1. **Product type** is a data constructor with more than one argument (e.g., a tuple). They're
    in some real sense an "and"
2. **Sum type** are represented using the pipe, `|`, in a datatype definition. They're an "or".
3. **Cons** is typically a verb meaning to put a value at the head of a list. It is represented
    by the `(:)` operator, and is the data constructor for the list type.
4. A **cons cell** is a product data constructor of the types `a` and `[a]`.
5. A **spine** is a structure that glues together values in a collection. In the list type it
    is the recursive nesting of cons cells.

### Meetup topic seeds

1. "cons cells" and "spines" don't seem to mean much to me. Anybody else? Maybe in 9.8 they start
    making more sense... there's the structure (spine) and the things in the structure (elements).
2. How did you all do the EnumFromTo exercises in section 9.5? Did you use `succ`?
3. Exercises in 9.6, splitting a string (use take/drop, using any character)...
    what about taking a predicate for characters, instead of just a character?
    Can you then pass a predicate that is wrapped in some sort of closure and remembers
    what it was split on before, and changes that? Like, split on the first "a", then the next "b", and so on?
4. What'd everybody get for the 9.8 "Is it in normal form exercises?"
5. Example of "lazy in the spine but strict in the leaves" for performance?
6. How'd everybody's Ceaser cipher function go?
7. What's better for `myAny`, a recusive pattern matching definition, or `myOr . (map f)`?
8. `myMaximumBy`, in the chapter exercises, seemed interesting. How'd it go?
9. Anybody poke at the "ninety-nine haskell problems" in the follow-up resources? Anything fun?

