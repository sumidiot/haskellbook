## Chapter 16: Functor

### My Reading Notes

#### 16.1 Functor

Functor is all about a pattern of mapping over a structure, with `fmap`.

The word functor seems to have come from a logician, Rudolf Carnap, in the 1930s,
to describe grammatical function words (e.g., negatation) and logical operations over
sentences and phrases.

#### 16.2 What's a functor?

A functor is a way to apply a function over a structure, and not change the structure,
just the things inside it. We have seen this already with lists, where `fmap` doesn't
change the length of the list.

The `Functor` typeclass is defined as
```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

#### 16.3 There's a whole lot of `fmap` goin' round

Recall that `map` is the same as `fmap` for lists, it just only works for lists. So,
`:t map` is `(a -> b) -> [a] -> [b]`, while `:t fmap` is
`Functor f => (a -> b) -> f a -> f b`.

`Maybe` is a functor, where `fmap` over `Nothing` is `Nothing`, while `fmap` over `Just`
modifies the thing the `Just` contains.

Tuples (where each coordinate has the same type) are functors, as is `Either a` for any `a`.

In GHC 8 or newer, you can `:set -XTypeApplications` on the REPL, and then do things like
`:type fmap @Maybe`.

#### 16.4 Let's talk about `f`, baby

The `f` in the typeclass definition for `Functor` has kind `* -> *`.

Each argument in the type signature for a function (e.g., `fmap`) must be fully applied,
which is to say, must have kind `*`. The kind of `->` is `* -> * -> *`.

##### Exercises: Be Kind

1. In `a -> a`, `a` has kind `*`
2. In `a -> b a -> T (b a)`, `a` has kind `*`, `b` has kind `* -> *` and `T` has kind
    `* -> *`.
3. In `c a b -> c b a`, `c` has kind `* -> * -> *`.

##### Functor is function application

Intuitively, if `f` is the identity wrapper, sort of like `newtype Id a = Id a`, then
`fmap` basically reduces to `(a -> b) -> a -> b`, which is just function application, `($)`.

`(<$>)` is an infix version of `fmap` (in `Data.Functor` if you have an older GHC). The
`$` it uses is suggestive of function application, as above. 

#### 16.5 Functor Laws

##### Identity

`fmap id == id`

##### Composition

`fmap (f . g) == fmap f . fmap g`

#### 16.6 The Good, the Bad, and the Ugly

Functors cannot change the structure, because the signature of `fmap` says the input wrapper,
`f a`, has the same type as the output wrapper, `f b`, and you have to preserve the identity
function. If you find you want to change the structure, and the contents, you've got
`(a -> b) -> a -> b`, just any old function. The point of `Functor` is to capture the
structure preservation.

In `data C a = H Int a`, since `Int` isn't part of the type constructor for `C`, we have to
consider it part of the structure, and so can't mess with it while defining `fmap` for `C`.

#### 16.7 Commonly used functors

`Maybe a`, `[a]`, and `(b,a)` are all functors (the final being a functor that ignores
the first element of the tuple).

`b -> a` is a functor. Given a map `a -> c`, `fmap (aToC) (bToA)` composes the maps.

##### The functors are stacked and that's a fact

Given type `List[Maybe[String]]`, which is `List[Maybe[List[Char]]]`, we note that `fmap` can be
composed with itself to reach into any given level of the nesting. `(fmap . fmap) f l` will leave
a `List` of `Maybes`, where the inner type will be the result type of `f`.

##### Exercises: Heavy Lifting

1. `a = (+1) <$> read "[1]" :: [Int]`
2. `b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])`
3. `c = (*2) <$> (\x -> x - 2)`
4. `d = ((return '1' ++) . show) <$> (\x -> [x, 1..3])`
5. [needed newlines](s16_7.hs)

### Meetup topic seeds

1. The function functor... `F(a) = Hom(a, a)` is the functor, I guess? `fmap f h = f . h`,
    where `h` is the `F(a)` (`f a`) of the functor. Note that actually, `F_b(a) = Hom(b, a)`
    is a functor of `a` for any `b` (you could post-compose a function out of `a` to any
    function that ends in `a`).
2. What does `fmap . fmap` mean (p. 991)? `fmap :: (a -> b) -> f a -> f b`. The book applies it to
    an `a -> b` and a `f (g a)` where `f` and `g` are both functors. So `(fmap . fmap)` must
    have type `(a -> b) -> f (g a) -> f (g b)`. Note that `(.)` has type `(b -> c) -> (a -> b) -> (a -> c)`.
    Re-writing slightly, `fmap :: (a -> b) -> (f a -> f b)`, so to compose that with another copy
    means `(a -> b) -> (f a -> f b)` composed with a thing that must then be of shape
    `(f a -> f b) -> (g (f a) -> g (f b))`, because the `a` of the second `fmap` is actually `f a` of
    the first, and the `b` of the second `fmap` is the `f b` of the first, and `fmap`'s second argument
    (or final two, depending on how much you curry) are functor-wrapped, we use `g` because `f` is taken.
    That's a decent amount for the compiler to put together! Ah, the book actually encourages walking
    through this a few pages later (p. 995).
