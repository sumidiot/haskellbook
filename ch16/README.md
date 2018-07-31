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

### Meetup topic seeds

