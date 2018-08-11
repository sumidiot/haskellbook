## Chapter 17: Applicative

### My Reading Notes

#### 17.1 Applicative

Applicatives are monoidal functors. It's a functor, so functions lifted over structure,
but the function is also embedded in a some structure, so we have to smash the structures
together.

#### 17.2 Defining Applicative

```
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

So, to be applicative, you have to first be a functor, and then also have `pure` and `(<*>)`.
The infix `<*>` is sometimes called "apply" or "ap", or "tie fighter".

`Control.Applicative` provides some utilities, like `liftA`, `liftA2`, and `liftA3`:

* `liftA  :: Applicative f => (a -> b) -> f a -> f b`
* `liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c`
* `liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d`

Note that `liftA` is the same signature as `fmap`, just with an `Applicative` constraint, vs
just `Functor`.

### 17.3 Functor vs Applicative

One relationship is that `fmap f x = pure f <*> x`. For example,
`fmap (+1) [1,2,3] = [2,3,4]`, which is the same as `pure (+1) <*> [1,2,3]`. In this example
`f` is `[]`, and `pure (+1)` in the context of `<*> [1,2,3]`, is a `[Int -> Int]`. Given this
wrapped function `f (a -> b)`, we can `<*>` to a wrapped `a` (in this case `Int`), and get
a wrapped `b` (still `Int` - really `Numeric` everywhere I have `Int`).

Taking this example as inspiration, thinking about `f = []`, we see `Applicative []` as
"given a list of functions `a -> b`, and a list of `a`, produce a list of `b`". So that looks
like it probably is implemented, roughly, as zip and apply. Actually, though, if you try it,
that's not quite right. `[(+1), (+2)] <*> [1,2,3] = [2,3,4,3,4,5]`, so it's more like all the
pairwise products, or you might think of it like nested for loops (for function in list, for
element in list, apply function to element). Really, think of fmapping each function over the
value list, producing a list of lists, and then using the monoidal structure of list to
collapse that list of lists to a single list, via concatenation.

`pure` is a way of embedding a value in a structure you're working with.

#### 17.4 Applicative functors are monoidal functors

In `<*>`, we have an `f (a -> b)` and an `f a`, and need an `f b`. Separating the structure
from the contents, this looks roughly like `f -> f -> f` and `(a -> b) -> a -> b`. The later
is function application, and the former is a monoidal `mappend`, `|*|`.

I sort of showed an example of `[]` being `Applicative` above.

`Maybe` is also `Applicative`, basically if either side is `Nothing` the result is nothing,
and if both sides are `Just`, the result is `Just` function application. Normal functor-ness
of `Maybe` maps over the possibility of the value not existing, while the applicative-ness
also allows the function to not exist.

##### Show me the monoids

For the two-tuple, `(a,b)`, if `a` is `Monoid`, then `(a,b)` is applicative, even without
`b` being (while the monoid structure for `(,)` requires both arguments to be monoids).
`("woo", (+1)) <*> (" Hoo!", 0)` is `("Woo Hoo", 1)`.

##### Tuple Monoid and Applicative side by side

```
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
```

##### Maybe Monoid and Applicative

As mentioned before, the `Maybe` applicative's `<*>` will be `Nothing` if either side is,
or `Just` application otherwise, aligning with the analogous monoid structure on `Maybe`,
where if either side of `|+|` is `Nothing` the whole thing is.

Different monoidal structures give different applicative structures, like you might guess,
and we'll see more examples soon.

#### 17.5 Applicative in use

Recall GHCi's `:set -XTYpeApplications`, which let's you see a specialized type signature,
like `:type (<*>) @[]` showing `(<*>) @[] :: [a -> b] -> [a] -> [b]`.

`(,) <$> [1, 2] <*> [3, 4]` is `[(1,), (2,)] <*> [3, 4]`, which is `[(1,3), (1, 4), (2, 3), (2, 4)]`.
It could also be written `liftA2 (,) [1,2] [3,4]`.

Tangentially, to serve more examples, there's some other utilities:

* `lookup :: Eq a => a -> [(a, b)] -> Maybe b`
* `Data.Map` provides `Map`, which has a `fromList`, converting a list of tuples to a `Map`.
* `Data.Map.lookup :: Eq a => a -> Map a b -> Maybe b`

`IO` is also applicative, `(++) <$> getLine <*> getLine`, or `(,) <$> getLine <*> getLine`.

##### Identity

The `Identity` type can help wrap things in an extra layer. For example, `const <$> [1,2,3]` is
`[const 1, const 2, const 3]`, but `const <$> (Identity [1, 2, 3])` is `Identity (const [1, 2, 3])`.

##### Constant

`Constant` seems, at first, like `Identity`, some weird thing you wouldn't use, but it does come up.
like `const`, it ignores one of its arguments.

### Meetup topic seeds

1. Struggled with exercise 4 in "Lookups" section of 17.5, p.1077 in my book.
