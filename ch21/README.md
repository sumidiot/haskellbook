## Chapter 21: Traversable

### My Reading Notes

#### 21.1 Traversable

`Traversable` was introduced in the same paper as `Applicative`. It allows you to

* transform elements in a structure, like functor
* produce applicative effects along the way, and
* lift those instances outside of the traversable structuree.

It is commonly described as

* a way to traverse a data structure,
* mapping a function inside a structure,
* while accumulating applicative contexts along the way.

#### 21.2 The `Traversable` typeclass definition

`Traversable` is defined in `Data.Traversable` as the typeclass

```
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

  {-# MINIMAL traverse | sequenceA #-}
```

#### 21.3 `sequenceA`

`sequenceA` flips the layers of a double-nested structure, for example converting a list of
`Maybe`s to a `Maybe` of a list. Note that there's a similar utility `Data.Maybe.catMaybes` with
type `[Maybe a] -> [a]`, which unpacks `Just`s and removes `Nothing`s. This is different from
`sequenceA`, which will return a `Nothing` from any list of `Maybe`s for which any are `Nothing`.

#### 21.5 `traverse`

`traverse` is somewhat similar to flipped bind, `(=<<)`, in terms of function signatures:

```
(=<<)    :: (a -> m b) -> m a -> m b
traverse :: (a -> f b) -> t a -> f (t a)
```

though note that `traverse` introduces an extra structure, where `bind` introduces more of the same
structure, which can be eliminated with the monadic `join`.

##### `mapM` is `traverse`

The monadic map has a similar type signature as `traverse`:

```
mapM     :: (a -> m b) -> [a] -> m [b]
traverse :: (a -> f b) -> t a -> f (t b)
```

Indeed, `mapM` is `traverse` for the case where `t` is `[]`, and `f` is a `Monad`, so `traverse`
is a generalization that only requires `Applicative`, not `Monad`, and works for things besides list.

Similarly, `sequence :: [m a] -> m [a]` is a specific instance of `sequenceA` (it seems like the name
`sequenceA` comes about because `sequence` was developed first, and so a new name had to be chosen
to not break existing code).

#### 21.5 So, what's `Traversable` for?

Any time you need to flip two type constructors, or map and then flip, that's probably `Traversable`.
It's generally better to use `traverse` when you have a `sequence` or `sequenceA` paired with a `map`
or `fmap`.

#### 21.6 Morse code revisited

`Data.Maybe` provides a nice utility, `fromMaybe :: a -> Maybe a -> a`.

`Data.Traversable` provides a method `sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)`,
equivalent to the `sequenceA` of the `Traversable t` instance.

`traverse` is just `fmap` followed by `sequence`, similar to how `>>=` is just `fmap` followed by `join`.

#### 21.7 Axing tedious code

#### 21.8 Do all the things

[`wreq`](http://hackage.haskell.org/package/wreq) is an HTTP client library.
[httpbin.org](http://httpbin.org/) is a website that is handy for testing HTTP clients.

It is possible to import a module and ignore some exports, e.g., via `import A hiding (f)`.

The `wget` library has a `get` method, and so we could `map get urls` to get a `[IO (Response ByteString)]`,
but instead of having a bunch of `IO` actions we can perform, we might prefer one large `IO` with a list
of responses, which is exactly what `traverse` does for us.

##### Strength for understanding

`Traversable` is stronger than `Functor` and `Foldable`, we can derive either from a definition of
`traverse` or `sequence`. Using `Identity`, with `Data.Functor.Identity.runIdentity :: Id a -> a`,
we can recover something like `fmap`. Similarly, with `Data.Functor.Constant` we recover `foldMap`.

#### 21.9 `Traversable` instances

##### Either

##### Tuple

#### 21.10 `Traversable` Laws

`Traversable` instances are supposed to follow a few laws:

1. *Naturality*: `t . traverse f = traverse (t . f)
2. *Identity*: `traverse Identity = Identity`
3. *Composition*: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

Coming from the `sequenceA` standpoint, the laws are:

1. *Naturality*: `t . sequenceA = sequenceA . fmap t`
2. *Identity*: `sequenceA . fmap Identity = Identity`
3. *Composition*: `sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA`.

#### 21.11 Quality Control

You can `QuickCheck` your `Traversable` instances with `checkers`.

#### 21.12 [Chapter Exercises](chEx.hs)

### Meetup topic seeds

1. Example of a `Foldable` that isn't `Traversable`?
2. My first guess of a functor for the `S` type of the chapter exercises didn't pass checkers,
    but did compile.
3. What should `traverse` mean for the `Tree` type of the chapter exercises, when `f` is `[]`?
    Playing with some examples, `length` gives the number of nodes in a tree, and if your function
    produces a list of size `n`, traversing that function over a tree of length `l` gives a resulting
    list of size `n^l`. The list is giving `n` options, and `traverse` is returning all of the possible
    ways you can apply any of those functions to each node in the tree. Thinking about the `sequenceA`
    version, if I have a `Tree` of lists, the list of `Tree`s would be the list of trees made by
    choosing one of the values at each node.

