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

### Meetup topic seeds

