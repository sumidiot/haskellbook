## Chapter 20: Foldable

### My Reading Notes

#### 20.1 Foldable

We've been seeing `Foldable` for a while, and in this chapter we'll get more into it, and explore
catamorphisms more.

Folds always rely on some monoid instance.

#### 20.2 The `Foldable` class

The typeclass begins

```
class Foldable t where
  {-# MINIMAL foldMap | foldr #-}
```

showing that a minimally complete definition can be obtained with just `foldMap` or `foldr`.
The kind of the parameter `t` is `* -> *`.

#### 20.3 Revenge of the monoids

Folding implies a binary associative operation that has an identiy, which we see in the type
signatures for the two defining functions:

```
class Foldable (t :: * -> *) where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
```

##### And now for something different

`foldMap` first converts elements of a foldable into monoidal values and then uses that monoidal
structure to fold the results. To `foldMap` an empty structure will end up using `mempty`, and
to fold only one value you'll end up not using the `mappend`.

#### 20.4 Demonstrating `Foldable` instances

##### Identity

The identity is only going to have one value in it, so folding is not really going to be combining
values, mostly just consuming them, converting them to the monoid.

##### Maybe

To `foldMap` a `Nothing` you need the monoidal `mempty`. For a `Just` you'll apply the folding
function and, as in `Identity`, dispose of the structure (here the `Maybe`, above the `Identity`).

Note that things like `foldMap (+1) (Just 10)` fail, because there's no default monoid instance
for integers, so you have to ask for the monoid you want, `foldMap (+1) (Just 10) :: Sum Int`.

#### 20.5 Some basic derived operations

* `toList :: t a -> [a]`. It is `foldMap (\x -> [x])`
* `concatMap :: Foldable t => (a -> [b]) -> t a -> [b]`
* `null :: t a -> Bool`, tests for empty
* `length :: t a -> Int`
* `elem :: Eq a => a -> t a -> Bool`, see if a thing is an element of the foldable
* `maximum :: Ord a => t a -> a` and `minimum :: Ord a => t a -> a`, but note that you aren't allowed
    to apply these to an empty structure
* `sum :: (Foldable t, Num a) => t a -> a` and `product` (with the same type)

Note that since the first type argument of `Either` is part of the structure, things like `elem`
can't see into it. So, `elem True (Left True)` is `False`.

##### [Exercises: Library Functions](s20_5.hs)

#### 20.6 Chapter Exercises

[Instances](chEx-instances.hs)

[Filter function](chEx-filter.hs)


### Meetup topic seeds

1. What's going on with `fmap length Just [1, 2, 3]` returning 1? How does that parse?
2. When you define a typeclass, like `class Foldable t where`, you then write a bunch of method
    signatures that involve `t`. If any of them weren't actually part of the structure of the
    typeclass, you could write them anyway by just adding another type constraint, right? Like,
    `toList :: t a -> [a]` becomes `toList :: Foldable t => t a -> [a]`.
3. My solution to exercise 5, in section 20.5, I'm not crazy about.
4. What are interesting uses of the `filterF` chapter exercise?
5. Can't you make an instance of `Foldable` for anything, with `foldMap f as = mempty`?

