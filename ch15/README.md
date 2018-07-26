## Chapter 15: Monoid, Semigroup

### My Reading Notes

#### 15.1 Monoids and semigroups

An __algebra__ is one or more __operations__ and the __set__ they operate over.
Haskell is very good at some standard algebras (monoid, semigroup, functor, monad).

#### 15.2 What we talk about when we talk about algebras

Algebra, in this context, means the study of symbols and the rules governing their
manipulation.

"An algebra" refers to some operations and the set they operate over. In Haskell,
these can be represented by typeclasses, where the typeclass defines the operations.
The set they algebra operates over is the "type" the operations are for.

#### 15.3 Monoid

A __monoid__ is a binary associative operation with an identity. This means
1. It operates on two things of a type, and produces a third thing of the type
2. If you have 3 things, you can group it as (12)3 or 1(23), and the order in which
    you do so doesn't matter, you'll get the same result. This means you don't have
    to care where the parens are, and can just write 123. Note that order may still matter,
    12 may or may not be 21.
3. There's an element, say _e_, which you can combine with any other, say _x_, and get
    the other element ( _x_ ) back.

For lists, `(++)` is a monoid. It takes two lists, and doesn't care where the parens are,
and has the empty list, `[]` as identity.

The `Monoid` typeclass has the `mappend` function, which for lists is `(++)`.

The `Monoid` typeclass uses `mempty` for the identity value, which for lists is `[]`.

#### 15.4 How Monoid is defined in Haskell

The `Monoid` typeclass captures those types that have a binary associative operator and
and identity. `class Monoid m` requires
* `mempty :: m` (the identity)
* `mappend :: m -> m -> m` (the binary operator)
* `mconcat :: [m] -> m ; mconcat = foldr mappend mempty` (a convenience)

#### 15.5 Examples of using Monoid

**List** is a `Monoid` with concatenation and the empty list.

#### 15.6 Why Integer doesn't have a Monoid

`Integer`, and the other numeric types, doesn't have a `Monoid` instance, because it
has several! It could be `+` with `0` as the identity, or `*` with `1` as the identiy.

The `Data.Monoid` module provides the `Sum` and `Product` `newtypes` to account for this.

Recall that `newtype` promises no runtime overhead, and requires a single unary data constructor.

There's an infix notation for `mappend`, namely `(<>)`, with type `Monoid m => m -> m -> m`.
It comes in the `Data.Monoid` package. It is useful for if you want to apply the operation
a few times. Since order doesn't matter, you can just `m1 <> m2 <> m3 <> m4`, vs trying
to gether up pairs to make `mappend` happy.

#### 15.7 Why bother?

Monoids can be useful for things like aggregating a large dataset which you might want to
do with parallel batches, but then you really need an associative operator. The identity
helps if you need to fill in a batch, or so.

We'll see more of a relationship between monoids and folding (catamorphisms) later.

#### 15.8 Laws

Monoid instances must obide by the following laws:
1. `mappend mempty x = x`, left identity
2. `mappend x mempty = x`, right identity
3. `mappend x (mappend y z) = mappend (mappend x y) z`, associativity
4. `mconcat = folder mappend mempty`

Haskell doesn't actually enforce these laws for you instances though.

#### 15.9 Different instance, same represenation

The two instances of Monoid for numerics are addition and multiplication. For lists, we
have seen the "addition" version, given by concatenation. In general, it is reasonable
to think of a monoid as a summarization (vs sum).

The `Bool` type has "conjunctive" (and) and "disjunctive" (or) monoids, represented by
the `newtype`s `All` and `Any` (from `Data.Monoid`), respectively.

`Maybe` has numerous monoid instances. The first two we'll see are `First` and `Last`,
which have a preference for the... first... or... last... `Some` in a series (again,
recall that monoids do not require commutativity, so order is allowed to matter).

#### 15.10 Reusing algebras by asking for algebras

Another monoid instance for `Maybe` is obtained by combining, with a monoidal structure
for the wrapped type `a`, the `Just` elements that exist.

Note that `instance Monoid b => Monoid (a -> b)`, and `instance (Monoid a, Monoid b) => Monoid (a, b)`.

##### Exercise: Optional Monoid

```
data Optional a = Nada | Only a deriving (Eq, Show) -- a type synonym for Maybe, Monoid (Maybe a) is already evidenced
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only a') = Only (mappend a a')
```

##### The problem of orphan instances

If you get a GHC warning about orphan instances, fix it. If you own the typeclass or the datatype,
you can put the instance in the same module. If not, you can `newtype` the datatype and provide an
instance of it (we'll see ways to improve this later).

`ghc -I. --make file.hs` will include the current directory (`.`) as available sources for
modules when building `file.hs`.

#### 15.11 [Madness](madness.hs)

#### 15.12 Better living through QuickCheck

As a fun aside, you can bind infix names for function arguments, just wrap it in parens, like
you'd guess.

`verboseCheck`, in `QuickCheck`, lets you see what random values were used.

Anyway, the point of this section is to use `QuickCheck` to validate the laws of your algebra,
for your instances.

##### Exercise: [Maybe Another Monoid](s12_mam.hs)

#### 15.13 Semigroup

A _semigroup_ is like a monoid, it just doesn't have an identity (so it's just an associative operator).

An example is non-empty lists, with concatenation, which does not form a Monoid, and is available in
`Data.List.NonEmpty`, with the infix data constructor `a :| [a]`.

In GHC 8, you have to `import Data.Semigroup`, because it isn't in the stanard `Prelude`.

Note that data constuctors can be infix, and are infix by default if they have only non-alphanumeric symbols
and begin with a colon.

#### 15.14 Strength can be weakness

Sometimes, folks talk about an algebra being "strong", if it provides many operations. However,
this puts more constraints on what types can be used in your algebra.

If you remove the associativity constraint you get a _magma_.

#### 15.15 Chapter Exercises

I honestly didn't do a full and great job on these, defining all the QuickCheck stuff you should do.

[Semigroup exercises](chEx-semigroup.hs)

[Monoid exercises](chEx-monoid.hs)

#### 15.16 Definitions

A *monoid* is a set that is closed under an associative binary operation with an identity.
*Closed* means that what you apply the operation to two elements of the set, you get another element
of the same set.

A *semigroup* is like a monoid, but doesn't require an identity element.

Usually, in a Haskell context, an *algebra* is a type and operations over that type, and laws
about those operations (e.g., associativity).

### Meetup topic seeds

1. Can we come up with the second monoidal structure on lists, beyond concatenation?
    Apparently we'll see it in a later chapter.
2. What makes `madlibbinBetter'` better? I could almost see it if you had a functinon that sort of
    took the intermediate literal text as a `[String]`, and then also took in a (H)List of the
    appropriate terms (Exclamation, Adverb, Noun, Adjective in the book example), and created the
    function that mapped an input sequence of words to the filled in madlib, maybe by zipping the lists
    and then calling `mconcat`.
3. Other examples of semigroup that aren't monoid?
4. Example of a magma that isn't a semigroup?
5. Showing `T a` is a semigroup seems to break into two cases: rely on `a` to have the structure, or don't.
    Are there different implications about (or names/adjectives for) `T` in the two cases?
6. What would the result of `print $ runMem (f' <> f') 0` be, in monoid exercise 8 (p. 953 in my edition)?
7. [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Monoid), useful in general.

