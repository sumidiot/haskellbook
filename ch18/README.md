## Chapter 18: Monad

### My Reading Notes

#### 18.1 Monad

Monads are applicative functors, but with more.

#### 18.2 Sorry - a monad is not a burrito

The definition of Monad in Haskell is:

```
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a
```

##### Applicative m

You can "derive" functor and applicative from monad, meaning that you can actually
define the `fmap` for a monad, it is given by

   `fmap f xs = xs >>= return . f`

##### Core operations

You only need to define `(>>=)` for a minimally complete `Monad` instance. `return` is the
same as `pure`.

Sometimes `>>` is referred to as the sequencing operator, because it sequences two actions while
discarding any resulting value from the first action. We'll see examples soon in the section on
`do` syntax. `>>` has a counterpart in `Applicative` that we skipped but will return to.

Finally, the main operator, `(>>=)` is called `bind`, and is what makes `Monad` special.

##### The novel part of `Monad`

If you call `fmap` and the `b` (target type) is actually an `f b`, then the signature is
`f a -> (a -> f b) -> f (f b)`, and so while we haven't changed the outer structure (because `fmap`
can't), we have introduced more structure inside it. We'd rather just have a once-wrapped output
at the end. We've seen, with `concat :: [[a]] -> [a]` (or, more generally, `Foldable t => t [a] -> [a]`),
that we can eliminate an extra layer of structure from lists, and `Monad`s are somewhat like
a generalization of that. In fact, `Control.Monad` provides

    `Control.Monad.join :: Monad m => m (m a) -> m a`

So what makes monads special is the ability to flatten two layers of structure into one.

##### The answer is the exercise

   ```
   bind :: Monad m   => (a -> m b) -> m a -> m b
   fmap :: Functor m => (a -> b) -> m a -> m b
   join :: Monad m   => m (m a) -> m a
   bind = join . fmap
   ```

##### What `Monad` is not

1. Impure. While `IO` has a `Monad` instance, `Monad` is not impure
2. A way to make Haskell look imperative. While `Monad`s are frequently used for sequencing actions
    that looks imperative (particularly `do`-syntax), not all monads order actions (e.g., `Reader`,
    which we'll see later).
3. A value. A typeclass is not a value, in general.
4. About strictness. We'll talk more about strictness later.

##### `Monad` also lifts!

There are `liftM` versions of the `liftA` methods we saw in the [previous chapter](../ch17). These
are mostly a historical artifact, they are equivalent to the `liftA` functions.

#### 18.3 Do syntax and monads

There are sequencing operators, `(*>)` (with an `Applicative` constraint) and `(>>)` (with a `Monad`
constraint), that do the same thing, `m a -> m b -> m b`.

`do`-syntax allows for two common operations

1. doing things in sequence, where newlines are effectively replaced by `(*>)` or `(>>)` depending
    on the types, and
2. assigning names to results (like `name <- getLine`), and using them later (`putStrln name`),
    which is equivalent to using `(>>=)` (e.g., `getLine >>= putStrLn`). In fact, you can imagine it
    replacing `a <- b` with `b >>= \a ->`, the final term being an anonymous function which calls its
    input `a`.

##### When `fmap` alone isn't enough

`putStrLn <$> getLine` is not the same as `getLine >>= putStrLn`. The first will wait for input
(`getLine`), but won't print anything, while the second will do both. In the first case, the type
is `IO (IO ())`, with the outer `IO` being the `getLine` `IO`, because that has to happen first for
`<$>` to have anything to operate over, and `<$>` can't change the outer structure - `getLine` returns
and `IO String`, so `f <$> getLine` will be an `IO (f String)`. Haskell, being lazy, only does the outer
`IO` action. However, if you `join` the `IO (IO ())`, to a `IO ()`, Haskell will do the whole thing -
read your input and echo it back to you.

Here's an example of how `do` de-sugars:

   ```
   do
     putStrLn "name: "
     name <- getLine
     putStrLn ("hi " ++ name)
   --- is the same as
   putStrLn "name: " >>
   getLine >>= \name ->
     putStrLn ("hi " ++ name)
   ```

Like we saw with applicatives, that nesting that we see when we use `>>=` can become ugly if you do
it more times, and that's why `do` (and `Monad` and `Applicative`) are helpful, because they flatten
out that nesting for you.

#### 18.4 Examples of `Monad` use

##### List

You can `x <- xs` in a `do` when `xs` is a `Monad`, to iterate through the elements.

##### Maybe

In the previous chapter, we saw how `Applicative` let us simplify argument checking / input validation
with the `do`-syntax. However, in this chapter we note the need for `Monad`, if any of the steps you're
doing generate values that are used later on. In the last chapter, we took `String -> Maybe Name`
and `Int -> Maybe Age` to create a `String -> Int -> Person {name: Name, age: Age}`. If, however, the
name and age were related via some checking (if your name is Paul you have to be at least 30), then
`Applicative` wouldn't do all you wanted, but `Monad` could. This is because you're generating monadic
values from monadic values (you get a `Maybe Person` given a `Maybe Name` and `Maybe Age` and some logic),
so they're effectively nested (similar to the `IO (IO ())` we saw above), and so you need to collapse
them, using `join`.

So, as in the validation example, you can write the various case-matchings and nestings as desired,
but if you rely on the `Monad` and `Applicative` structures, you get to use the shorter, easier,
`do`-syntax.

##### Either

Being a `Monad` means that later values in a `do`-sequence can depend on earlier ones. So if you are using
`Either`s, the whole sequence will fail with the **first** failure. This is different from the `Validation`
type (`Either`s other `Applicative`).

`Applicative` and `Monad` must have the same behavior. Given a `Monad`, you can define the `Applicative`
`ap` (tie-fighter, `<*>`), with type `Monad m => m (a -> b) -> m a -> m b`, as

   ```
   ap mf ma = do
     f <- mf
     a <- ma
     return (f a)
   ```

Written with `>>=`, this becomes `ap mf ma = mf >>= (\f -> ma >>= (\a -> return (f a)))`.

#### 18.5 Monad laws

To write your own monad, you only have to provide `>>=`.

##### Identity laws

The right identity is: `m >>= return = m`

The left identity is: `return x >>= f = f x`

##### Associativity

`(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)`

As usual, the `checkers` library provides ways to check the monad laws.

It's important to write tests for your instances, because it can be easy to write things which
type-check but do not satisfy the laws.

#### 18.6 Application and composition

Composition "just worked" when we were dealing with functor and applicative, because the function
never introduced more structure, it was always `a -> b`, where monad has `a -> m b`. That complicates
composition, but you can work around it because you can `fmap` and `join`. So, if `f :: b -> m c`,
and `g :: a -> m b`, then you can compose them with `mcomp f g a = g a >>= f` to get an `a -> m c`
(this is also equivalent to `mcomp f g a = join (f <$> (g a))`. The function `mcomp` we are suggesting
here is built-in to Haskell, under the operator `(>=>)`, known as the Kleisli fish operator:

   `(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c`

This operator is in the `Control.Monad` module.

#### 18.7 Chapter Exercises

[Instances](chEx-instances.hs)

[Monadic Helpers](chEx-helpers.hs)

#### 18.8 Definition

1. **`Monad`** is a typeclass that deals with functorially appyling a function that introduces more structure.
    It's primary method is `(>>=) :: Monad m => m a -> (a -> m b) -> m b`. `flip (>>=)` also exists, and
    is denoted `(=<<)`, and also goes by the name `bind`. It has type `(a -> f b) -> f a -> f b`, which
    somewhat lines up better with `fmap` and `(<*>)`.
2. A **monadic function** generates more structure, like the function in the signature to `(>>=)`.
3. **bind** is overloaded, from `let` expressions like we saw earlier in the book, to monadic lifted functions,
    under `(>>=)`.

### Meetup topic seeds

1. I found the `Either` applicative and monad instances interesting to write. The types are a little
    hard to think about.
2. You can define applicative `<*>` in terms of bind (see end of 18.4). They say you only need `>>=`
    to get a `Monad`, so how do you define `return` (also giving you Applicative's `pure)`, or `fmap`?
3. I found the `meh` and `flipType` chapter exercises interesting.
