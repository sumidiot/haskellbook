## Chapter 22: Reader

### My Reading Notes

#### 22.1 Reader

It is common to have some information that is needed intermittently or universally throughout
an application, and we don't want to have to add it to the type signature of all of our functions.
`Reader` is supposed to help with that.

#### 22.2 A new beginning

There is a `Functor` instance for functions, so you can `fmap f g`, where `g` is a function that
produces an `b` and `f` is a function that takes in a `b`. That is, `fmap f g = f . g`. The functor
here is actually `Monad`ic, so you could also write this as `do; b <- g; return $ f b`.

`Reader` is another name for "partially applied function", meaning a function awaiting an argument.
It allows us to not pass a bunch of arguments around a function, and just supply them in one place
"at the end".

##### [Short Exercise: Warming Up](s2_warmup.hs)

#### 22.3 This is Reader

In this text, `Reader` refers to the `Monad` instance of the functions `Functor`, where `fmap` is composition.

There's a `ReaderT` monad transformer we'll read more about later.

#### 22.4 Breaking down the `Functor` of functions

`:info Functor` in a REPL shows `instance Functor ((->) r)`, partially applied functions that take an
`r` as input. `fmap` is given by `(.)`, function composition. As with `Either`, whose first type
parameter is _not_ part of the `Functor`, so it is with `(->) a b` - lifting a function over this
(`fmap`ping) won't change the `a`. The value being transformed by an `fmap` is the `b`, and so we'll
`fmap f` where `f :: b -> c`.

#### 22.5 But uh, `Reader`?

`Reader` is a newtype for the function type, `newtype Reader r a = Reader { runReader :: r -> a}`.
Note that this gives us a `runReader` function to extract the function.

##### [Exercise: Ask](s5_ask.hs)

#### 22.6 Functions have an `Applicative` too

`pure` specializes to, for `(->) r`, to `a -> (r -> a)`, so `pure = const`. `(<*>)` specializes to
`(r -> a -> b) -> (r -> a) -> (r -> b)`, so `(rab <*> ra) r = rab r (ra r)`.

##### [Demonstrating the function `Applicative`](s6_app.hs)

##### [Exercise: Reading Comprehension](s6_comp.hs)

The `InstanceSigs` `LANGUAGE` pragma lets you specialize the type signature of a typeclass method,
which seems helpful for debugging and actually just specifying instances.

#### 22.7 The `Monad` of functions

The `return` is the same as `pure`, and is just `const`. The monadic bind has type
`(>>=) :: (r -> a) -> (a -> r -> b) -> r -> b` so is given by `(>>=) ra arb r = arb (ra r) r`,
which looks very similar to the applicative `<*>` definition, they're very close, just some of the
orders of the arguments are different.

#### 22.8 `Reader` `Monad` by itself is boring

The `Monad` for `Reader` doesn't get you anything beyond the `Applicative`, really, as we just saw
with their implementations. You can implement the bind with `flip` and `<*>`.

#### 22.9 You can change what comes below, but not above

If you want to call `f :: r -> a` but have an `r'`, you have to have an `r' -> r` first.

#### 22.10 You tend to see `ReaderT`, not `Reader`

`Reader` is usually one monad in a stack of types. In that setup, it's usually a monad transformer,
which is frequently indicated with `T` added to the end, so `ReaderT`.

We'll see more about `ReaderT` and other monad transformers in a few chapters.

### Meetup topic seeds

1. In the section 22.2 "Warming Up" exercise, the version of `tupled` written with `>>=` is interesting,
    I'm not sure how to write it _not_ point-free.
2. What's the instance of `Applicative` for `Reader r`? In particular, `<*>`? How do you `checkers` it?

