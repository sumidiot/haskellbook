## Chapter 19: Applying Structure

### My Reading Notes

#### 19.1 Applied structure

This chapter will include some examples of how the various topics we've seen are applied.

#### 19.2 Monoid

Monoids are everywhere once you start seeing them.

##### Templating content in Scotty

[scotty](https://github.com/scotty-web/scotty) is a web framework for Haskell. It has templating
support so that a URL with a parameter, `/:word` can then use that parameter. An example usage is
to `mconcat` strings together, filling in the value of the `word` parameter for a given web call.

##### Concatenating connection parameters

[Making a website with Haskell](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)
shows how to do that, using `scotty` (see last section). It uses `mconcat` in a `foldr` to
aggregate and modify parameters - like to url-encode parameters of a web request.

##### Concatenating key configurations

`xmonad` is a windowing system for X11, written in Haskell, and configured with Haskell code. It
uses `mappend` to override default settings. The example also shows the use of `!` which is a
*strictness annotation*, meaning it overrides the default Haskell laziness, but that's not the
point of this chapter.

The `Monoid` instance is the monoid `a -> b`, where `b` is a monoid. To combine `f :: a -> b` and
`g :: a -> b`, produce a new function, `(f <> g) :: a -> b` which is `f(a) <> g(a)`.

Recall `Data.Map`, giving ordered pairs of keys and values, and the utility function `fromList`
(note that you (may?) need to add the `containers` package to your build to get this).
There's a monoid structure for `Map`, where you keep the first instance of a key and ignore later
instances.

#### 19.3 Functor

##### Lifting over IO

It is relatively common to take a function that doesn't involve I/O, and lift it to `IO` so that
it can be applied to an `IO`-wrapped value. For example,
`addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime`, partially applied, is a function
`UTCTime -> UTCTime`, and a reasonable thing to do is to `fmap` it over the result of applying
`getCurrentTime :: IO UTCTime`.

There's also the `uuid` library, providing `Data.UUID`, a function `UUIDv4.nextRandom`, which
has type `IO UUID` (because it relies on a random number generator). One can convert a `UUID`
to `Text` (from `Data.Text`, in the `text` package), by going through `toString :: UUID -> String`
and `Text.pack :: String -> Text`. While `UUID` is a non-`IO` type, and you can make the
conversion, to get a `UUID` involves `IO`.

##### Lifting over web app monads

Frequently for web applications, you will have a custom datatype to describe the application,
and it will be a `Monad` because you application context will have a type parameter describing
the result of running the app. An example framework is [`snap`](http://snapframework.com/),
where it is conventional to have a `type AppHandler = Handler App App`, which is a functor.
When you write an application using this, you're basically responsible for providing functions
that will be fun when a request happens, and leaving actually getting those requests up to
somebody else (the framework / server).

#### 19.4 Applicative

`Applicative` is actually somewhat new in Haskell (a [2014 proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal))
, but they are found in and around parsers.

##### hgrev

`hgrev` uses `Monoid` and `Applicative` to combine parsers of command line arguments.
It also demonstrates use of a helper method in `Applicative`,
`(<*) :: Applicative f => f a -> f b -> f a`, which lets you sequence operations but forget the
second argument (so, apply it to cause side-effects, but discard the result). This is similar
to the monadic `>>`, and also has a "forget the first one" version, `*>`.

##### More parsing

We already saw in the `Applicative` chapter how applicatives are useful for input validation,
a variant of which is converting JSON payloads into structured objects of a particular type.

##### And now for something different

It's useful to `liftA2` to lift an function of two arguments into an applicative context.
For example, `(<||>) = liftA2 (||)` lifts boolean or into an applicative context `a -> Bool`,
so that it can be applied to two functions that take an `a` and produce a `Bool`, where the
result is a function that takes an argument, gives it to both functions, and `||`s the results.

#### 19.5 Monad

Since effects are required to be in `IO` in Haskell, and `IO` is a `Monad`, monads show up
everywhere in Haskell.

##### Opening a network socket

The `network` package, and `haproxy-haskell` library, demonstrate opening a web socket at
an address, and returning a handle so you can read and write from/to it.

##### Binding over failure in initialization

[`Seraph`]() is a process monitor, has a `main`, which is an outermost `IO`. However, it uses
`Either` (specifically a monad transformer `EitherT`) to bind over the possibility of failure
in constructing an initialization function, which might happen if a configuration is invalid.
We'll return to monad transformers in a later chapter.

#### 19.6 An end-to-end example: URL shortener

We build up this example in [shawty](shawty), instead of cloning the
[existing one](https://github.com/bitemyapp/shawty-prime).

##### Brief aside about polymorphic literals

We have seen `Num` as a typeclass before, it relies on `fromInteger :: Integer -> a`. Similarly,
`Data.String` provides `IsString`, relying on `fromString :: String -> a`. When you have a
polymorphic value (e.g., a `Num a => a`, the result of `fromInteger`), you can then restrict it
to a specific type with `::`. For example, `(fromInteger (1 :: Integer)) :: Int` is `1 :: Int`.

### Meetup topic seeds

1. Do you all find the example on p.1230 "much cleaner and more readable"? What's it actually doing,
    and what might we compare it to, to evaluate cleanliness and readability?
2. The `R.defaultConnectInfo` bit of the url shortener app leaves a little to be desired. What's
    a good way to inject config into my app? Same goes for the server port.

