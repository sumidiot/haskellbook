## Meetup 10: Chapter 15 - Monoid, Semigroup

July 23, 2018

Attendees: 5
Current Members: 47 (3 new!). RSVPed: 5

### Discussions

Jon asked about QuickCheck and how much to use `Gen` vs `Arbitrary`, but most of us didn't
put too much energy into those libraries yet.

Ben asked about arbitrarily setting a unit in an algebra. Note that an identity is always
unique in a monoid. Vivek notes that you can "follow" an isomorphism to convert an identity
element (e.g., rename n -> n+1, identity becomes 1). You can always add a new element to any
semigroup, and make it the identity and turn it into a monoid. In particular, you can always
add a brand new identity to any monoid, and the old identity is no longer the identiy.

Jon asked a tangent: what about dividing by 0. Vivek: you have to think about the integers
as a curve. Actually, Integers mod 2 is the same set as `Bool`, and quickly shows two more monoidal
structures, XOR and XNOR, that the book didn't mention. It's the field with 2 elements.
There's this quest for "the field with 1 element" which would sort of require "0 = 1",
the additive and multiplicative identities are the same, and the integers are a curve over
this field. [Wikipedia](https://en.wikipedia.org/wiki/Field_with_one_element)

Computer science interpretation of associativity: it's what you need to parallelize a
computation.

Vivek draws the diagram of `((a b) c) d` re-associating, forming a pentagon. Also draws each
expression as just a tree. For 3 elements `(ab)c=a(bc)` there are two diagrams (nodes) that
are the same, so you get a line. For 4 elements you get a pentagon, and you can keep going,
getting [associahedron](https://en.wikipedia.org/wiki/Associahedron). Ben asked about what
happens when you also incorporate commutativity into the diagrams. Vivek notes that for
3 elements you get a hexagon, and for higher dimensions you get
[permutahedron](https://en.wikipedia.org/wiki/Permutohedron). There's a natural mapping from
the permutahedron to the associahedron, and so hexagons are really just pentagons.


Jon shared his progress on [currybot](https://github.com/cville/currybot). One note: files
much have the same name as the `module` they represent for things to go well. There are
`Text` and `ByteString` types, and the `{-# OverloadedStrings #-}` pragma to automatically
convert hard-coded strings into the appropriate type based on the usage. `ByteString` is
preferred for network-type string-y interactions, but others think of things in terms of
`Text` / unicode. Going back and forth and among the string types is a hassle.
Nick recalls some similar links: [1](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings),
and [2](http://www.alexeyshmalko.com/2015/haskell-string-types/).
Jon used the `Wreq` package for web requests. Also used `aeson` (json), and found he had to add it
to the `cabal` file to get past some issues with hidden members. The `lens` library
provides something like getters and setters, for pulling out "properties of objects"
(if you're coming from an OO world). The `HoogleItem` and `HoogleResult` classes have
`instance`s of `FromJSON` and `ToJSON`, without any body, becase they both derive
[`GHC.Generic`](https://wiki.haskell.org/GHC.Generics) (using `{-# DerivingGeneric #-}`). Jon
found that the `sendMessage` function we were using in the slack library didn't support
useful things like nice formatting, and found that we needed to update the version of the
slack library. Had also banged his head on things like `do let x = putStrLn "abc"; return ()`
being lazy enough that nothing every happens, and that `liftIO`, from `MonadIO`, with type
`IO a -> m a`.

Semigroups that aren't monoids include `First` and `Last`.

Vivek notes that thinking of a type as a set, and a typeclass as a structure on that set,
was useful for him.

We worked through the `Mem s a` monoid exercise. Vivek points out that
`s -> (a, s)` is the same type as `(s -> a, s -> s)`, and that previous exercises showed that
(1) if `a` is a monoid then maps into `a` is a monoid (`Combine`, exercise 6), and
(2) functions from a type to itself is a monoid (via composition, `Comp`, exercise 7),
so really the instance of `Mem s a` is just a pair of prior instances.

Jon pointed out the confusing syntax of `runMem mempty 0 :: (String, Int)`, recalling that
the record syntax takes an extra argument, being the instance to apply to.

Notes from Jon:
* wreq:
    An HTTP client library for Haskell.  See http://www.serpentine.com/wreq/tutorial.html .
* aeson:
    A JSON library for Haskell.  See https://artyom.me/aeson .
* lens:
    A library for Haskell. See http://hackage.haskell.org/package/lens .  I really don't know much about them.
* Overloaded strings: 
    A language extension that does for strings what Num and Fractional do for numeric literals.  See http://tuttlem.github.io/2017/03/21/overloadedstrings-language-pragma.html and http://www.serpentine.com/wreq/tutorial.html#working-with-string-like-types .
* Derived generics:
    A language extension that allows you to take advantage of the category-theory-like structure of types to specify functions that say how, in terms of this structure, to derive new functions over future (to be declared) types.  See https://wiki.haskell.org/GHC.Generics .
* GHC Core Language:
    An output format for GHC and GHCi that shows the underlying translation by the compiler into a typed language.  See http://www.stephendiehl.com/posts/ghc_03.html .
* Memory Leaks:
    Check out http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/ .
* GHC RTS:
    The runtime: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts .
* Typing lambdas:
    From https://www.schoolofhaskell.com/user/commercial/content/monad-transformers :
     > type MyDeeperStack = ReaderT Int (WriterT String (MaybeT (ExceptT String IO))) Bool
     > :t \x -> (liftIO x :: MyDeeperStack)
     > \x -> (liftIO x :: MyDeeperStack) :: IO Bool -> MyDeeperStack

