## Chapter 14: Testing

### My Reading Notes

#### 14.1 Testing

This chapter, like the last, is more practically about _using_ Haskell, than about the language.

Types + Tests = Awesome!

We'll cover `Hspec` and `QuickCheck` in this chapter.

#### 14.2 A quick tour of testing for the uninitiated

The haskell compiler, and the type system, prevent a lot of errors, but there's still plenty of
room for errors, and tests are a way to cut that down further.

_Unit_ testing tests small atoms independently of one another, allowing the programmer to check
each of a collection of functions in isolation. _Spec_ or _property_ testing is a form of unit
testing, where specifications for how a function should work are represented as fairly human-readable
assertions. `hspec` will be a focus for this chapter, but `HUnit` is also available.

_Property_ testing was pioneered in Haskell. It tests formal properties of programs without
requiring formal proofs. We'll see how to use `QuickCheck` to generated random inputs, which is
feasible because of the type system. It is designed to choose edge cases where possible, but
won't generate all possible inputs.

#### 14.3 Conventional testing

Let's begin with `hspec`. From a skeletal cabal file, since source file with the `library`'s
`exposed-modules`, and a `touch`ed `LICENSE` file, `stack init` will get you started, and then
you should be able to `stack build`.

`:browse (module)` (e.g., `:browse Test.Hspec`) in a REPL (e.g., `stack ghci`) shows the things
in the module (e.g., function signatures).

```
hspec $ do
  describe "Module/section" $ do
    it "test" $ do
      (test) -- e.g., 1 > 0 `shouldBe` True
    it "test2" $ do
      (another test) -- e.g., 2 > 1 `shouldBe` True
```

#### 14.4 Enter QuickCheck

If you `import Test.QuickCheck` (and `QuickCheck` is in `build-dependencies`), you can add a test
like
```
    it "test3" $ do
      property $ \x -> x + 1 > (x :: Int)
```

`QuickCheck` provides the `Arbitrary` typeclass, and a `newtype Gen`, along with `arbitrary`, a function
of type `Arbitrary a => Gen a`. It also provides `sample` (type `Show a => Gen a -> IO ()`) and
`sample'` (type `Gen a -> IO [a]`). These rely on a global pseudorandom number generator, which is why
they involve `IO`. As an example, `sample (arbitrary :: Gen Int)` prints out a handful of integers.

`return x :: Gen T` (e.g., `return 5 :: Gen Int`) converts a raw `T`, in this case called `x`, to a
`Gen T`. You can then `sample` from it. Another way to get a `Gen T` is `elements`, with type
`[a] -> Gen a`. Giving `elements` a list with duplicated values makes those values more likely to
be selected by `sample`. `choose` has type `(a, a) => Gen a` and is another good thing to `sample`
from.

`Maybe` and `Either` come with `Arbitrary` instances, so you can generate them, in addition to
things like `Int`, `Char`, `Bool`, and `[]`.

You can use `frequency` to make a `Gen` with specified frequencies. It's type is `[(Int, Gen a)] -> Gen a`

You can use `QuickCheck` without `hspec` with the `quickCheck` function, with type
`Testable prop => prop -> IO ()`.

`QuickCheck` shows the value that causes a test failure.

#### 14.5 Morse code

`stack new project-name` makes a project template, with a `Setup.hs` and `.cabal` file (among others).

For now, think of `Data.Map` as a balanced binary tree, where each node is a key and value (the value
must have an `Ord` instance). `Data.Map.fromList` is a useful function for getting a `Map` from a
list of pairs, its type is `Ord k => [(k, v)] -> Map k v`.

Useful `Data.Map` functions:

* `lookup :: Ord k => v -> Map k v -> Maybe v`
* `foldWithKey :: Ord k => (k -> a -> b -> b) -> b -> Map k a -> b`
* `insert :: Ord k => k -> v -> Map k v -> Map k v`
* `empty :: Ord k => Map k v`

`stack exec which project-name` shows the path of the executable that stack builds.

`stack ghci morse:tests` launches a REPL for the tests, you can then run `main` to see if tests pass.

`forAll`, from `QuickCheck` is a helpful way to set up tests, its type is
`(Testable prop, Show a) => Gen a -> (a -> prop) -> Property`.

#### 14.6 Arbitrary instances

An important part of working with `QuickCheck` is making `Arbitrary` instances for your datatypes.

To launch a `stack ghci` with some extra package(s) without a `.cabal` file, use `stack ghci --package name`.

`Gen` values are generators of random values.

`oneof` and `frequency` let you create `Gen` values from a list of `Gen`s.

`CoArbitrary` is the dual to `Arbitrary`, in that it lets you generate functions of a type. While
`Arbitrary` requires `arbitrary :: Arbitrary a => Gen a`, `CoArbitrary` requires
`coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b`. You give it an `a` as a way to modify a `Gen`.

There's a `GHC.Generics` that provides the `Generic` typeclass, and if you have that, you can instances
of `CoArbitrary` for free (maybe `Arbitrary` too?) (with the `DeriveGeneric` pragma).

#### 14.7 Chapter Exercises

##### [Validating numbers into words](chEx-vnumWord.hs)

##### [Using QuickCheck](chEx-useQC.hs)


### Meetup topic seeds

1. Development environment? atom? vim?
2. Quick way to `QuickCheck` that two functions are the same? `quickCheck (\x -> f x == g x)`
