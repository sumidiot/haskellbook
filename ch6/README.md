## Chapter 6: Typeclasses

### My Reading Notes

#### 6.1 Typeclasses

#### 6.2 What are typeclasses?

A typeclass defines how a set of types are consumed.
They are _like_ interfaces.
Wadler says, roughly, they allow you to define a datatype by cases,
so you can add new cases and new functions on the datatype, without recompiling existing code,
and with static type safety (no casts).

##### 6.3 Back to Bool

`:info Bool` shows the typeclass instances for `Bool`, for example, `instance Eq Bool`.

Various typeclasses `Bool` implements:
* `Bounded` - has upper and lower bound
* `Enum` - can be enumerated
* `Eq` - can be tested for equality
* `Ord` - can be put in sequential order
* `Read` - can be read from string (WARN: don't use it)
* `Show` - can be rendered to string

[Hoogle](https://www.haskell.org/hoogle/) is a search engine for Haskell APIs. It lets you find
functions by their type signature, which is kind of awesome.

##### 6.4 Eq

Some data types do not have a sensible notion of equality. In particular, function types don't
have instances of `Eq` (maybe we'll learn more about this later).

`:i Eq` shows the `class Eq` (what methods are associated with it) as well as instances of `Eq`
(e.g., `Bool`).

Some typeclasses can be magically (automatically) derived (with some caveats):
`Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`.

##### 6.5 Writing typeclass instances

The docs for `Eq` say that a minimal definition is _either_ `==` or `/=`. So you only have
to define one, and you'll get the other "for free". That said, you can write both if you
really want to (e.g., if your type supports something clever).

Put typeclass instances for a type in the same file as the type.

`instance T C where (required methods of typeclass)` defines the class `C` as being in
the typeclass `T`.

    data TrivialC = TrivialV -- not that this is equivalent to the () type
    instance Eq TrivialC where TrivialV == TrivialV = True

Typeclass instances are unique for a given type. You can't define `Eq`uality two ways
for one type.

If you miss defining a case in a function, at runtime you'll get an exception like
"Non-exhaustive patternrs in function". You can get warnings about this at compile-time though,
with the `-Wall` flag (`:set -Wall` in the REPL).

If your type has a type parameter (e.g., `data Identity a = Id a`), and you need to know
something about the type (e.g., it is in the `Eq` typeclass), you can add it to your `instance`
signature:

    instance Eq a => Eq (Identity a) where (==) (Id v) (Id v') = v == v'

Haskell then checks that you don't try `Id x == Id y` when `x` and `y` are in some type
that isn't in the `Eq` typeclass.

##### 6.6 Num

The `Num` typeclass expects `+`, `-`, `*`, `negate`, `abs`, `signum`, and `fromInteger`
of its members. Here, `signum` returns -1, 0, or 1 if the number is negative, 0, or positive,
respectively.

`Integral` extends the `Real` and `Enum` typeclasses with the `div` family of functions.
Note that `Real` extends `Num`, but cannot override the methods of `Num` (there's only
ever one implementation allowed for evidence that a class belongs to a typeclass), so
you avoid "diamond of death" multiple inheritance issues.

`Num` also contains the typeclass `Fractional` which provides `/`, `recip`, and `fromRational`.

_Commentary_:
It seems like we could provide evidence that the non-zero elements of a finite field
are in the `Fractional` typeclass. However, to show they are in `Num` we need to have 0,
but in that case we can't implement Fractional. Or, rather, we can in the same way the
other numeric types probably do, which is to throw some exception for `reciprocal 0`.
I don't think we know enough yet for how to say 0-4 are the elements of F_5, besides making up
something like `data F5 = Zero | One | Two | Three | Four`. We _could_ then define the
methods for `Num` and `Fractional`, but they're cumbersome (25 cases for each binary function).

##### 6.7 Type-defaulting typeclasses

For polymorphic values, like `1 :: Num a => a`, the typeclass will default to a concrete type
when necessary. For example, `default Num Integer`.

Types can be made more specific, but not more general.

##### 6.8 Ord

The `Ord` typeclass describes things that can be put in order, with the relations like
`<`, `<=`, `>`, and `>=`. It also requires `compare :: a -> a -> Ordering` and the binary
`max` and `min` functions. Technically, only `compare` or `<=` are required to define an
instance (`Ord` extends `Eq`). Note that `data Ordering = LT | EQ | GT`.

If you define a datatype with something like `data X = A | B | C | D | E`, you can
automatically derive `Ord`, and the compiler uses the order of the data definition.
The compiler can also automatically derive `Eq` and `Show` for such types. However,
per the example in the book, you can provide your own implementation if you want to
do something else (e.g., `C` is the best of all the above, everything else is lesser,
and `EQ` under `compare` (though not `(==)`)).

##### 6.9 Enum

`Enum` covers this which are enumerable, meaning they have predecessors and successors.
The typeclass also requires conversion to/from `Int`, and creation of lists after/up-to
elements.

The `enumFromThen` method takes the first 2 instances of a list, and produces the rest.
For example, `take 5 (enumFromThen 10 6)` returns `[10,16,22,28,34]`. If you don't use
the methods with a `To` on the end, you might get infinite/long lists, so remember to `take`.

##### 6.10 Show

`Show` covers types which can produce human-readable string representations of structure data.
The `show :: a -> String` method is intuitive. The `showsPrec` method does something
with precedence, and `showList` allows you to override the default for lists.

In the REPL, "P" phase uses `print :: Show a => a -> IO ()` to do its job. So if the
result of any line you enter isn't in the `Show` typeclass, you get an error. The result
of `print` is an IO action that returns a value of type `()` (there's only one such value,
`()`). Note that `main` also has the `IO ()` return type.

`IO String` is a "means of producing" a `String` which probably has (IO) side-effects.

_Commentary_:
I tried playing with `main` and `IO String` a little in [ioString.hs](ioString.hs).

##### 6.11 Read

`Read` is effectively the opposite of `Show`, but the authors really don't like it,
at least somewhat because `read :: Read a => String -> a` is regularly destined to fail.
for example, `(read "3.4") :: Int` (though the error here is somewhat confusing,
"*** Exception: Prelude.read: no parse"). Note that, interestingly, `read` is not
a method of the `Read` typeclass, which only has `readsPrec` and `readList`.
The `Read` typeclass is in `GHC.Read`, while `read` is in `Text.Read`.

##### 6.12 Instances are dispatched by type

A typeclass defines the set of operations and values all instances will provide.
An instance is a unique pairing of a typeclass and a type.

It is glossed over, but `newtype` is another way to define a type, similar to `data`.
For example, `newtype Year = Y Int` means that `Year`
is a type, and you can construct one with something like `Y 0`.

One of the examples, just to play, has a typeclass that provides a default value.
The book recommends never _actually_ having a typeclass that provides a default value.
They don't really say why though, besides "we'll get to it with Monoids".

##### 6.13 Gimme more operations

The point of this section is basically: if you try to use an operation and it comes
from some typeclass, then that typeclass has to be a constraint on your function.

You don't need to specify the typeclass constraints for a concrete type in your
function. However, instead of using concrete types, there is some benefit to using
constrained polymorphism, because you're sort of specifying what you're using about
the parameters better. A method `Int -> Int` can do lots of things, coming from any
of the typeclasses `Int` has instances of (e.g., `Ord`, `Eq`, `Num`). Instead, a
method `Num a => a -> a` isn't going to be using the comparisons from `Ord`, just
the operations from `Num` (addition, subtraction, multiplication).

### Exercises

#### 6.5

[Eq Instances](sec6_5.hs)

#### 6.6

Given the types of `quotRem :: a -> a -> (a, a)` and `divMod :: a -> a -> (a, a)`,
I'd expect them to be the products of the functions `quot` and `rem`, `div` and `mod`,
respectively.

#### 6.8

1. This works, and returns 5
2. This works, and returns `LT`
3. This fails, because it is trying to `compare` things of different types
4. This works, and returns `False`

#### Chapter Exercises

Multiple choice

1. c
2. a, b
3. a
4. c
5. a

[Does it typecheck?](chExDiT.hs)

_Commentary_:
[The link](https://wiki.haskell.org/Show_instance_for_functions)
for why there's no `Show` for functions was a good read.

Given a datatype declaration, what can we do?

1. No, you need to wrap it, `Papu (Rocks "chases") (Yeah True)`
2. Typechecks
3. Typechecks
4. Fails, you need to derive `Ord` for `Papu`. Note that to automatically derive `Ord`,
    you have to have an ordering of `Rocks` and `Yeah` also.

### Meetup topic seeds

1. What happens if you sort a list of `a` where the `Ord a` instance compares everything `LT`?
    What if everything compares `GT`?
    Note, `sort :: Ord a => [a] -> [a]` is in `Data.List`, so you have to `import` that first.
2. In [scopeExperiments](scopeExperiments) I play with how to separate a type and typeclass from
    evidence of the type being in the typeclass. Naturally, this allows multiple evidences, as
    long as you separate scopes. You are recommended _not_ to do so, I guess it's called an
    "orphaned instance". Instead, you're supposed to provide all evidences in the same module
    as the type definition, and it seems like then you're losing the power of the typeclass
    system, which is that the type shouldn't have to know about the typeclasses it implements.
    I read a little about this [here](https://pchiusano.github.io/2018-02-13/typeclasses.html).
