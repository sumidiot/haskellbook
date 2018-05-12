## Chapter 11: Algebraic datatypes

### My Reading Notes

#### 11.1 Algebraic datatypes

The goal is to see how we can make our own types, and what it means to say a type is "algebraic."

"A type can be thought of as an enumeration of constructors that have zero or more arguments."

In this chapter, we'll learn about sum and product types, record syntax, type aliases, and `newtype`.
We'll also be introduced to "kinds".

#### 11.2 Data delarations review

Datatype declarations begin with the `data` keyword, then the type constructor (optionally with
arguments), then a data constructor (possibly `|`-delimited for sum types, or self-referential
for recursive types).

#### 11.3 Data and type constructors

Type constructors are used only at the type level. They are static and resolve at compile time.
Data constructors constuct values at the term level that you can interact with at runtime.

Type and data constructors that take no arguments are constants. For example, `Bool` is a type
constant, and `True` and `False` are constant values.

There's a "phantom" type argument to type/data constructors, which maybe we'll read more about later
(yep, section 11.5).

#### 11.4 Type constructors and kinds

Kinds are the types of types, or types one level up. Kinds are represented by `*`. A fully
applied, concrete type has kind `*`. A type construct with a parameter represents a way to produce
a type given another, so has kind `* -> *`.

`:k`, in GHCi, shows the kind of a type constructor (where `:t` shows the type of a value).

The kind of `[]` is `* -> *`, because it expects another type. Same for `Maybe`. `Either` actually
takes two type parameters, so has kind `* -> * -> *`. (This cheats, pulling a little from Chapter 12).

#### 11.5 Data constructors and values

In `data T = D`, `T` is a type constructor, but since it has no arguments, we also refer to it
as a type "constant". `D` is a data constructor, and since it takes no arguments is also a
"constant value".

In `data T a = D`, `T` is a type constructor. `D` is again a constant value.

In `data T a = D a`, `T` is a type constructor and `D` is a data constructor. It is not uncommon
for the type constructor and data constructor to have the same name, `data T a = T a`.

##### Exercises

1. `Doggies` is a type constructor
2. `:k Doggies` is `* -> *`
3. `:k Doggies String` is `*`
4. `:t Husky 10` is `Num a => Husky a`
5. `:t Husky (10 :: Integer)` is `Husky Integer`
6. `:t Mastiff "Scooby Doo"` is `Mastiff String`
7. `DogueDeBordeaux` is both a type and data constructor
8. `:t DogueDeBordeaux` is `a -> DogueDeBordeaux a`
9. `:t DogueDeBordeaux "doggie!"` is `DogueDeBordeaux String`

So, exercise 8 finally made me realize a fun thing: data constructors are functions. Like,
we've done things like `map (+1) [1..5]`, but we can also `map DogueDeBordeaux [1..5]`.

#### 11.6 What's a type and what's data?

Types are static and resolve at compile time. Information about types does not persist through to runtime.

In standard Haskell we can't choose specific values of types as type arguments. Like, I can't do
`data T = D 10`, I have to do `data T = D Int`. However, if you try the first, GHCi hints that
you might be able to do something entertaining with `DataKinds`.

#### 11.7 Data constructor arities

"Arity" is the number of arguments a function or constructor takes. "nullary" means no arguments,
an example is the data constructors (constant values) `True` or `False`. "unary" means one argument.
Data constructors that take more than one argument are called "products".

Tuples are the canonical product type. They are "anonymous" products because they have no name
(the components don't).

#### 11.8 What makes these datatypes algebraic?

We can descibe the pattern of argument structures with two basic operations: sums and products.
Sum types are constructed by the `|` in a data constructor, product types are constructed, as
we saw in the last section, by data constructors that take 2 or more arguments.

A way to convince yourself of the analogy is to think of cardinality, the number of elements.
The cardinality of a datatype is the number of values it defines (this may be infinite).

`Bool` has two inhabitants, `True` and `False`, and is a sum type because of the `|`,
`data Bool = True | False`.

If you `import Data.Int` you get the `Int8` type which consists of 8-bit integers, in the range
[-128,127]. This type has cardinality 256 (2^8).

Data types with a unary constructor always have the same cardinality as the type they contain.

##### Exercises: Cardinality

1. `data PugType = PugData` has cardinality 1
2. the `Airline` type of the earlier section has cardinality 3
3. `Int16` has cardinality 2^16, 65536
4. `Int` has cardinality 2^64, `Integer` is infinite
5. 2^8 = 256

##### Exercises: For Example

1. `:t MakeExample` is `Example`. `:t Example` gives an error.
2. Yes, you can tell the typeclass instances with `:info`
3. `:t MakeExample` is `Int -> Example`

#### 11.9 newtype

`newtype` allows you to construct a new type that can only ever have a single unary data constructor.
It cannot be a sum or product or nullary. However, it has no runtime overhead, because the difference
between the `newtype` and the type it contains is eradicated by the compiler.

Sometimes sum types are called "tagged unions", and product types "record" types.

The first example is a `tooManyGoats :: Int -> Bool`. If you wanted a similar `tooManyCows :: Int -> Bool`
function, there's nothing to guard against you passing the number of cows to the `tooManyGoats`
function. If, instead, you `newtype Goats = Goats Int` then `tooManyGoats :: Goats -> Bool`,
then you can't ask if there are too many cows (assuming another `newtype` for those) via the
`tooManyGoats` function.

A `newtype` is similar to a type synonym, in that both are eliminated by the compiler. The difference is
that you can define typeclass instances for `newtype`s that are different from typeclass instances for the
underlying type, and you can't do that for type synonyms.

Going the other way, you might want your `newtype` to inheric typeclass instances from the
underlying type. To do this, use the `GeneralizedNewtypeDeriving` language extension by adding
the following "pragma" to the top of your source file: `{-# LANGUAGE GeneralizedNewtypeDeriving #-}`.
Then, for your `newtype`, you can say `deriving T` where `T` is a typeclass for there there is
an instance for the underlying type of the `newtype`.

##### Exercises: [Logic Goats](s11_9.hs)

#### 11.10 Sum types

A type defined with `|` is a sum type. It has cardinality equal to the sum of the components.
They are a way of expressing alternate possibilities within a single datatype.

##### Exercises: Pity the Bool

1. The cardinality of the type `data BigSmall = Big Bool | Small Bool` is 4, because
    there are 2 values for `Big` (`Big False` and `Big True`) and similarly 2 for `Small`.
2. The cardinality of `data NumberOrBool = Numba Int8 | BoolyBool Bool` is 258,
    256 for the `Numba` values and 2 for the `BoolyBool`.

In the type of exercise 2, if you try `Numba (-128)` you get an warning, because the compiler
is worried that `negate 128` involves a value outside the range of `Int8`. You can create a
value for `-128` (e.g., `n = (-128)`), or `:set -XNegativeLiterals`.

#### 11.11 Product types

Product types have cardinality equal to the product of the underlying types. Products represent
_and_ where sums represent _or_. A product is like a `struct` in C-like languages, they're a way
to carry multiple values around in a single constructor. A data constructor with multiple type
arguments is a product.

The canonical example is a tuple.

##### Record syntax

Recaords are product types with convenient field accessors.

If you have a simple product type like `data Person = MkPerson String Int`, then to do
anything with it, you have to pattern match, like `name (MkPerson s _) = s`. To re-write
that with record syntax, it would like like:

    `data Person =
      Person { name :: String , age :: Int }`

Doing this gives you two automatic functions, `name` and `age` with domain Person and the
ranges `String` and `Int`, respectively.

You still construct a record the same way, `Person "name" 13`

#### 11.12 Normal form

Normal algebraic rules for sums and products apply in the system. For example, the
distributive property holds. That is, if you have a type `data T = P A R` (a product type)
where `data A = A` and `data R = B | C`, then roughly `T` is `A * R` or `A * (B + C)`,
which is equivalent to `A * B + A * C`, which would be `data T' = PB A B | PC A C`.

Normal form is when the type is written as a sum of products.

##### Exercises: [How Does Your Garden Grow](s11_12.hs)

#### 11.13 Constructing and deconstructing values

Try avoiding type synonyms with unstructured data like text or binary, because the compiler
can fail to help you. They're best used when you want something lightweight but explicit.

We've already seen a fair amount of how to construct values. Note that for types defined
with record syntax, you can construct it as normal for a product, or you can explicitly
write the fields. Following the `Person` example from above, you can construct a value with
syntax like `Person "name" 13` or `Person { name = "name", age = 13 }`. Using the second
syntax, the order doesn't matter, so you can also write `Person { age = 13, name = "name" }`.

##### Exercise: [Programmers](s11_12_programmer.hs)

`nub` from `Data.List` will remove duplicates from a list.

Note that with record syntax, if you don't specify all the fields in the constructor you
get a warning. The resulting value will actually basically be a bottom, you can't do
much of anything useful with it (I guess?). The book definitely recommends against
making partial records.

#### 11.14 Function type is exponential

The cardinality of the type `a -> b`, functions from `a` to `b`, is `b^a`.

##### Exercises: The Quad

Given `Quad` is a type with 4 inhabitants,

1. `Either Quad Quad` has 4+4=8 inhabitants
2. `(Quad, Quad)` has 4*4=16 inhabitants
3. `Quad -> Quad` has 4^4=256 inhabitants
4. `(Bool, Bool, Bool)` has 2*2*2=8 inhabitants
5. `Bool -> Bool -> Bool` has (2^2)^2=16 inhabitants
6. `Bool -> Quad -> Quad` has (2^4)^4=65536 inhabitants

#### 11.15 Higher-kinded datatypes

Kinds are the types of type constructors. The default kind is `*`. The kind `* -> *` is
waiting for a type, and will produce a new type. Lists are higher-kinded datatypes, because
they take a type, like `Int`, and produce a new type, `[Int]`. Higher-kinded types provide
a generic way to express a "hole" to be filled by a consumer.

For example
`data QueryResult a = QueryResult { _meta :: QueryMetadata, result :: a }`
provides a structure to hold query results of a type you don't know ahead of time. A library
with such a type is likely to want to do something with the query result, which would require
some understanding about the type of `a`. This is expressed with typeclass constraints _on the
function consuming the result_ vs on the `QueryResult` type itself.

#### 11.16 Lists are polymorphic

Operators with non-alphanumeric names are infix by default. Any operator that starts with `:`
must be an infix type or data constructor, and all infix data constructors must start with a colon.
The type constructor `->` is an infix type constructor that doesn't start with a colon. Operators
cannot be `::`, because that is reserved for type assertions.

#### 11.17 Binary Tree

We can define a binary tree type like the list type, recursively: `data Tree a = Leaf | Node (Tree a) a (Tree a)`.
This creates a binary tree type with a value of type `a` at every inner (non-leaf) node.

The book works through defining `insert` that inserts an `a` into a `Tree a`, assuming `Ord a`.
Since Haskell's immutable, this function returns a new tree.

##### [Exercises](s11_17.hs)

### Meetup topic seeds

1. You can make recurisvely defined types, probably as long as you do so within the scope of one module/file.
2. Can you do exercise 2 in 11.6 with case or guard statements?
3. I'm pretty sure I didn't do 11.9 exercises the intended way.
4. I'm also not sure I did the 11.12 exercise the intended way.
5. Can you specify a type with a polymorphic constraint? We can make a function with type `Num a => a -> a`,
    can we make a `Num a => data NumHolder a = NumHolder a`?

