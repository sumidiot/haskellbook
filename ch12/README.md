## Chapter 12: Signaling adversity

### My Reading Notes

#### 12.1 Signaling adversity

There are standard datatypes for indicating when something has gone wrong during program execution.
They are `Maybe` and `Either`.

#### 12.2 How I learned to stop worrying and love Nothing

`data Maybe a = Nothing | Just a`

You use `Maybe` when there's a chance you won't be able to compute an `a` and so might have to return
`Nothing`.

A use case for `Maybe` is "smart constructors". You can represent `data Person = Person Name Age`, but
it wouldn't require a positive age (`type Age = Integer`). So you create a
`mkPerson :: Name -> Age -> Maybe Person` that checks the input arguments for validity and returns
a `Just Person` or `Nothing`.

You can say _what_ went wrong with a `Maybe`, just that something did.

#### 12.3 Bleating either

`data Either a b = Left a | Right b`

If you have two error types, like `data PersonInvalid = NameEmpty | AgeTooLow`, you'd set up
`mkPerson :: Name -> Age -> Either PersonInvalid Person`, so that on failure you could return
the reason for the failure (as the `Left PersonInvalid`) or just return the person (as
`Right Person`).

It is conventional in Haskell to use the `Left` of an `Either` for the error case. In particular,
you can `fmap` over an `Either`, and it maps the `Right`.

Sometimes you want to return a list of failures, all the reasons a thing was wrong. In this 
chapter we see a not-particularly-elegant way, with promises of `liftA2` cleaning things up
in a later chapter.

#### 12.4 Kinds, a thousand stars in your types

Kinds are types one level up. They describe the types of type constructors. Type constructors
are types that take more types as arguments. A "type constant" is a type that takes no
arguments, while a "type constructor" is a type which must have arguments applied to become
a type.

`:k` gives the kind of a thing in GHCi, similar to `:t` for type

`Int` and `Bool`, for example, are type constants, with kind `*`. `(,)` has kind `* -> * -> *`
because you have to provide two types, and once you do you get a third (the tuple type).

To be precise, `*` represents a **lifted** type, meaning a type with _bottom_. **Unlifted
types** do not include _bottom_, and are represented with kind `#`. These are often machine
types and raw pointers. `newtype`s are interesting here, because they're represented as kind
`*`, but are unlifted because the newtype doesn't have a bottom (just the contained type does).

Apparently higher-kindedness will show up more notably in a later chapter.

Data constructors that take arguments are basically functions. For example, you can do things
like `fmap Just [1,2,3]`.

You generally can't hide polymorphic types from your type constructor, so you can't do
`data T = D a` (you have to do `data T a = D a`). Apparently there's ways around it, and
probably reasons to do so.

#### 12.5 Chapter Exercises

##### Determine the kinds

1. In `id :: a -> a`, the kind of `a` is `*`
2. In `r :: a -> f a`, the kind of `a` is `*`, and of `f` is `* -> `

##### [String processing](chEx-StringProcessing.hs)

##### [Validate the word](chEx-ValidateTheWord.hs)

### Meetup topic seeds

1. The book shows examples of how `:k Maybe a` is ok for some `a`s (e.g., `Int`, `(Maybe Int)`),
    but not ok with something like `:k Maybe Maybe`. To me, that looks like we should be able
    to try `:k Maybe $ Maybe`, which of course we can _try_, but it fails, because you apparently
    can't apply operators to types, unless you add the `TypeOperators` pragma.
2. What's the use case for `data T = D a`? How do you make it work? Note that it's different
    from a [Phantom Type](https://wiki.haskell.org/Phantom_type), which is where a type-level
    parameter doesn't show up in any data constructors, like `data T a = D`.
3. What do we think the answer is for the 'determine the kind' chapter exercises?
4. In chEx-StringProcessing.hs, I found a little bit of a surprise with guards. The following
    has an issue:
    ```
    go w s@(c:cs) r
      | s == ""   = r ++ (checkWord w)
      | isAlpha c = go (w++[c]) cs r
      | otherwise = go "" cs (r ++ (checkWord w) ++ [c])
    checkWord "the" = "a"
    checkWord s = s

    ```
