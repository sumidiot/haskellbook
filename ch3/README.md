## Chapter 3

### Reading Notes

ghci `:type` to get the type of a things

focus in this chapter is `Char` and `String = List[Char]` types

`::` is read as "has the type"

* `print` leaves quotes on the output
    `:type print` shows `Show a => a -> IO ()` ("if `a` is `Show`able")
* `putStrLn` prints a string, stripping quotes.
    `:type putStrLn` shows `String -> IO ()`
* `putStr` like `putStrLn`, but doesn't add a newline

fun! You can add things to run on startup in your `~/.ghci` file.
In particular, the following is entertaining:

    :set prompt "λ> "

`do` notation allows for sequencing actions

`++` allows for string concatenation.

things declared in where clauses in a file aren't visible when you load the module.
The order of top-level declarations doesn't matter (generally).

`++` concatenates, as does `concat`. `:type (++)` is `[a] -> [a] -> [a]`, while
`:t concat` is `[[a]] -> [a]`. Since `String = [Char]`, `concat` on a list of strings
is reasonable. (ghc 7.10+ has `:t concat` as `Foldable t => t [a]`, which is fun!)

The type `[a]` means a list with elements of some type `a` we do not yet know.

List operations
* `(:)`, called _cons_, lets you add a thing to the front of a list. `:t (:)` is
    `a -> [a] -> [a]`
* The element at the front of a list can be retrieved with `head`, whose complement
    is `tail` (that is, `tail $ cons _ == id`)
* `take` is like `head` but lets you specify how many elements to take (`head == take 1`).
    `drop` is the complement. `(take n x) ++ (drop n x) == x`
* `(!!)` is similar to `[]` array access in "normal" languages. `:t (!!)` is
    `[a] -> Int -> a`. `head == (!!) _ 0`

Many of these operations are "unsafe" because they can throw exceptions. e.g.,
`head ""` or `"" !! 4`.

### Exercises

#### Scope

1. Yes
2. No
3. No
4. Yes

#### Syntax Errors

1. No. `(++) [1, 2, 3] [4, 5, 6`
2. No. "<3" ++  Haskell"
3. Yes

#### Chapter Exercises

##### Reading syntax

1.
    1. ok
    2. Nope. Need `++` infix, or `(++)`
    3. ok
    4. ok
    5. Nope. Reverse the arguments
    6. ok
    7. Nope, needs int
    8. ok
2. Note that you can get 3/5 of these just by thinking about the types
    1. d
    2. c
    3. e
    4. a
    5. b

##### Building functions

1.
    1. `"Curry is awesome" ++ "!"`
    2. `"Curry is awesome" !! 4`
    3. `drop 9 "Curry is awesome"`

Most of the others are [here](ChapterExercises.hs), final one [here](Reverse.hs)

### Meetup seeds

1. Any clean answers to `rvrs`?
2. Notes of String performance? [Char] vs "native" String?
