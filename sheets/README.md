## Cheatsheets

This directory contains some cheatsheets for useful functions and typeclasses and things.
For now, they're all just in this file.

### Prelude functions

* `id`, the identity function
* `div`, `quot`, `rem`, `mod`, for numeric divisors and remainders and such
* `print :: Show a => a -> IO ()`, leaves quotes around strings `a` is `String`
* `putStrLn :: String -> IO ()`
* `putStr :: String -> IO ()`, like `putStrLn` without newline
* `concat :: [[a]] -> [a]`, or more recently/generally, `Foldable t => t [a] -> [a]`
* `head :: [a] -> a`, `tail :: [a] -> [a]`
* `take :: Int -> [a] -> [a]`, `drop :: Int -> [a] -> [a]`
* `takeWhile :: (a -> Bool) -> [a] -> [a]` and `dropWhile :: (a -> Bool) -> [a] -> [a]`
* `fst` and `snd` extract components of a tuple
* `length :: [a] -> Int`
* `reverse :: [a] -> [a]`
* `elem :: Eq a => a -> [a] -> Bool`
* `flip :: (a -> b -> c) -> b -> a -> c`
* `map :: (a -> b) -> [a] -> [b]`
* `filter :: (a -> Bool) -> [a] -> [a]`
* `zip :: [a] -> [b] -> [(a, b)]` and `unzip :: [(a, b)] -> ([a], [b])`
* `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`
* `foldr :: (a -> b -> b) -> b -> [a] -> b`, in older versions of GHC, see `Foldable` below
* `foldl :: (b -> a -> b) -> b -> [a] -> b`
* `scanr` and `scanl` are like `foldr` and `foldl` except return `[b]`, the list of intermediate results

### Additional useful functions

* `Data.List.nub` removes duplicates from a list
* `Data.List.intersperse` puts a value between all values in a list. e.g., `intersperse ' ' "word" == "w o r d"`.

### Prelude operators

| Operator | Context | Type                               | Purpose |
| -------- | ------- | ---------------------------------- | ------- |
| `++`     | `List`  | `[a] -> [a] -> [a]`                | Concatenate lists, recall `String` is `[Char]` |
| `:`      | `List`  | `a -> [a] -> [a]`                  | "cons", create a list                          |
| `!!`     | `List`  | `[a] -> Int -> a`                  | Retrieve element of list based on index |
| `==`     | `Eq`    | `a -> a -> Bool`                   | Equality checking    |
| `/=`     | `Eq`    | `a -> a -> Bool`                   | In-equality checking |
| `.`      | `->`    | `(b -> c) -> (a -> b) -> (a -> c)` | Function composition |


### Typeclasses

* `Num`, giving `+` and `-`
* `Fractional`, giving `/`
* `Bounded` has `minBound` and `maxBound`
* `Eq`, giving `==` and `/=`
* `Ord`, giving `>`, `<`, etc, and `compare` returning `Ordering`, `LT`, `EQ`, or `GT`.
* `Enum` is for things that are enumerable, meaning they have predecessors and successors.
* `Show` is for things that can be converted to strings, via `show :: a -> String`


### syntax

* Anonymous function: `\x -> x + 1`
* The type of a function is `A -> B`. All functions take one argument, but that may then return
    another function, etc. `f a b c d` applies `f` to 4 arguments.
* `$` is function application, just associates to the right
* You can partially apply an infix binary operator as a "slice" with, e.g., `(+30)`
* `(,)` creates a tuple, can be multiple components, not just two
* `[]` creates list instances, an indeterminate length
* `[a..b]` creates a list of values from `a` to `b`.
* `[a,b..c]` creates a list of values from `a` to `b` with step size `(b-a)`
* `f :: (X) => Y` shows the type of a thing with typeclass constraints `X` and type `Y`
* `data T p1 p2 ... = D1 q11 ... | D2 q21 ... | ...` defines a type constructor `T` with parameters
    `p1...`, and data constructors `D1...` with parameters.
* `newtype T = D a` defines a newtype, it like `data` but can only have one constructor and one field
* `deriving (C1,C2,...)` can be added to a `data` or `newtype` declaration for some automatic
    typeclass derivations (normally things like `Eq`, `Show`, `Ord`).
* `class C a where ...` defines a typeclass `C`
* `instance C T where ...` shows how the type `T` is an instance of typeclass `C`
* `let a=b in ...`
* case matching looks like
    ```
    case v of
      value1 -> result1
      value2 -> result2
    ```
* guards are similar to cases but just different enough. The following defines `min`
    ```
    f :: Ord a => a -> a -> a
    f x y
      | x < y = x
      | x >= y = y
    ```
* `[ expr | symb <- list]` is a list comprehension. you can do `[e | s1 <- l1, s2 <- l2 ...]` to
    combine multiple lists, or filter elements with `[e | s1 <- l1, p s1]` where `p` is a function taking
    the `s1` to a boolean
* "Record syntax" allows you to define a type like
    ```
    data T = T { f1 :: A1, f2 :: A2, ... }
    ```
    where, in some sense, the `f`s are fields of types `A`s. This syntax gives you automatic functions
    `fi :: T -> Ai`, which is sometimes more convenient than pattern matching an algebraic type. You
    still construct a `T` as usual, e.g., `T f1 f2` (but if you want to change the order, or name the
    arguments, you can: `T { f2 = f2, f1 = f1 }`)
* "As patterns" let you match an expression but also capture the entire match. So, as a silly example,
    ```
    f :: (Int, Int) -> ((Int, Int), Int)
    f p@(a,b) -> (p, a+b)
    ```
* `module M (e1, e2, ...) where` defines a module that exports the symbols `ei`. It occurs at the top
    of a file
* `import M`, `import qualified M`, and `import qualified M as N` import a module. In the first case
    you can refer to its exports directly. In the second, you must prefix them with `M.` (but is handy
    if there are name collisions). In the third, you prefix them with `N.`, which is handy if `M` is
    a long string.


### ghci

#### Patterns

To create a value, you have to use a let expression, `let x = 1`. You can separate the type
declaration from the definition, on one line, with a semicolon.

You can enter multi-line blocks with `:{` and `:}`.

#### Commands

You can enter just the first letter of a command if it is not ambiguous

* `:load <file.hs>` -- load this module, changes prompt from `Prelude>` to maybe `*Main>`
* `:module` -- "quit" the loaded module, back to `Prelude>`
* `:module *Main` -- jump back to the `*Main` module
* `:reload` -- reload current module (e.g., re-read file)
* `:type` and `:kind` -- show... type... and kind
* `:info` -- type information for function (including infix, associativity, precedence) and type
* `:kind` shows the kind of a type
* `:browse` lets you see the functions of a module

#### Pragmas

Compiler extensions, added at the top of a source with with `{-# LANGUAGE PragmaName #-}`, where
`PragmaName` is one of the following (or many others we won't get to):

* `GeneralizedNewtypeDeriving`, allows you to add `deriving T` to a `newtype` declaration to automatically
    derive that the `newtype` is in the `T` typeclass, as long as the wrapped type is.


### stack

* `stack new (name) simple` will create a new simple template project called `name`
* `stack build` builds a project
* `stack ghci` gives you a ghci REPL with your project in scope (it's `Main` anyway)
* `stack exec` runs the `Main` executable of your project
