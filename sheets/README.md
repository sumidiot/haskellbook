## Cheatsheets

This directory contains some cheatsheets for useful functions and typeclasses and things.
For now, they're all just in this file.

### built-in functions

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


### build-in operators

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
* `$` is function application, just associates to the right
* You can partially apply an infix binary operator as a "slice" with, e.g., `(+30)`
* `(,)` creates a tuple, can be multiple components, not just two
* `[]` creates list instances, an indeterminate length
* `[a..b]` creates a list of values from `a` to `b`.
* `[a,b..c]` creates a list of values from `a` to `b` with step size `(b-a)`
* `f :: (X) => Y` shows the type of a thing with typeclass constraints `X` and type `Y`
* `data T p1 p2 ... = D1 q11 ... | D2 q21 ... | ...` defines a type constructor `T` with parameters
    `p1...`, and data constructors `D1...` with parameters.
* `newtype T a = D a` defines a newtype, it like `data` but can only have one constructor and one field
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


### ghci

#### Patterns

To create a value, you have to use a let expression, `let x = 1`. You can separate the type
declaration from the definition, on one line, with a semicolon.

You can enter multi-line blocks with `:{` and `:}`.

#### Commands

You can enter just the first letter of a command if it is not ambiguous

* `:load <file.hs>` -- load this module, changes prompt from Prelude> to maybe *Main>
* `:module` -- "quit" the loaded module, back to Prelude>
* `:module *Main` -- jump back to the *Main module
* `:reload` -- reload current module (e.g., re-read file)
* `:type` and `:kind` -- show... type... and kind
* `:info` -- type information for function (including infix, associativity, precedence) and type
