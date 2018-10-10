## Chapter 2: Hello, Haskell!

### My Reading Notes

ghci commands:
* `:load <file.hs>` -- load this module, changes prompt from `Prelude>` to maybe `*Main>`
* `:module` -- "quit" the loaded module, back to `Prelude>`
* `:module *Main` -- jump back to the `*Main` module
* `:reload` -- reload current module (e.g., re-read file)
* `:type` and `:kind` look fun :)
* `:info` -- type information for function, including infix, associativity, precedence

Expression vs Declaration:
* **expressions** evaluate to results
* **declarations** allow us to name expressions

Reducible expressions are called "redexes"

Argument vs Parameter:
* _arguments_ are passed to the function's _parameters_

Define a function in ghci (pre-8.0.1):
* `let triple x = x * 3`

Note: function name (declaration) begins with lowercase letter. Variable names do too.

Haskell uses lazy evaluation.
Haskell evaluates to "weak head normal form" by default

`(\f -> (1,2+f)) 2` yields `(1,4)` in repl

`id` is builtin, `id x = x`

**operators** are functions that can be used in infix style
* You can use a function infix with backticks: 10 `div` 4
* You can use an infix as prefix with parens: `(+) 10 4`

Order of declarations doesn't matter if you load a file.

Module names are capitalized.

Indentation and whitespace matter in Haskell. Use spaces not tabs.
Indent under the beginning of an expression if you continue to a line, all to the same level.
Indentation of first declaration determines required indentation for all subsequent lines in file.

`--` is used for comments

Division
* `div` is integer division, rounds down.
* `quot` is integer division, rounds toward zero.
* `(quot x y) * y + (rem x y) == x`
* `(div x y) * y + (mod x y) == x`
* `mod` has same sign as divisor, `rem` has sign as dividend`

`-` might mean "negate" or "subtract"

`$` is right-associative function application (which is normally left associative).
`triple (triple 3) == triple $ triple 3`. Note that `$` delays function application.

(*30) is called "slicing" or "sectioning", creates partially applied function

`let` introduces an _expression_, `where` is a _declaration_, but i still don't quite parse those examples.
The chapter notes point to the [haskellwiki](https://wiki.haskell.org/Let_vs._Where), which
shows some more examples/differences/advantages.

### Exercises

#### 2.5 Exercises

1. `let half x = x / 2` or `(\x -> x / 2)` if you don't need the name
   `let square x = x * x`
2. `let area r = 3.14 * (r * r)`
3. `let area r = pi * (r * r)`

#### Parens + Association Exercises

1. changes
2. doesn't change
3. changes

#### Heal the Sick Exercises

1. `let area x = 3.14 * (x * x)`
2. `let double x = x * 2`
3.
    x = y
    y = 10
    f = f + y

#### A Head Code Exercises

1. 5
2. 25
3. 30
4. 6

#### Chapter Exercises

Parenthesization
1. 2 + (2 * 3) - 1
2. (^) 10 $ (1 + 1)
3. (2 ^ 2) * (4 ^ 5) + 1

Equivalent Expressions: 1, 2, 

### Meetup Discussion

#### My notes

1. The HeadCode exercises, "rewrite with `where`"?
2. Chapter Exercises, Functions, 3, fails with nested where clauses?

