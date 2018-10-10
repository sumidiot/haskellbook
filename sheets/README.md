## Cheatsheets

This directory contains some cheatsheets for useful functions and typeclasses and things.
For now, they're all just in this file.

### built-in functions

* `id`, the identity function
* `div`, `quot`, `rem`, `mod`, for numeric divisors and remainders and such

### syntax

* Anonymous function: `\x -> x + 1`
* `$` is function application, just associates to the right
* You can partially apply an infix binary operator as a "slice" with, e.g., `(+30)`

### ghci

#### Patterns

To create a value, you have to use a let expression, `let x = 1`.

You can enter multi-line blocks with `:{` and `:}`.

#### Commands

You can enter just the first letter of a command if it is not ambiguous

* `:load <file.hs>` -- load this module, changes prompt from Prelude> to maybe *Main>
* `:module` -- "quit" the loaded module, back to Prelude>
* `:module *Main` -- jump back to the *Main module
* `:reload` -- reload current module (e.g., re-read file)
* `:type` and `:kind` -- show... type... and kind
* `:info` -- type information for function, including infix, associativity, precedence
