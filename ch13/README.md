## Chapter 13: Building projects

### My Reading Notes

#### 13.1 Modules

Haskell programs are organized into modules, which are like namespaces in other languages.
A module imports from others, and specifies what can be exported.

This chapter is mostly focused at the build steps and modules and such, less with the
particular code that's getting built. All the same, we're going to write a pacman game.

_Cabal_ is a Haskell package manager. _Stack_ is a Haskell project build tool. (I guess?)

#### 13.2 Making packages with Stack

_Cabal_ is the Common Architecture for Building Applications and Libraries.

A package is a program, with all of its modules and dependencies.

Stack is a cross-platform program for developing Haskell projects. It helps with projects
that consist of a single package, or multiple packages. Cabal is primarily for describing
a single package, though a file with the `.cabal` extension.

Hackage is a place to get packages.

There's no prescibed project layout, but the one we'll develop in this chapter is a good example.

#### 13.3 Working with a basic project

`git clone https://github.com/haskellbook/hello && cd hello`

`stack build` uses the `stack.yaml` file to build the project, compiling files and saving
them, apparently, in `.stack-work` in the root project directory. You might need to `stack setup`
if you get errors about GHC installations.

`stack ghci` then loads the `Main` module of the project (from `src/Main.hs`). Since the module
is loaded (and its dependencies, if it had any), you can use the functions from the module.

`stack exec` can be used to run the built executable, made during `stack build`. If you really
want you can dig into long file paths under `.stack-work`, looking at the output of `stack build`.

The executable is made because of the `executable hello` block in the `hello.cabal` file.
You can also set things up so that cabal generates reusable libraries, vs executables.

#### 13.4 Making our project a library

Add a `library` "stanza" to `hello.cabal`, which will then cause `stack build` to make a
`hello` library. Also make a `src/Hello.hs` module, putting the functionality of `Main` in it,
and then call it from `Main`.

If you also move `src/Main.hs` to `exe/Hello.hs` (which is maybe recommended?), the build will
fail. You have to add the `hello` library as a dependency of the `executable hello` build target.

#### 13.5 Module exports

If you don't explicitly export anything in a module, every top-level binding is exported.

`module M ( export1, export2, ...) where` is the syntax to export symbols. It can technically
be an empty tuple.

To add more source files and have them be included in the library, you have to change the
`library` stanza of the `hello.cabal` file, adding to the list of `exposed-modules`.

#### 13.6 More on importing modules

`:browse` in GHCi lets us see the functions in a module, distinct from `import`.

You can start GHCi without the `Prelude` with `stack ghci --ghci-options -XNoImplicitPrelude`.
If you do, you won't have many things, like `bool`. To import just part of a module, use
`import M (f)` syntax, where `M` is the module (e.g., `Data.Bool`) and `f` is the function to
import (e.g., `not`). For a list of functions, just separate them with commas.

##### Qualitified imports

You can use a `qualified` import so you can tell where a thing you imported came from.
`import qualified X` imports the things from `X`, but you have to refer to them by `X.x`.
For example, `import qualified Data.Bool` will then make `Data.Bool.bool` available, but
not the unqualified `bool`.

You can alias a qualified import, `import qualified X as Y`, and then refer to `X.x` as `Y.x`.
So, `import qualified Data.Bool as B` makes `B.bool` available.

#### 13.7 Making our program interactive

`do` introduces a block of code, it is syntactic sugar we'll understand more about de-sugaring later.
It lets us sequence side effects, particularly in `IO`, easily.

`getLine` has type `IO String`

`<-` is pronounced **bind** in a `do` block. In some sense, `x <- IO X` unpacks the `X`
value from the `IO X`, assigning the value to `x` (so `:t x` is `X`).

#### 13.8 `do` syntax and `IO`

The `main` of a Haskell program must have type `IO ()`. `do` let's you sequence "monadic actions",
and `IO` is an example, which is why many `main` methods are written as `do` blocks. We'll go
into more detail on monads in later chapters.

##### `return`

`return` doesn't really do a lot. It wraps a value in a monad, like `IO`. So, `return "this"`
is of type `IO String` (when you're dealing with the `IO` monad). If you need to return
nothing, like `main` returning `IO ()`, you can `return ()`.

`do` notion is considered bad style in single-line expressions. It's also unnecessary to use
it with functions like `putStrLn` or `print` that already return an `IO`.

#### 13.9 Hangman game

`stack new (name) simple` will create a new project with name `name`.

#### 13.10 Step One: Importing modules

`Control.Monad.forever` makes an infinite loop. The type is `Monad m => m a -> m b`

`:t all` is `Foldable t => (a -> Bool) -> t a -> Bool` (you can think of `Foldable` as `List`
for now).

`Data.List.intersperse` shuffles a thing into all interior spots of a list. `intersperse ' ' "word"`
is "w o r d".

`System.Exit.exitSuccess` is a way to indicate that a program has ended successfully.

`System.Random.randomRIO` has type `:Random a => (a, a) -> IO a`. It treats the tuple as a
range, and gives a random item in the range.

#### 13.11 Step Two: Generating a word list

`lines` is a function to split a big blob string by newlines (compare `words` for splitting by spaces).

`>>=`, also known as **bind**, we'll talk more about in the `Monad` chapter

#### 13.12 Step Three: Making a puzzle

#### 13.13 Adding a newtype

### Meetup topic seeds

1. Favorite editor/plugins?
2. What does `RIO` stand for in `randomRIO`? "Range IO" since `:t` is `Random a => (a, a) -> IO a`
3. It seems like a fun/useful exercise would be to convert the hangman example to take a
    command line argument with the number of incorrect guesses to allow
4. There's a lot more `IO` going around in the hangman code than I'd have guessed we should
    be trying to aim for. Can we reduce it?
5. I tried deleting the `LICENSE` file of hangman, and `stack build` failed because of it!
    Same for the `README`!
