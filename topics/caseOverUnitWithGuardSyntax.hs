-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This was prepared in response to the first meeting of our study group.
--
-- During our meetup, we browsed various online documents and saw syntax similar to this:
--
--   let f = case () of _ | True -> 1
--
-- We consider this syntax in this document.
--
-- This code is intended to run in GHCi, not to be compiled by GHC.
-- You can copy and paste this whole file into GHCi (tested on 7.10.3).
--
-- Expected outputs will be shown using -- => result
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:set +m -- enable multi-line mode in GHCi


-- Run the example syntax
let f = case () of _ | True -> 1
-- an extra space is required

f
-- => 1


-- ~~~~~~~~
-- before we discuss, here are some examples to show how case works
-- ~~~~~~~~

-- simple case
:{
let  
f a = case a of 
  1 -> "one"
  2 -> "two"
:}

f 1
-- => "one"
f 2
-- => "two"

-- Here's a ridiculous case expression with alternatives and guards.
:{
let
f a = case a of 
    1 -> "one"
    2 -> "two"
    _  | (a > 10) -> "greater than 10"
       | (a > 5) -> "greater than 5"
    _  ->  "less than 5 but not one or two"
:}

-- apply f to each value 1, 2, up to and including 15.
map f [1..15]  
-- => ["one","two","less than 5 but not one or two","less than 5 but not one or two",
--     "less than 5 but not one or two","greater than 5","greater than 5",
--     "greater than 5","greater than 5","greater than 5","greater than 10",
--     "greater than 10","greater than 10","greater than 10","greater than 10"]

-- Want to be more fancy?
-- try this: 
--   import Control.Monad
--   mapM_ putStrLn $ map f [1..15]
-- try this: 
--   import Data.List
--   putStrLn $ intercalate "\n" $ map f [1..15]


-- ~~~~~~~~
-- Let's look at the weird version again.
-- ~~~~~~~~

let f = case () of _ | True -> 1

f
-- => 1

-- ~~~~~~~~~
-- So, the function returns 1.
-- Let's break it down.
--
-- let               : I added this because declarations in GHCi require let, but GHC doesn't.
--   f =             : We're declaring a new function named "f"
--      case ()      : We're going to do a case on (), which is the unit value (discussed below).
--                     This means, we will provide multiple alternatives to try to match
--                     against () to see which wins.
--
--        of _       : _ is a variable with no constraints, so it will match ()
--            | True : Having matched () to _, we proceed to consider conditions.
--                     There's single alternative, True, in play.  This will always
--                     evaluate to true.  (Another condition like "2 > 3" may not).
--              -> 1 : And if this condition is true, this is what the case should evaluate as being.
-- ~~~~~~~~~

-- ~~~~~~~~~
-- About the unit value ():
--                   You can think of it as the name of a value in a type that permits a single value.
--                   It's not special that it's called "()", and the choice of the value is irrelevant.
--                   If you build your own type system, you can pick "1", 1, "XLKDJLKS", or whatever you want.
-- ~~~~~~~~~~~~~~
