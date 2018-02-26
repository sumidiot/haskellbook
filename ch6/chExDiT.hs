module ChapterExercisesDoesItTypecheck where

-- Problem 1. Fails without the `deriving Show` on the next line
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- Problem 2: Needs `Eq` for the `if` test
data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                 then Blah
                 else x

-- Problem 3.
--   a. Blah and Woot are acceptable values
--   b. settleDown 9 fails because `:: Mood -> Mood`
--   c. Blah > Woot fails because we don't derive `Ord`

-- Problem 4. Typechecks just fine
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool" -- type is Object -> Sentence
s2 = Sentence "Julia" "loves" "dogs" -- type is Sentence
