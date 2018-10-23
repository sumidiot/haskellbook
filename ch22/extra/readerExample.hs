module ReaderExample where

import Control.Applicative
import Control.Monad.Reader
import Data.Map as Map

-- copied in from http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:4


type Bindings = Map String Int

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
    count <- asks (lookupVar "count")
    bindings <- ask
    return (count == (Map.size bindings))

calc_isCountCorrect' :: Bindings -> Bool
calc_isCountCorrect' = liftA2 (==) (lookupVar "count") Map.size

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (Map.lookup name bindings)

sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

main = do
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
    putStrLn $ show (isCountCorrect sampleBindings);



-- shorter still

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect' :: Bindings -> Bool
isCountCorrect' msi = (lookupVar "count" msi) == (Map.size msi)

main' = do
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
    putStrLn $ show (isCountCorrect' sampleBindings);



