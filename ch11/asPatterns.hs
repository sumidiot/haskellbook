module AsPatterns where

import Data.Char -- for toUpper

-- exercise 1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False -- ordered after the case above, the [] [] case is ok
isSubseqOf xs@(xh:xt) ys@(yh:yt)
  | xh == yh = isSubseqOf xt yt -- we don't need || isSubseqOf xs yt, right?
  | xh /= yh = isSubseqOf xs yt

testSubseqOf =
  (isSubseqOf "blah" "blahwoot")
    && (isSubseqOf "blah" "wootblah")
    && (isSubseqOf "blah" "wboloath")
    && not (isSubseqOf "blah" "wootbla")
    && not (isSubseqOf "blah" "halbwoot")
    && (isSubseqOf "blah" "blawhoot")


-- exercise 2, failing to use as-pattern
capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = go s "" []
  where
    go [] [] l = l
    go [] b  l = l ++ [(b, munge b)]
    go (' ':cs) [] l = go cs "" l
    go (' ':cs) b l = go cs "" (l ++ [(b, munge b)])
    go (c:cs) b l = go cs (b++[c]) l
    munge (c:cs) = (toUpper c):cs
