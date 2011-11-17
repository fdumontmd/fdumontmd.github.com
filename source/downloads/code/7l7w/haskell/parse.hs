module Parse where

import Data.Char
import Data.List

parse str =
    let (dig, frac) = break (== '.') str
    in foldl' pDig 0 (clean dig) + foldr pFrac 0 (clean frac) / 10

clean s   = filter isDigit s
pDig  a b = 10*a + toNum b
pFrac a b = toNum a + b/10
toNum c   = fromIntegral $ ord c - ord '0'

