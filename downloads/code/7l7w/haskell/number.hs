module Number where

import Justify

addLineNum lines =
    let md = floor $ logBase 10 (fromIntegral (length lines)) + 1
    in zipWith (lineNum md) [1..] lines
  where lineNum md num line = toStr md num ++ " " ++ line
        toStr maxDigits num = right maxDigits (show num)
