module Justify where

import Data.Char

-- generalize words and lines: remove extra break characters at the start,
-- then split at the next break characters; recurse on the rest

breakAtEvery f xs = case dropWhile f xs of
                      []  -> []
                      xs' -> p:breakAtEvery f r
                        where (p, r) = break f xs'

my_words = breakAtEvery isSpace

-- loop iterate over lines built by accumUntil
-- accumUntil builds a line (at least one word) and adds words
-- until the length is too long

split m str = loop $ my_words str
  where loop [] = []
        loop (w:words) = case accumUntil m w words of
                           (line, []) -> [line]
                           (line, rest) -> line:loop rest
        accumUntil _ line []        = (line, [])
        accumUntil m line (w:words) = let line' = line ++ " " ++ w
                                      in if (length line' < m)
                                         then accumUntil m line' words
                                         else (line, w:words)                                        

-- takes a justify function, a max length line (will be adjusted to max of strs lengths)

justify f m strs =
  let m' = foldl max m (map length strs)
  in map (f m') strs

-- when justifying both sides, the last line should be left justified
justify_both m strs = 
  let m' = foldl max m (map length strs)
  in map (both m') (init strs ) ++ [left m' (last strs)]
  
-- create n spaces

pad n = take n (repeat ' ')
 
-- Justify function helpers: add spaces at the right location to make the length of line equal to m. Incorrect if m is smaller than length line 

-- pad spaces on the right 
left m line = line ++ pad (m - length line)

-- pad spaces on the left

right m line = pad (m - length line) ++ line

-- pad both sides by half the difference

center m line =
  let lp = floor (fromIntegral (m - length line) / 2)
      rp = m - lp
  in pad lp ++ line ++ pad rp

-- justify both sides: compute the size of each interval in FRACtional value
-- then build a list of interval paddings with the length as close as possible
-- from the running sum of fractional intervals
both m []   = pad m
both m line =
  let (w:words) = my_words line
      inter = length words
      addSpacesFrac = take inter (repeat (fromIntegral (m - length line + inter) / (fromIntegral inter)))
      addSpaces = case foldl adjust (0, 0, []) addSpacesFrac of
                    (_,_,spaces) -> spaces
  in concat (w:zipWith (++) addSpaces words)
  where adjust (curr, currFrac, spaces) add = let newFrac = currFrac + add
                                                  diff = round (newFrac - fromIntegral curr)
                                              in (curr + diff, newFrac, spaces ++ [pad diff])
                                              
-- addLineNum simply computes how many digits are needed for line numbers,
-- then prefix each line with the right justified line number

addLineNum lines =
    let md = floor $ logBase 10 (fromIntegral (length lines)) + 1
    in zipWith (lineNum md) [1..] lines
  where lineNum md num line = toStr md num ++ " " ++ line
        toStr maxDigits num = right maxDigits (show num)
