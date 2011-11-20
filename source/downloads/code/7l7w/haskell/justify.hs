module Justify where

import Split

-- takes a justify function, a max length line (will be adjusted to max of strs lengths)

justify f m strs =
  let m' = foldl max m (map length strs)
  in map (f m') strs

-- when justifying both sides, the last line should be left justified
justify_both m strs = 
  let m' = foldl max m (map length strs)
  in map (both m') (init strs ) ++ [left m' (last strs)]

justify_left = justify left
justify_right = justify right
justify_center = justify center

-- helper functions  
-- create n spaces

pad n = take n (repeat ' ')
 
-- Justify function helpers: add spaces at the right 
-- location to make the length of line equal to m. 
-- Incorrect if m is smaller than length line 

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
      addInterFrac = map (makeInter inter) [1..inter]
      addSpaces = case foldl adjust (0, []) addInterFrac of
                    (_,spaces) -> spaces
  in concat (w:zipWith (++) addSpaces words)
  where adjust (curr, spaces) newFrac = let diff = round (newFrac - fromIntegral curr)
                                        in (curr + diff, (pad diff):spaces)
        makeInter inter i = fromIntegral i 
             * (fromIntegral (m - length line + inter)) 
             / fromIntegral inter                                           
