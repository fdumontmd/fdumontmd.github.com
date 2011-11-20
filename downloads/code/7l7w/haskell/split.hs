module Split where

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
