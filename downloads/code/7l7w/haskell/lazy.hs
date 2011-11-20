module Lazy where

thirds x = [x, x+3..]
fifths x = [x, x+5..]
eighths x y = zipWith (+) (thirds x) (fifths y)
