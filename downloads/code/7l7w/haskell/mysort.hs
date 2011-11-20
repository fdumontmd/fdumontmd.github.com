module MySort where
import Data.Ord

my_sort [] = []
my_sort (x:xs) = my_insert x $ my_sort xs
  where my_insert x [] = [x]
        my_insert x (y:ys) | x > y        = y:my_insert x ys
                           | otherwise    = x:y:ys
