module MySort where
import Data.Ord

my_sort [] = []
my_sort (x:xs) = my_insert x $ my_sort xs
  where my_insert x [] = [x]
        my_insert x (y:ys) | x <= y    = x:y:ys
                           | otherwise = y:my_insert x ys
                           
my_sort_by :: (a -> a -> Ordering) -> [a] -> [a]
my_sort_by _ []     = []
my_sort_by f (x:xs) = my_insert_by f x $ my_sort_by f xs
  where my_insert_by _ x [] = [x]
        my_insert_by f x (y:ys) | f x y == GT = y:my_insert_by f x ys
                                | otherwise   = x:y:ys
