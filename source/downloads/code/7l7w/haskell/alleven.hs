module AllEven where

-- basic definition
allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t) = if even h then h:allEven t else allEven t

-- essentially we are filtering, so maybe a filter function would help
my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f (x:xs) = if f x then x:my_filter f xs else my_filter f xs

-- so now we can use the my_filter function to write a new allEven:
allEven_2 ls = my_filter even ls

-- in point free notation,
allEven_2' = my_filter even

-- actually, filter already exists:
allEven_filter = filter even

-- if even numbers are replaced by a singleton, and odd numbers 
-- by an empty list, when we concatenate, we get the result we want
allEven_3 = concat . map (\x -> if even x then [x] else [])

-- concat . map == concatMap
allEven_4 = concatMap (\x -> if even x then [x] else [])

-- processing lists left to right is a job for foldr. 
-- Actually this is closer to the original definition
allEven_5 = foldr (\x xs -> if even x then x:xs else xs) []

-- accumulator and tail recursive version of allEven_5
allEven_6 ls = foldl (\g b x -> g (f b x)) id ls []
  where f x xs = if even x then x:xs else xs

-- list comprehension
allEven_7 ls = [x | x <- ls, even x]