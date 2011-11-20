module MyReverse where

-- the naive way
reverse_1 [] = []
reverse_1 (x:xs) = reverse_1 xs  ++ [x]

-- accumulator and tail recursion
reverse_2 xs = go [] xs
  where go acc []     = acc 
        go acc (x:xs) = go (x:acc) xs

-- accumulator and tail recursion means foldl
reverse_3 = foldl (\xs x -> x:xs) []

-- the anonymous function in reverse_3 is actually flip (:)
reverse_4 = foldl (flip (:)) []

-- foldl leaning so much to the left it comes back from the right:
reverse_5 ls = foldr (\b g xs -> g (b:xs)) id ls []

