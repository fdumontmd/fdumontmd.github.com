module GCD where

-- Euclidean algorithm
my_gcd m n | m < n     = my_gcd n m
           | n == 0    = m
           | otherwise = my_gcd (m-n) n
