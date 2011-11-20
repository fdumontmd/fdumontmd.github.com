module Sieve where

minus xl@(x:xs) yl@(y:ys) 
    | x == y    = minus xs ys
    | x < y     = x : minus xs yl
    | otherwise = minus xl ys
    
primes = 2:sieve [3,5 ..]

sieve (p:xs) = p:sieve (xs `minus` [2*p, 3*p ..])
