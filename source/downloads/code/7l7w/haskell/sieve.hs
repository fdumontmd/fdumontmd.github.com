module Sieve where

minus xl@(x:xs) yl@(y:ys) 
    | x == y    = minus xs ys
    | x < y     = x : minus xs yl
    | otherwise = minus xl ys
    
-- [2..] also works, just a bit of optimization by removing even numbers. primes = sieve [2..] is a valid implementation as well
primes = 2:sieve [3,5 ..]

sieve (p:xs) = p:sieve (xs `minus` [2*p, 3*p ..])
