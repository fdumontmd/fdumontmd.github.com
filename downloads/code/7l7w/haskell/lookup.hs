module Lookup where

my_lookup key [] = Nothing
my_lookup key ((k, value):rest)
  | key == k  = Just value
  | otherwise = lookup key rest

testData = [(1, []), (2, [("a", [("i", "tada!")]), ("b", [("j", "nope")])]), (3, [("c", [("k", "tada!")])])]