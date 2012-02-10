import Data.List
import Data.Ord

import Control.Monad
import System.Random

-- helpers functions
fact n = product [1..n]
combi n c = (fact n) `div` (fact c * fact (n-c))

remainingNumbers js = foldr union [] js

-- generate combinations
genCombi 0 _ = [[]]
genCombi _ [] = []
genCombi k (l:ls) = [l:cs | cs <- genCombi (k-1) ls] ++
                    genCombi k ls

-- estimates for the solution size
ticketCover n k j l = sum [ (combi k i) *
                            (combi (n-k) (j-i)) | i <- [l..j]]
lowerBound n k j l = (fromIntegral $ combi n j) /
                     (fromIntegral $ ticketCover n k j l)

-- definition of coverage measure
cover l1 l2 = length $ intersect l1 l2

-- predicate functions: check cover between ticket and j-subset
coveredP l t j = l <= cover t j
notCoveredP l t j = l > cover t j

-- keep only j-subsets that are not covered by tickets
notCoveredBatch l ts js = foldr (notCovered l) js ts
notCovered l t js = filter (notCoveredP l t) js

-- length of j-subsets from js that are sufficiently covered by t
coverageScore l t js = length $ filter (coveredP l t) js

-- check the coverage of a single k number ticket on the C_j^n
-- potentials; can be compared against ticketCover estimate
checkFormula n k j l =
  let candidates = genCombi j [1..n]
      ticket = [1..k]
      covered = filter (coveredP l ticket) candidates
  in length covered

-- compute solution given a candidate generator
solve n k j l gc =
  let jtuples = genCombi j [1..n]
  in loop jtuples
 where loop [] = return []
       loop js = do
         t <- gc n k j l js
         ts <- loop $ notCovered l t js
         return (t:ts)

-- naive candidate generator
getCandidate n k j l js =
  let numbers = remainingNumbers js
      tickets = genCombi k numbers
      ticketsScore = map (\t -> (coverageScore l t js, t)) tickets
  in return $ snd $ maximumBy (comparing fst) ticketsScore

-- Knuth method for generating combination
-- adapted for arbitrary set
sample 0 _ = return []
sample _ [] = return []
sample k ds = do
  s <- sample (k-1) (tail ds)
  p <- randomRIO (0, (length ds - 1))
  let t = ds!!p
  if not (t `elem` s)
    then return (t:s)
    else return (head ds:s)

-- random candidate generator
getCandidateRandom beta n k j l js = do
  let numbers = remainingNumbers js
  tickets <- replicateM beta (sample k numbers)
  let ticketsScore = map (\t -> (coverageScore l t js, t)) tickets
  return $ snd $ maximumBy (comparing fst) ticketsScore
