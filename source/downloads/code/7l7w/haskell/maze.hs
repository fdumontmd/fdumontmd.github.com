module Maze where

import Data.List
import Control.Monad

-- a Maze is an array of Node
type Maze = [[Node]]

-- each node can have a number of exits (indicated by locations in the Maze)
data Node = Exits [(Int, Int)]
  deriving (Show)

-- solve a maze using List monad:
-- 
solveMaze :: Maze -> (Int, Int) -> (Int, Int)-> Maybe [(Int, Int)]
solveMaze m pos (e1, e2) =
  case loop [pos] of
    [] -> Nothing
    (p:_) -> Just $ p
  where 
    loop path@((i, j):_) = 
      if i == e1 && j == e2
      then return $ reverse path
      else
        let (Exits exits) = ((m !! i) !! j)
            poss = exits \\ path
        in do guard (not $ null poss)
              pos <- poss
              loop (pos:path)

-- the problem is parsed by looking at characters around
-- every even position (position with even x and y)
-- if the character in a direction is a space, there's an exit
parseMaze :: [String] -> Maze
parseMaze raw =
  let rows = floor $ (fromIntegral $ length raw) / 2
      cols = floor $ (fromIntegral $ length $ head raw) / 2
  in [[makeNode i j | j <- [1..cols]] | i <- [1..rows]]
 where 
  makeNode i j = Exits $ concatMap (makeExit i j) 
                           [(-1,0), (0,1), (1,0), (0,-1) ] 
  makeExit i j (y, x) = if (raw !! (2*i + y - 1) !! (2*j + x - 1)) == ' '
                        then [(i+y-1, j+x-1)]
                        else []

data Problem = Prob (Int, Int) (Int, Int) [String]
  deriving (Show)

-- updateAt runs an update function on the nth element in a list
-- keeps the rest
updateAt n f ls = 
  case splitAt n ls of
    (pre, (u:rest)) -> pre ++ ((f u):rest)

-- update the prob description with path iteratively
displaySol prob sol =
  foldl' update prob sol
 where
  update prob (i,j) = updateAt (2*i+1) (updateAt (2*j+1) (const '*')) prob

solveProblem :: FilePath -> IO ()
solveProblem f =
  do (Prob start end prob) <- readProblem f
     let maze   = parseMaze prob
     case solveMaze maze start end of
       Just sol -> putStrLn $ unlines $ displaySol prob sol
       Nothing  -> putStrLn "no solution found"

-- special code for sample mazes from
-- http://benjamin-meyer.blogspot.com/2005/01/ascii-maze-ment-puzzle.html
readProblem :: FilePath -> IO Problem
readProblem f = 
  do raw <- readFile f
     let filtered = case filterMaze raw of
                      (l:ls) -> ((init $ init l):ls)
         (i, j) = (length filtered, length (head filtered))
     return $ Prob ((i `div` 2) - 1, 0) (0, (j `div` 2) - 1) filtered

splitAtEvery :: Int -> [a] -> [[a]]
splitAtEvery _ [] = []
splitAtEvery n ls = 
  case splitAt n ls of
    (pre, rest) -> pre:splitAtEvery n rest

removeExtraSpaces (l:ls) = l:(concatMap tail $ splitAtEvery 3 ls)

filterMaze = map removeExtraSpaces . map (drop 5) . lines

