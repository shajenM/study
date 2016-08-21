--
-- Sodoku Solver Using Backtracking
-- Shajen M
-- 20 Aug 2016
--
import Data.List

groupstart :: Int->Int  -- start index of a group 
groupstart t = (t `div` 3 )*3

col_matrix :: Int -> [[Int]] -> [[Int]]  -- crop matrix at column c
col_matrix     c p = map ( take 3. drop c ) p

sub_matrix :: (Int,Int) -> [[Int]] -> [[Int]] 
sub_matrix (i,j) p = take 3. drop (groupstart i)  $ col_matrix (groupstart j) p

notInGroup :: (Int,Int) -> Int -> [[Int]] -> Bool
notInGroup (i,j) n p = all (/=n) $ concat $ sub_matrix (i,j) p

notInRow   :: (Int,Int) -> Int -> [[Int]] -> Bool
notInRow   (i,j) n p = all (/=n) $ p !! i 

notInCol   :: (Int,Int) -> Int -> [[Int]] -> Bool
notInCol   (i,j) n p = all (/=n) $ (transpose p) !! j 

isValid    :: (Int,Int) -> Int -> [[Int]] -> Bool
isValid    (i,j) n p = foldr (\f a -> a&& f (i,j) n p ) True  [notInRow , notInCol, notInGroup]

val        :: (Int,Int) -> [[Int]] -> Int  -- value at an index of matrix
val        (i,j)   p = p !! i !! j

next       :: (Int,Int) -> (Int,Int)      -- index of next element in matrix
next       (i,j)    
                  | j<8  = (i,j+1)
                  | otherwise  = ( i+1, 0)

update     :: (Int,Int) -> Int -> [[Int]] -> [[Int]] -- change matrix with val n
update     (i,j) n p =
  let r = p !! i
      u  = ( take j r ) ++ [n] ++ ( drop (j+1) r )
      p1 = ( take i p ) ++ [u] ++ ( drop (i+1) p )
      in p1

solve      :: (Int,Int) -> [[Int]] -> (Bool, [[Int]])  -- start solving from (i,j)
solve (9,_) p = (True,p)
solve (i,j) p
   | ( val (i,j) p )/=0  = solve (next (i,j)) p
   | otherwise  = tryPossibles (i,j) p 
                      
possibles :: (Int,Int) -> [[Int]] -> [Int]    -- possible valid values at (i,j)
possibles (i,j) p = filter (\n->isValid (i,j) n p)  [1..9]

tryPossibles :: (Int,Int) -> [[Int]] -> (Bool, [[Int]] )
tryPossibles (i,j) p = 
  foldl( \(b,r) n -> if b==False 
                        then solve (next (i,j) ) ( update (i,j) n p)
                        else (True,r)
       ) (False,p) $ possibles (i,j) p
                                         
printMatrix :: [[Int]] -> IO ()
printMatrix p = do 
  print "------------------"
  mapM_ print p
   
-- start solving from (0,0)
soduku p  = solve (0,0) p

p1=[
  [9,8,7,0,0,0,4,0,0],
  [0,0,0,0,8,3,2,0,0],
  [0,0,6,0,0,0,0,0,5],
  [0,0,0,1,7,0,0,0,0],
  [8,1,0,5,0,4,0,7,3],
  [0,0,0,0,3,6,0,0,0],
  [1,0,0,0,0,0,3,0,0],
  [0,0,4,3,2,0,0,0,0],
  [0,0,3,0,0,0,5,2,9]
  ]

-- ...............3282.37.8..5......9.147.1.5.863.6......5..6.41.9791...............
p2=[
 [0,0,0,0,0,0,0,0,0],
 [0,0,0,0,0,0,3,2,8],
 [2,0,3,7,0,8,0,0,5],
 [0,0,0,0,0,0,9,0,1],
 [4,7,0,1,0,5,0,8,6],
 [3,0,6,0,0,0,0,0,0],
 [5,0,0,6,0,4,1,0,9],
 [7,9,1,0,0,0,0,0,0],
 [0,0,0,0,0,0,0,0,0]
 ]
 
main :: IO ()
main = do
  printMatrix p2
  printMatrix $ snd $ soduku p2
