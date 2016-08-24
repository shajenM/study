--
-- Sodoku Solver Using Backtracking
-- Shajen M
-- 20 Aug 2016
--
import Data.List


sub_matrix :: (Int,Int) -> [[Int]] -> [[Int]] 
sub_matrix (i,j) p = 
         let substart  t = (t `div` 3 )*3
             col_matrix  c p = map ( take 3. drop c ) p
             in  take 3. drop (substart i)  $ col_matrix (substart j) p

isValid    :: (Int,Int) -> Int -> [[Int]] -> Bool
isValid (i,j) n p =
         let  notInRow   = all (/=n) $ p !! i 
              notInCol   = all (/=n) $ (transpose p) !! j 
              notInUnit = all (/=n) $ concat $ sub_matrix (i,j) p
              in  foldr (\f a -> a&& f ) True  [notInRow , notInCol, notInUnit]

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
             in   ( take i p ) ++ [u] ++ ( drop (i+1) p )

solve      :: (Int,Int) -> [[Int]] -> (Bool, [[Int]])  -- start solving from (i,j)
solve (9,_) p = (True,p) -- reached last index of matrix , means we solved it!
solve (i,j) p
            | ( val (i,j) p )==0  = tryPossibles (i,j) p 
            | otherwise           = solve (next (i,j)) p
                      
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

p1::[[Int]]
p1=[
  [0,8,7,0,0,0,4,0,0],
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
p2::[[Int]]
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
