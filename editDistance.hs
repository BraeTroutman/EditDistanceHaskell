import System.Environment
import Data.Array

editDistance :: String -> String -> Int
editDistance src tgt = sltns ! (m,n)
  where m = length src
        n = length tgt
        srcArr = A.array (0,m-1) $ zip (A.range (0,m-1)) src
        tgtArr = A.array (0,n-1) $ zip (A.range (0,n-1)) tgt
        sltns = array ((0,0),(m,n)) [
        
        dist (0,j) = j
        dist (i,0) = i
        dist (i,j) = minimum [sltns ! (i-1,j) + 1, sltns ! (i,j-1) + 1, 
          if srcArr A.! (i - 1) == tgtArr A.! (j - 1) then sltns ! (i-1,j-1) else 1 + sltns ! (i-1,j-1)]

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go [src,tgt] = print $ editDistance src tgt

editDistanceMatrix :: String -> String -> Matrix Int
editDistanceMatrix src trg = sltns
  where m = (length src)
        n = (length trg)
        sltns = matrix m n dist
        dist (i,1) = i - 1
        dist (1,j) = j - 1
        dist (i,j) = minimum [1 + sltns ! (i-1,j), 1 + sltns ! (i,j-1), if src !! (i-2) == trg !! (j-2) 
                                                                        then sltns ! (i-1,j-1) 
                                                                        else 1 + sltns ! (i-1,j-1)]
