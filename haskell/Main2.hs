module Main where 
import Data.BanditSolver.OBFinder
import System.Random.Mersenne.Pure64

main = do
    g <- newPureMT
    let result = test g
    print result
    
