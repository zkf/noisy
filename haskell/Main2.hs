module Main where 
import Data.BanditSolver.OBFinder
import System.Random.Mersenne.Pure64
import Control.Monad.State

main :: IO ()
main = do
    g <- newPureMT
    let result = evalState findOB g
    mapM_ print result
    
