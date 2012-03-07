module Main where
import Data.BanditSolver.LTS

import System.Environment (getArgs)
import System.Random (getStdGen)
import Control.Monad (liftM)

main = do
    [obStart, obEnd, obStep] <- liftM (map read) getArgs
    gen <- getStdGen
    let results = go [obStart, obStart+obStep .. obEnd] gen
    putStrLn $ "Observation noise,Average cumulative reward over " ++ show rounds ++" rounds"
    mapM_ (putStrLn.(\(ob, r) -> show ob ++ "," ++ show r)) results
