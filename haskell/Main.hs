module Main where
import Data.BanditSolver.LTS

import System.Environment (getArgs)
import System.Random (getStdGen)
import Control.Monad (liftM)

main :: IO ()
main = do
    [obStart, obEnd, obStep] <- liftM (map read) getArgs
    gen <- getStdGen
    let arms = [(5.0, 2.0), (2.0, 2.0), (3.0, 5.0)] -- actual arms of bandit
        armEstimate = (3.5, 3.0) -- beginning estimate for arms
        rounds = 100 -- rounds (timesteps, or t) to run
        obNoiseRange = [obStart, obStart+obStep .. obEnd]
        results = findBestObservationNoise arms armEstimate rounds obNoiseRange gen
    putStrLn $ "Observation noise,Average cumulative reward over " ++ show rounds ++" rounds"
    mapM_ (putStrLn.(\(ob, r) -> show ob ++ "," ++ show r)) results
