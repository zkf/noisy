{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Main where
import Data.Word
import Foreign.Storable (sizeOf)
import OpenSSL.Random
import Data.Binary.Strict.Get
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64 
import Control.Monad 
--import Control.Concurrent (forkIO)
--import Control.Concurrent.MVar 
import Data.List (sort)
import System.IO
import Data.BanditSolver.LTS
-- import GHC.Conc (getNumCapabilities)
import Control.Parallel.Strategies

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [obStart, obEnd, obStep, roundsS] <- getArgs
    let arms = [(5.0, 2.0)] ++ replicate 99 (3.0, 2.0) -- actual arms of bandit
        armEstimates = replicate (length arms) (3.5, 3.0) -- starting estimate for arms
        obNoiseRange = [read obStart, (read obStart) + (read obStep) .. read obEnd]
        rounds = read roundsS
    gens <- (map pureMT) `fmap` replicateM (length obNoiseRange) getOpenSSLRand
    let results = parMap rseq id (zipWith (runSimulation arms armEstimates rounds) obNoiseRange gens)
        resultsS = map (\(o, m, s) -> unwords $ map show [o,m,s]) $ sort results
    putStrLn $ "#Observation noise, mean, std-dev of cumulative reward over "
                   ++ show rounds ++ " rounds"
    mapM_ putStrLn resultsS
    
runSimulation ::  [(Double, Double)] -> [(Double, Double)] ->
    Int -> Double -> PureMT -> (Double, Double, Double)
runSimulation arms armEstimates rounds ob rndGen =
    let !(!mean, !variance) = runAveragedLts (GaussianArms arms) (GaussianArms armEstimates) rounds ob rndGen
    in (ob, mean, variance)

-- Get a decent random seed
-- randBytes is not thread-safe!
getOpenSSLRand :: IO Word64
getOpenSSLRand = do
    bytes <- randBytes n 
    let (Right w64, _) = runGet getWord64host bytes
    return w64
  where n = sizeOf (undefined :: Word64)

{-
printResults :: Int -> MVar String -> IO ()
printResults n var = do
    r <- getResults n var
    putStrLn $ "\n\nObservation noise, mean, std-dev of cumulative reward over "
                   ++ show rounds ++ " rounds"
    mapM_ putStrLn (sort r) 

getResults :: Int -> MVar String -> IO [String]
getResults len var = forM [1..len] $ \n -> do
    result <- takeMVar var 
    putStr ("\r" ++ show n ++ " / " ++ show len)
    return result
-}
