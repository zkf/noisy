{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Main where
import Data.Word
import Foreign.Storable (sizeOf)
import OpenSSL.Random
import Data.Binary.Strict.Get
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64 
import Control.Monad 
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar 
import Data.List (sort)
import System.IO
import Data.BanditSolver.LTS

rounds = 1000 -- rounds (timesteps) to run
type CryptoMutex = MVar (IO Word64)
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [obStart, obEnd, obStep] <- liftM (map read) getArgs
    let arms = GaussianArms $ [(5.0, 2.0)] 
                ++ replicate 1 (2.0, 2.0) -- actual arms of bandit
        armEstimates = GaussianArms $ replicate 2 (3.5, 3.0) -- starting estimate for arms
        obNoiseRange = [obStart, obStart+obStep .. obEnd]
    results <- newEmptyMVar
    atomicRand <- newMVar getOpenSSLRand -- fun trick to ensure atomicity
    forM_ obNoiseRange (forkIO . (runSimulation atomicRand results arms armEstimates rounds))
    printResults (length obNoiseRange) results

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

runSimulation :: CryptoMutex -> MVar String -> GaussianArms -> GaussianArms ->
    Int -> Double -> IO ()
runSimulation atomicRand var arms armEstimates rounds ob = do
    getRand <- takeMVar atomicRand
    rndGen <- pureMT `fmap` getRand
    putMVar atomicRand getRand
    let !(!mean, !variance) = runAveragedLts arms armEstimates rounds ob rndGen
    putMVar var $ show ob ++ "," ++ show mean ++ ", " ++ show variance

-- Get a decent random seed
-- randBytes is not thread-safe!
getOpenSSLRand :: IO Word64
getOpenSSLRand = do
    bytes <- randBytes n 
    let (Right w64, _) = runGet getWord64host bytes
    return w64
  where n = sizeOf (undefined :: Word64)
