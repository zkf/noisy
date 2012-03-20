{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.LTS (runAveragedLTS, GaussianArms) where

import Control.Monad.State 
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT)
import Data.RVar (sampleRVar)
import Statistics.Sample (meanVariance)
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (write)
import Data.List.Split
import Data.List
import Control.Applicative
import Control.Monad.Writer

import System.Random.Mersenne.Pure64
    
type GaussianArm = (Double, Double) -- Mean, standard deviation
type GaussianArms = (V.Vector GaussianArm)
type BanditSolver = (GaussianArms, Double)

runAveragedLTS :: GaussianArms -> GaussianArms -> Double -> Int -> Int -> PureMT
    -> [(Int, Double, Double, Double)] -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTS realArms startEstimates ob rounds repetitions randomgen = 
    fst $ evalState (runStateT (execWriterT $ runAll realArms ob rounds) solvers) randomgen
  where solvers = replicate repetitions (startEstimates, 0)


runAll :: GaussianArms -> Double -> Int -> 
    WriterT [(Int, Double, Double, Double)] (StateT [BanditSolver] (State PureMT)) ()
runAll realArms ob rounds = forM_ [1..rounds] $ \n -> do
    lift $ oneRound realArms ob 
    if n `elem` checkpoints 
        then do 
             solvers <- lift get
             let (_, crewards) = unzip solvers
                 (mean, variance) = meanVariance $ V.fromList crewards
             tell [(n, ob, mean, sqrt variance)]
        else return ()
  where checkpoints = rounds : [y * 10^x | y <- [1, 5],
          x <- [1..(floor . logBase 10 $ (fromIntegral rounds)/5.0)]]
             

oneRound :: GaussianArms -> Double -> StateT [BanditSolver] (State PureMT) ()
oneRound realArms ob = do
    solvers <- get
    selections <- lift $ mapM selectArm solvers
    rewards    <- lift $ mapM (getReward realArms) selections
    let solvers' =  getZipList $ update ob <$>
                    ZipList solvers <*> ZipList selections <*> ZipList rewards
    put solvers'


getReward :: GaussianArms -> Int -> State PureMT Double
getReward arms idx = gaussian (arms ! idx)

selectArm :: BanditSolver -> State PureMT Int
selectArm (arms, _) = do
    v <- gaussian (arms ! start)
    go start v start
  where start = V.length arms - 1
        go :: Int -> Double -> Int -> State PureMT Int
        go 0 mVal idx = do
            nVal <- gaussian (arms ! 0)
            if nVal > mVal 
                then return 0
                else return idx 
        go n mVal idx = do
            nVal <- gaussian (arms ! n)
            if nVal > mVal 
                then go (n-1) nVal n
                else go (n-1) mVal idx
            
update :: Double -> BanditSolver -> Int -> Double -> BanditSolver
update ob (arms, creward) index reward = 
    let (mu, sigma) = arms ! index
        armVariance = sigma**2
        obVariance  = ob**2
        mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        arm'   = (mu', sigma')
        arms' =  V.modify (\v -> write v index arm') arms
    in (arms', creward + reward)

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma
