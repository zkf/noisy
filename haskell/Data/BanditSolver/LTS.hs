{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.LTS (runAveragedLTS, GaussianArms) where
import Control.Monad.Writer
import Control.Monad.State
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT)
import Data.RVar (sampleRVar)
import Statistics.Sample (meanVariance)
import Data.Vector.Generic.Mutable (write)
import Control.Parallel (pseq)
import Data.Vector ((!))
import qualified Data.Vector as V


type GaussianArm = (Double, Double) -- Mean, standard deviation
type GaussianArms = (V.Vector GaussianArm)
type BanditSolver = (GaussianArms, Double)

runAveragedLTS :: GaussianArms -> GaussianArms -> Double -> Int -> Int -> PureMT
    -> [(Int, Double, Double, Double)] -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTS realArms startEstimates ob rounds repetitions randomgen = 
    snd $ evalState (runWriterT $ runAll realArms ob rounds solvers) randomgen
  where solvers = replicate repetitions (startEstimates, 0)


runAll :: GaussianArms -> Double -> Int -> [BanditSolver] ->
    WriterT [(Int, Double, Double, Double)] (State PureMT) ()
runAll realArms ob rounds startSolvers = run 0 startSolvers
  where run !n solvers
            | n `notElem` checkpoints = do
                solvers' <- lift $ oneRound realArms ob solvers
                forceList solvers' `seq` run (n+1) solvers'
            | otherwise = do
                solvers' <- lift $ oneRound realArms ob solvers
                let (_, crewards) = unzip solvers'
                    (mean, variance) = meanVariance $ V.fromList crewards
                tell [(n, ob, mean, sqrt variance)]
                if n == rounds
                    then return ()
                    else forceList solvers' `seq` run (n+1) solvers'
        checkpoints = rounds : [y * 10^x | y <- [1, 5],
              x <- [1 :: Int .. floor (logBase 10 $
                                         (fromIntegral rounds)/5.0 :: Double)]]

forceList :: [a] -> ()
forceList (x:xs) = x `pseq` forceList xs
forceList _      = ()
             

oneRound :: GaussianArms -> Double -> [BanditSolver] -> State PureMT [BanditSolver]
oneRound realArms ob solvers = mapM (oneLTS realArms ob) solvers

oneLTS :: GaussianArms -> Double -> BanditSolver -> State PureMT BanditSolver
oneLTS realArms ob solver = 
    selectArm solver >>= \selected -> 
        getReward realArms selected >>= return . update ob solver selected

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
        !creward' = creward + reward
    in (arms', creward')

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma
