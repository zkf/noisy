module Data.BanditSolver.LTS (runAveragedLts, GaussianArms(..)) where

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

instance Arms GaussianArms where

    getReward (GaussianArms arms) idx = gaussian (arms ! idx)
    
    selectArm (GaussianArms arms) = do
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
                
    updateSelectedArm (GaussianArms arms) index ob reward = 
        let (mu, sigma) = arms ! index
            armVariance = sigma**2
            obVariance  = ob**2
            mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
            sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
            arm'   = (mu', sigma')
            arms' =  V.modify (\v -> write v index arm') arms
        in (GaussianArms arms')

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma


class Arms as where
    selectArm :: as -> State PureMT Int
    updateSelectedArm :: as -> Int -> ObNoise -> Double -> as
    getReward :: as -> Int -> State PureMT Double
    
newtype GaussianArms = GaussianArms (V.Vector GaussianArm) deriving Show
newtype BanditSolver as = BanditSolver as

type GaussianArm = (Double, Double) -- Mean, standard deviation
type ObNoise = Double
type Reward = Double

getIntermediates :: [a] -> [a]
getIntermediates = map last . splitPlaces ([10,40,50,400,500,1000,3000,5000,90000] :: [Int])

runAveragedLts :: (Arms a)  =>
       a       -- Real arms
       -> a    -- beginning arm estimates
       -> Int  -- Rounds
       -> Int  -- Repetitions
       -> ObNoise
       -> PureMT
       -> [(Int, ObNoise, Double, Double)] -- after n rounds, obNoise, mean, stddev
runAveragedLts arms armEstimates rounds reps obNoise gen = meanStdDev
    where meanStdDev = runManyLtss arms rounds reps ltsProto obNoise gen
          ltsProto = BanditSolver armEstimates

runManyLtss :: (Arms a) =>  
    a -> Int -> Int -> BanditSolver a -> ObNoise -> PureMT -> [(Int, ObNoise,  Double, Double)]
runManyLtss realArms rounds reps lts ob gen = 
          let results = evalState (replicateM reps $ runLts realArms rounds ob lts) gen :: [[(Int, Double)]]
              ms (roundN, creward) = let (mean, variance) =  meanVariance . V.fromList $ creward
                                    in  (roundN, ob, mean, sqrt variance)
              tr = transpose results
              ids = map (head . (map fst)) tr
              vals = map ((map snd)) tr
              r = zip ids vals
          in map ms r

runLts :: Arms a => a -> Int -> ObNoise -> BanditSolver a -> State PureMT [(Int, Double)]
runLts realArms rounds ob startingLts = do
    rewards <- evalStateT (replicateM rounds (pullArm realArms ob)) startingLts
    return (getIntermediates $ zip [1..] (scanl1 (+) rewards))
            
pullArm :: Arms a => a -> ObNoise -> StateT (BanditSolver a) (State PureMT) Reward
pullArm realArms ob = do
    (BanditSolver arms) <- get
    selected                     <- lift $ selectArm arms 
    reward                       <- lift $ getReward realArms selected
    let arms' = updateSelectedArm arms selected ob reward 
    put (BanditSolver arms')
    return reward

