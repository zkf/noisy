module Data.BanditSolver.LTS (findBestObservationNoise) where

import Data.Random.Normal
import System.Random (RandomGen, split)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (State, StateT, execStateT, evalState, state, get,
                                put, lift)
import Control.Parallel.Strategies (parMap, rseq)


type Mu = Double
type Sigma = Double
type Arm = (Mu, Sigma)
type Arms = [Arm]
type ObNoise = Double
type CReward = Double
type LTS = (Arms, CReward, [Int])

-- Number of LTSs to run in parallel
numLtss :: Int
numLtss = 1000

parallelize :: (a -> b) -> [a] -> [b] 
parallelize fun list = parMap rseq fun list

average :: (Fractional a) => [a] -> a
average list = (sum list) / (fromIntegral $ length list)

findBestObservationNoise :: 
    RandomGen g =>
       Arms
       -> Arm
       -> Int       -- Rounds
       -> [ObNoise]
       -> g
       -> [(ObNoise, CReward)]
findBestObservationNoise arms armEstimate rounds obNoiseRange gen =
    go ltsProto arms rounds obNoiseRange gen
    where ltsProto = (replicate (length arms) armEstimate, 0, []) 

go :: RandomGen g =>
         LTS -> Arms -> Int -> [ObNoise] -> g -> [(ObNoise, CReward)]
go ltsProto realArms rounds obNoiseRange gen = 
    zipWith (runParallelLtss realArms rounds ltsProto) obNoiseRange gens
    where gens = genGens gen
     
genGens :: RandomGen g => g -> [g] 
genGens gen = iterate (\g -> snd $ split g) gen
    
runParallelLtss :: (RandomGen g) => Arms -> Int -> LTS -> ObNoise -> g -> (ObNoise, CReward)
runParallelLtss realArms rounds lts ob gen = (ob, avgReward)
    where avgReward = average $
                parallelize getCReward $
                zipWith evalState ltss gens
          getCReward (_, r, _) = r
          ltss = map (runLts realArms rounds ob) $ replicate numLtss lts
          gens = genGens gen

runLts :: RandomGen g => Arms -> Int -> ObNoise -> LTS -> State g LTS
runLts realArms rounds ob startingLts =
    execStateT (replicateM rounds (pullArm realArms ob)) startingLts
            
pullArm ::  (RandomGen g) => Arms -> ObNoise -> StateT LTS (State g) ()
pullArm realArms ob = do
    (arms, cReward, selections) <- get
    (selectedArm, index)        <- lift $ selectArm arms 
    reward                      <- lift $ getReward realArms index
    let arms' = updateSelectedArm arms selectedArm index ob reward 
    put (arms', cReward + reward, (index:selections))

selectArm :: (RandomGen g) => Arms -> State g (Arm, Int)
selectArm arms = do
    probs <- evalArms arms
    let index = snd . maximum $ zip probs [0..]
        arm  = arms !! index 
    return (arm, index)

updateSelectedArm :: Arms -> Arm -> Int -> ObNoise -> Double -> Arms
updateSelectedArm arms (mu, sigma) index ob reward = 
    let armVariance = sigma**2
        obVariance  = ob**2
        mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        arm'   = (mu', sigma')
        arms' = map (\(i, a) -> if i == index
                                then arm'  
                                else a
                 ) $ zip [0..] arms
    in arms'
    
evalArms :: (RandomGen g) => Arms -> State g [Double]
evalArms = mapM gaussian

getReward :: (RandomGen g) => Arms -> Int -> State g Double
getReward realArms index = gaussian (realArms !! index)

gaussian :: (RandomGen g) => (Double, Double) -> State g Double
gaussian params = state $ normal' params

-- pullArm ob (arms, cReward, selections) = 
--     selectArm arms >>= \index -> getReward index >>= \reward -> updateLts (

