module Data.BanditSolver.LTS where

import Data.Random.Normal
import System.Random (RandomGen, StdGen, getStdGen, split)
import Control.Monad (liftM, replicateM)
import Control.Monad.State.Strict (State, StateT, execStateT, evalState, state, get,
                                put, lift)
import Control.Parallel.Strategies (parMap, rseq)


type Mu = Double
type Sigma = Double
type Arm = (Mu, Sigma)
type Arms = [Arm]
type Bandit = Arms
type ObNoise = Double
type CReward = Double
type LTS = (Arms, CReward, [Int])

parallelize :: (a -> b) -> [a] -> [b] 
parallelize fun list = parMap rseq fun list

average :: (Fractional a) => [a] -> a
average list = (sum list) / (fromIntegral $ length list)

-- The actual arms
arms = [(5.0, 2.0), (2.0, 2.0), (3.0, 5.0)] :: Arms
numArms = length arms

-- Starting estimates used by LTSs
armEstimates = replicate numArms $ (3.5, 3.0) :: Arms

ltsProto = (armEstimates, 0, []) :: LTS
-- How many rounds an LTS should run for
rounds = 100

-- Number of LTSs to run in parallel
numLtss = 1000

-- obNoise = 5.0
-- obNoiseRange = [0.00,0.125 .. 10.0]


go obNoiseRange gen = 
    zipWith (runParallelLtss numLtss ltsProto) obNoiseRange gens
    where gens = genGens gen
      
genGens gen = iterate (\g -> snd $ split g) gen
    
runParallelLtss :: (RandomGen g) => Int -> LTS -> ObNoise -> g -> (ObNoise, CReward)
runParallelLtss count lts ob gen = (ob, avgReward)
    where avgReward = average $
                parallelize getCReward $
                zipWith evalState ltss gens
          getCReward (_, r, _) = r
          ltss = map (runLts rounds ob) $ replicate count lts
          gens = genGens gen

runLts :: RandomGen g => Int -> ObNoise -> LTS -> State g LTS
runLts rounds ob startingLts =
    execStateT (replicateM rounds (pullArm ob)) startingLts
            
pullArm ::  (RandomGen g) => ObNoise -> StateT LTS (State g) ()
pullArm ob = do
    (arms, cReward, selections) <- get
    (selectedArm, index)        <- lift $ selectArm arms 
    reward                      <- lift $ getReward index
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
    
evalArms :: (RandomGen g) => [Arm] -> State g [Double]
evalArms = 
   mapM gaussian

getReward :: (RandomGen g) => Int -> State g Double
getReward index = gaussian (arms !! index)

gaussian :: (RandomGen g) => (Double, Double) -> State g Double
gaussian params = state $ normal' params

-- pullArm ob (arms, cReward, selections) = 
--     selectArm arms >>= \index -> getReward index >>= \reward -> updateLts (

