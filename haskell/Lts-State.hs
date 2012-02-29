module Lts.State  where 

import Data.Random.Normal
import System.Random (RandomGen, StdGen, getStdGen, split)
import Control.Monad (liftM, replicateM)
import Control.Monad.State (State, StateT, execStateT, evalState, state, get,
                                put, lift)
import Control.Parallel.Strategies (parMap, rseq)
import System.Environment (getArgs)


type Mu = Double
type Sigma = Double
type Arm = (Mu, Sigma)
type Arms = [Arm]
type Bandit = Arms
type ObNoise = Double
type CReward = Double
type LTS = (Arms, CReward, [Int], StdGen)
type LTS2 = (Arms, CReward, [Int])

parallelize :: (a -> b) -> [a] -> [b] 
parallelize fun list = parMap rseq fun list

average :: (Fractional a) => [a] -> a
average list = (sum list) / (fromIntegral $ length list)

armEstimates :: Arms
armEstimates = replicate 2 $ (3.5, 3.0)

rounds = 100 :: Int
numBandits = 250 :: Int

-- obNoise = 5.0
-- obNoiseRange = [0.00,0.125 .. 10.0]

-- The actual arms
arms :: Arms
arms = [(5.0, 2.0), (2.0, 2.0), (3.0, 5.0)]

runManyLtss :: (RandomGen g) => ObNoise -> [LTS2] -> [g] -> (ObNoise, CReward)
runManyLtss ob ltss gens = 
    let ready = map (runLts rounds ob) ltss 
        go =  zipWith evalState ready gens :: [LTS2]
        getCReward (_, r, _) = r
        avgReward = average $ parallelize getCReward go
    in (ob, avgReward)

runLts :: RandomGen g => Int -> ObNoise -> LTS2 -> State g LTS2
runLts rounds ob startingLts =
    execStateT (replicateM rounds (pullArm ob)) startingLts
            
pullArm ::  (RandomGen g) => ObNoise -> StateT LTS2 (State g) ()
pullArm ob = do
    (arms, cReward, selections) <- get
    probs <- lift $ evalArms arms
    let index = snd . maximum $ zip probs [0..]
        arm  = arms !! index 
    reward <- lift $ getReward index
    let arm' = updateArm arm ob reward 
        arms' = map (\(i, e) -> if i == index
                                then arm'  
                                else e
                 ) $ zip [0..] arms
    put (arms', cReward + reward, (index:selections))

updateArm :: Arm -> ObNoise -> Double -> Arm
updateArm (mu, sigma) ob reward = 
    let
        armVariance = sigma**2
        obVariance  = ob**2
        mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
    in (mu', sigma')
    
evalArms :: (RandomGen g) => [Arm] -> State g [Double]
evalArms = 
   mapM (\params -> gaussian params) 

getReward :: (RandomGen g) => Int -> State g Double
getReward index = gaussian (arms !! index)

gaussian :: (RandomGen g) => (Double, Double) -> State g Double
gaussian params = state $ normal' params

-- pullArm ob (arms, cReward, selections) = 
--     selectArm arms >>= \index -> getReward index >>= \reward -> updateLts (

