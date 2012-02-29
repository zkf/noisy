-- module Lts where 

import Data.Random.Normal
import System.Random (RandomGen, getStdGen, split)
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
type LTS = (Arms, CReward, [Int])


parallelize fun list = parMap rseq fun list

average list = (sum list) / (fromIntegral $ length list)

gaussian :: (RandomGen g) => (Double, Double) -> g -> (Double, g)
gaussian = normal'

-- The actual arms
arms :: Arms
arms = [(5.0, 2.0), (2.0, 2.0), (3.0, 5.0)]
numArms = length arms

-- Starting estimates used by LTSs
armEstimates = replicate numArms $ (3.5, 3.0)

ltsProto = (armEstimates, 0, []) :: LTS
-- How many rounds an LTS should run for
rounds = 100

-- Number of LTSs to run in parallel
numLtss = 1000

-- obNoise = 5.0
-- obNoiseRange = [0.00,0.125 .. 10.0]


main = do
    [obStart, obEnd, obStep] <- liftM (map read) getArgs
    gen <- getStdGen
    let results = go [obStart, obStart+obStep .. obEnd] gen
    putStrLn $ "Observation noise,Average cumulative reward over " ++ show rounds ++" rounds"
    mapM_ (putStrLn.(\(ob, r) -> show ob ++ "," ++ show r)) results
--    mapM_ (printfun) results

-- printfun cReward = do
--    --mapM_ (putStrLn) $ map (("Selected arm " ++) . show) (reverse selections)
--    putStrLn $ "Cumulative reward: " ++ show cReward
--        ++ " after " ++ show rounds
--        ++ " rounds using ob: " ++ show obNoise

go obNoiseRange gen = 
    zipWith (runParallelLtss numLtss ltsProto) obNoiseRange gens
    where gens = genGens gen
      

runParallelLtss :: (RandomGen g) => Int ->  LTS -> ObNoise -> g -> (ObNoise, Double)
runParallelLtss count lts ob gen = (ob, avgReward)
    where 
        avgReward = average $ parallelize getCReward $ zipWith (runLts rounds ob) ltss gens
        getCReward (_, r, _) = r
        ltss = replicate count lts
        gens = genGens gen
        
genGens gen = iterate (\g -> snd $ split g) gen

runLts :: (RandomGen g) => Int -> ObNoise -> LTS -> g -> LTS
runLts rounds ob startingLts gen =
    discardRng . last . take rounds $ iterate (pullArm ob) (startingLts, gen)
    where discardRng = fst
    
pullArm :: (RandomGen g) => ObNoise -> (LTS, g) -> (LTS, g)
pullArm ob ((arms, cReward, selections), gen) = 
    let
        -- Select arm randomly and get index of it
        (probs, gen') = evalArms arms gen
        index = snd . maximum $ zip probs [0..]
        chosenArm = arms !! index
        (reward, gen'') = getReward index gen'

        arm' = updateArm chosenArm ob reward
        arms'= map (\(i, e) -> if i == index
                                  then arm'  
                                  else e
                   ) $ zip [0..] arms
    in ((arms', cReward + reward, (index:selections)), gen'')

updateArm :: Arm -> ObNoise -> Double -> Arm
updateArm (mu, sigma) ob reward = 
    let
        armVariance = sigma**2
        obVariance  = ob**2
        mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
    in (mu', sigma')

evalArms :: (RandomGen g) => [Arm] -> g -> ([Double], g)
evalArms arms gen = 
    let (gen1, gen2) = split gen
    in  (go arms gen1, gen2)
    where
        go [] _ = []
        go (arm:as) g = 
            let (sample, g') = gaussian arm g
            in  sample:(go as g')
            
getReward :: (RandomGen g) => Int -> g -> (Double, g)
getReward index gen = 
     gaussian (arms!!index) gen

