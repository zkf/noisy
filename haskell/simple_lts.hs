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


parallelize fun list = parMap rseq fun list

average list = (sum list) / (fromIntegral $ length list)

armEstimates = replicate 2 $ (3.5, 3.0)
rounds = 100
numBandits = 250
-- obNoise = 5.0
-- obNoiseRange = [0.00,0.125 .. 10.0]

-- The actual arms
arms :: Arms
arms = [(5.0, 2.0), (2.0, 2.0), (3.0, 5.0)]

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
    map (runManyLtss ob) ltss
    where ltss = map makeLtss obNoiseRange (genGens gen)
      
makeLtss :: ObNoise -> StdGen -> [LTS]
makeLtss ob gen = 
    zipWith expand startingBandits (genGens gen)
    where expand arms gen = (arms, 0, [], gen)
          startingBandits = replicate numBandits armEstimates

genGens gen = iterate (\g -> snd $ split g) gen


-- Non-state version
 
runParallelLtss :: ObNoise -> [LTS] -> (ObNoise, Double)
runParallelLtss ob ltss =
    let go = map (runLts rounds ob) ltss
        getCReward (_, r, _) = r
        avgReward = average $ parallelize getCReward go
    in (ob, avgReward)
    
--     let cRews = parallelize ((\(_,_,c,_,_) -> c).last.take rounds.iterate pullArm) $ bandits
--         -- just get ob from first bandit
--         ob    = (\(_,ob,_,_,_) -> ob) (bandits !! 0)
--     in (ob, average cRews)
    
runLts rounds ob startingLts =
    iterate (pullArm ob startingLts)

    
pullArm :: ObNoise ->  LTS -> LTS
pullArm ob (arms, cReward, selections, gen) = 
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
    in (arms', cReward + reward, (index:selections), gen'')

evalArms :: (RandomGen g) => [Arm] -> g -> ([Double], g)
evalArms arms gen = 
    let (gen1, gen2) = split gen
    in  (go arms gen1, gen2)
    where
        go [] _ = []
        go (arm:as) g = 
            let (sample, g') = gaussian arm g
            in  sample:(go as g')
            
getReward :: Int -> StdGen -> (Double, StdGen)
getReward index gen = 
     gaussian (arms!!index) gen
 
gaussian :: (RandomGen g) => (Double, Double) -> g -> (Double, g)
gaussian = normal'

    
---

updateArm :: Arm -> ObNoise -> Double -> Arm
updateArm (mu, sigma) ob reward = 
    let
        armVariance = sigma**2
        obVariance  = ob**2
        mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
    in (mu', sigma')
    
---- State version

runManyLtssSt :: (RandomGen g) => ObNoise -> [LTS2] -> [g] -> (ObNoise, CReward)
runManyLtssSt ob ltss gens = 
    let ready = map (runLts rounds ob) ltss 
        go =  zipWith evalState ready gens :: [LTS2]
        getCReward (_, r, _) = r
        avgReward = average $ parallelize getCReward go
    in (ob, avgReward)

runLtsSt :: RandomGen g => Int -> ObNoise -> LTS2 -> State g LTS2
runLtsSt rounds ob startingLts =
    execStateT (replicateM rounds (pullArmSt ob)) startingLts
            
pullArmSt ::  (RandomGen g) => ObNoise -> StateT LTS2 (State g) ()
pullArmSt ob = do
    (arms, cReward, selections) <- get
    probs <- lift $ evalArmsSt arms
    let index = snd . maximum $ zip probs [0..]
        arm  = arms !! index 
    reward <- lift $ getRewardSt index
    let arm' = updateArm arm ob reward 
        arms' = map (\(i, e) -> if i == index
                                then arm'  
                                else e
                 ) $ zip [0..] arms
    put (arms', cReward + reward, (index:selections))

evalArmsSt :: (RandomGen g) => [Arm] -> State g [Double]
evalArmsSt = 
   mapM (\params -> gaussianSt params) 

getRewardSt :: (RandomGen g) => Int -> State g Double
getRewardSt index = gaussianSt (arms !! index)

gaussianSt :: (RandomGen g) => (Double, Double) -> State g Double
gaussianSt params = state $ gaussian params

-- pullArmSt ob (arms, cReward, selections) = 
--     selectArm arms >>= \index -> getRewardSt index >>= \reward -> updateLts (

