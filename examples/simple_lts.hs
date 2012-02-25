import Data.Random.Normal
import System.Random (StdGen, getStdGen, split, randomR)
import Control.Monad (liftM)
import Control.Parallel.Strategies (parMap, rseq)
import System.Environment (getArgs)

parallelize fun list = parMap rseq fun list

average list = (sum list) / (fromIntegral $ length list)

armEstimates = replicate 2 $ (3.5, 3.0)
-- obNoise = 5.0
-- obNoiseRange = [0.00,0.125 .. 10.0]
rounds = 100
numBandits = 250
main = do
    [obStart, obEnd, obStep] <- liftM (map read) getArgs
    gen <- getStdGen
    let results = go [obStart, obStart+obStep .. obEnd] gen
    putStrLn $ "Observation noise,Average cumulative reward over " ++ show rounds ++" rounds"
    mapM_ (putStrLn.(\(ob, r) -> show ob ++ "," ++ show r)) results
--    mapM_ (printfun) results

-- printfun cumReward = do
--    --mapM_ (putStrLn) $ map (("Selected arm " ++) . show) (reverse selections)
--    putStrLn $ "Cumulative reward: " ++ show cumReward
--        ++ " after " ++ show rounds
--        ++ " rounds using ob: " ++ show obNoise
      
makeBandits :: ObNoise -> StdGen -> [LTS]
makeBandits ob gen = 
    zipWith expand startingBandits (genGens gen)
    where expand = \(a, ob) g -> (a, ob, 0, [], g)
          startingBandits = replicate numBandits (armEstimates, ob)

runBandits :: [LTS] -> (ObNoise, Double)
runBandits bandits =
    let cRews = parallelize ((\(_,_,c,_,_) -> c).last.take rounds.iterate pullArm) $ bandits
        -- just get ob from first bandit
        ob    = (\(_,ob,_,_,_) -> ob) (bandits !! 0)
    in (ob, average cRews)
    
go obNoiseRange gen = 
    map runBandits bandits
    where bandits = zipWith makeBandits obNoiseRange (genGens gen)
    
genGens gen = iterate (\g -> snd $ split g) gen

type Mu = Double
type Sigma = Double
type Arm = (Mu, Sigma)
type Arms = [Arm]
type Bandit = Arms
type ObNoise = Double
type CumReward = Double
type LTS = (Arms, ObNoise, CumReward, [Int], StdGen)

gaussian = normal'

evalArms :: [Arm] -> StdGen -> ([Double], StdGen)
evalArms arms gen = 
    let (gen1, gen2) = split gen
    in  (go arms gen1, gen2)
    where
        go [] g = []
        go (arm:as) g = 
            let (sample, g') = gaussian arm g
            in  sample:(go as g')

pullArm :: LTS -> LTS
pullArm (arms, ob, cumReward, selections, gen) = 
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
    in (arms', ob, cumReward+reward, (index:selections), gen'')

updateArm :: Arm -> ObNoise -> Double -> Arm
updateArm (mu, sigma) ob reward = 
    let
        armVariance = sigma**2
        obVariance  = ob**2
        mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
    in (mu', sigma')

arms :: Arms
arms = [(5.0, 2.0), (2.0, 2.0)]
getReward :: Int -> StdGen -> (Double, StdGen)
getReward index gen = 
    let (reward, gen') = gaussian (arms!!index) gen
    in  (reward, gen') 
