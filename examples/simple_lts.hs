import Data.Random.Normal
import System.Random (StdGen, getStdGen, split, randomR)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rseq)

parallelize fun list = parMap rseq fun list

armEstimates = replicate 2 $ (3.5, 3.0)
obNoise = 5.0
rounds = 10
main = do
    gen <- getStdGen
    let gens = iterate (\g -> snd $ split g) gen
        bandits = zipWith (\(a, ob) g -> (a, ob, 0, [], g))
                    (replicate 1 (armEstimates, obNoise))
                    gens
        results =  parallelize ((\(_,_,c,s,_) ->(c,s)).last.take rounds.iterate pullArm) $ bandits
    mapM_ (printfun) results

printfun (cumReward, selections) = do
    mapM_ (putStrLn) $ map (("Selected arm " ++) . show) (reverse selections)
    putStrLn $ "Cumulative reward: " ++ show cumReward
        ++ " after " ++ show rounds
        ++ " rounds using ob: " ++ show obNoise

type Mu = Double
type Sigma = Double
type Arm = (Mu, Sigma)
type Arms = [Arm]
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
        index = snd . maximum $ zip  probs [0..]
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
