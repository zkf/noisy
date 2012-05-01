{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Data.BanditSolver.LTS 

(   Environment(..), Solver(..), GaussianArms, GaussianArm(..), LTS(..), GA,
    runAveragedLTS, -- runOne,runAveragedInstantRewards 
    updateLTS, makeLTS, -- runAvg, 
    makeGaussianArm
) where

import Control.Monad.Writer
import Control.Monad.State
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT)
-- import System.Random.MWC
-- import System.Random.MWC.Distributions (normal)
import Data.RVar (sampleRVar)
import qualified Statistics.Sample as S (meanVarianceUnb)
import Data.Vector.Generic.Mutable (write)
import Control.Parallel (pseq)
import Control.Concurrent.Chan.Strict
import Control.Concurrent (forkIO, getNumCapabilities)
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.List
import Control.Exception (evaluate)

type ActionId = Int
type Reward   = Double
data GaussianArm = GaussianArm !Double !Double deriving (Show)-- Mean, standard deviation
type GaussianArms = V.Vector GaussianArm
data LTS = LTS !GaussianArms !Double !Double deriving (Show)-- arms, cumulative reward, obnoise

type GA = (Double, Double)
makeGaussianArm :: GA -> GaussianArm
makeGaussianArm (mu, sigma) = GaussianArm mu sigma

makeLTS :: GaussianArm -> Int -> Double -> LTS
makeLTS armEstimate numArms ob = LTS startEstimates 0 ob
  where startEstimates = V.fromList $ replicate numArms armEstimate

class Environment e where
    getReward :: e -> ActionId -> StateT PureMT IO Reward
    
class Solver s where
    {-# INLINE select #-}
    select :: s -> StateT PureMT IO ActionId
    {-# INLINE update #-}
    update :: s -> ActionId -> Reward -> s
    continue :: s -> Bool -- condition on which to stop early
    getArms :: s -> GaussianArms
    getCumulativeReward :: s -> Double
    getObservationNoise :: s -> Double
    make :: GaussianArms -> Double -> Double -> s
    select = selectArm
    update = updateLTS
    continue _ = True

instance Environment GaussianArms where 
    {-# INLINE getReward #-}
    getReward arms idx = gaussian (arms ! idx)

instance Solver LTS where
    make = LTS
    getArms (LTS arms _ _) = arms
    getCumulativeReward (LTS _ r _) = r
    getObservationNoise (LTS _ _ ob) = ob

{-
runOne :: (Environment e, Solver s) => e -> Int -> s -> State PureMT s
runOne realArms rounds startSolver = run 0 startSolver
    where run n !solver 
            | n == rounds = return solver
            | otherwise   = do
                solver' <- fst `liftM` oneRound realArms solver
                if continue solver' 
                    then run (n+1) solver'
                    else return solver'

runAvg :: (Environment e, Solver s) => e -> Int -> Int -> s -> State PureMT Double
runAvg realArms n rounds startSolver = do
    res <- mapM (runOne realArms rounds) (replicate n startSolver)
    return . mean $ map getCumulativeReward res
-}
mean :: Fractional a => [a] -> a
mean = go 0 0
  where go len num [] = num / len
        go len num (x:xs) = go (len + 1) (num + x) xs
{-
runAveragedLTS :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> PureMT -> [(Int, Double, Double, Double)] -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTS bestArm badArm armEstimate numArms rounds repetitions ob randomgen = 
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        solvers = replicate repetitions (makeLTS myArmEstimate numArms ob)
        forceW ((!a,!b,!c,!d):xs) = (a,b,c,d) `pseq` force xs
        forceW []                 = ()
        result = snd $ evalState (runWriterT $ runEnsemble realArms rounds solvers) randomgen
    in forceW result `seq` result

runEnsemble :: (Environment e, Solver s) =>  e -> Int -> [s] ->
    WriterT [(Int, Double, Double, Double)] (State PureMT) () 
runEnsemble environment rounds startSolvers = run 1 startSolvers
  where run n solvers 
            | n > rounds = return ()
            | otherwise = do
                 solvers' <- lift $ mapM (liftM fst . oneRound environment) solvers
                 writeLog solvers' n
                 force solvers' `seq` run (n + 1) solvers'
        checkpoints = makeCheckpoints rounds
        writeLog s n = when (n `elem` checkpoints) $
            let crewards    = map getCumulativeReward s
                (myMean, var) = S.meanVarianceUnb $ V.fromList crewards
                ob          = getObservationNoise $ head s
            in tell [(n, ob, myMean, sqrt var)]
-}

runAveragedLTS :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> WriterT [(Int, Double, Double)] IO () -- Checkpoint, ob, mean of cumulative reward
runAveragedLTS bestArm badArm armEstimate numArms rounds repetitions ob = do
    threads <- liftIO getNumCapabilities
    let chunk = repetitions `div` threads
    chans <- liftIO $ replicateM threads newChan
    _ <- liftIO $ mapM (forkIO . runAveragedLTSIO bestArm badArm armEstimate numArms rounds chunk ob) chans
    forM_ (makeCheckpoints rounds) $ \n -> do
        results <- liftIO $ mapM readChan chans
        let m = sum results / fromIntegral repetitions
        tell [(n, ob, m)]
    return ()

runAveragedLTSIO :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> Chan Double -> IO () -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTSIO bestArm badArm armEstimate numArms rounds repetitions ob chan = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        solvers = replicate repetitions (makeLTS myArmEstimate numArms ob)
        forceW ((!a,!b,!c,!d):xs) = (a,b,c,d) `pseq` force xs
        forceW []                 = ()
    gen <- newPureMT
    evalStateT (runEnsembleIO realArms rounds solvers chan) gen

runEnsembleIO :: (Environment e, Solver s) =>  e -> Int -> [s] ->
    Chan Double -> StateT PureMT IO ()
runEnsembleIO environment rounds startSolvers chan = run 1 startSolvers
  where run :: Solver s => Int -> [s] -> StateT PureMT IO () 
        run n solvers
            | n > rounds = return ()
            | otherwise = do
                 solvers' <- mapM (liftM fst . oneRound environment) solvers
                 liftIO $ writeLog solvers' n
                 run (n + 1) solvers'
        checkpoints = makeCheckpoints rounds
        writeLog s n = when (n `elem` checkpoints) $
            let crewards      = sum $ map getCumulativeReward s
            in evaluate crewards >>= writeChan chan
{-
runAveragedInstantRewards :: GA -> GA -> GA -> Int
    -> Int -> Int -> Double -> PureMT -> [(Int, Reward, Double)]
runAveragedInstantRewards bestArm badArm armEstimate numArms rounds repetitions ob gen =
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agents = replicate repetitions (makeLTS myArmEstimate numArms ob)
        result = snd $ evalState 
                    (runWriterT $ runInstantRewards realArms agents rounds) gen
    in result

runInstantRewards :: (Environment e, Solver s) => e -> [s] -> Int 
    -> WriterT [(Int, Reward, Double)] (State PureMT) ()
runInstantRewards environment startAgents rounds = run 1 startAgents
  where run n agents
            | n > rounds  = return ()
            | otherwise   = do
                (agents', rewards) <- lift $ mapAndUnzipM (oneRound environment) agents
                writeLog rewards n
                force agents' `seq` run (n + 1) agents'
        checkpoints = makeCheckpoints rounds
        writeLog rewards n = when (n `elem` checkpoints) $
            let (m, var) = S.meanVarianceUnb $ V.fromList rewards
            in tell [(n, m, sqrt var)]
-}
makeCheckpoints :: Int -> [Int]
makeCheckpoints rounds =
    nub
    . sort
    $ rounds : [c | y <- [1..99]
          , x <- [0 :: Int .. floor ( logBase 10 (fromIntegral rounds) - 1 :: Double)]
          , let c = y * 10^x
          , c < rounds]

force :: [a] -> () 
force (x:xs) = x `pseq` force xs
force []     = ()

{-# INLINE oneRound #-}
oneRound :: (Environment e, Solver s) => e -> s -> StateT PureMT IO (s, Reward)
oneRound env solver = do
    selected <- select solver
    reward   <- getReward env selected
    let !solver' = update solver selected reward
    return (solver', reward)

{-# INLINE selectArm #-}
selectArm :: Solver s => s -> StateT PureMT IO ActionId
selectArm solver = do
   let arms = getArms solver
       start = V.length arms - 1
       go :: ActionId -> Double -> ActionId -> StateT PureMT IO ActionId
       go 0 maxValue index = do
           newValue <- gaussian (arms ! 0)
           if newValue > maxValue 
               then return 0
               else return index 
       go n maxValue index = do
           newValue <- gaussian (arms ! n)
           if newValue > maxValue 
               then go (n - 1) newValue n
               else go (n - 1) maxValue index
   firstValue <- gaussian (arms ! start)
   go (start - 1) firstValue start

-- Alternative to the `go' function above is this:
--      V.maxIndex `fmap` V.mapM gaussian arms
-- The downside is that it is much slower.


{-# INLINE updateLTS #-}
updateLTS :: Solver s => s -> ActionId -> Reward -> s
updateLTS solver !index !reward = 
    let arms    = getArms solver
        creward = getCumulativeReward solver
        ob      = getObservationNoise solver
        (GaussianArm mu sigma) = arms ! index
        armVariance = sigma * sigma
        obVariance  = ob * ob
        !mu'      = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        !sigma'   = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        !arms'    =  V.modify (\v -> write v index (GaussianArm mu' sigma')) arms
        !creward' = creward + reward
    in make arms' creward' ob

gaussian :: GaussianArm -> StateT PureMT IO Reward
gaussian (GaussianArm mu sigma) =
    sampleRVar $ normal mu sigma
