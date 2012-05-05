{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Data.BanditSolver.LTS 

(   Environment(..), Solver(..), GaussianArms, GaussianArm(..), LTS(..), GA,
    runAveragedLTS, runOne,runAveragedInstantRewards,
    updateLTS, makeLTS, runAvg, 
    makeGaussianArm
) where

import Control.Monad.Writer
import Control.Monad.State
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT)
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
import Data.Random (MonadRandom)

type ActionId = Int
type Reward   = Double
data GaussianArm = GaussianArm !Double !Double deriving (Show)-- Mean, standard deviation
type GaussianArms = V.Vector GaussianArm
data LTS = LTS !GaussianArms !Double !Double deriving (Show)-- arms, cumulative reward, obnoise
type RandomStateIO = StateT PureMT IO
type RandomState = State PureMT

type GA = (Double, Double)
makeGaussianArm :: GA -> GaussianArm
makeGaussianArm (mu, sigma) = GaussianArm mu sigma

makeLTS :: GaussianArm -> Int -> Double -> LTS
makeLTS armEstimate numArms ob = LTS startEstimates 0 ob
  where startEstimates = V.fromList $ replicate numArms armEstimate

class Environment e where
    getReward :: (MonadRandom m) => e -> ActionId -> m Reward
    
class Solver s where
    select :: (MonadRandom m) => s -> m ActionId
    update :: s -> ActionId -> Reward -> s
    continue :: s -> Bool -- condition on which to stop early
    getArms :: s -> GaussianArms
    getCumulativeReward :: s -> Double
    getObservationNoise :: s -> Double
    make :: GaussianArms -> Double -> Double -> s
    {-# INLINE select #-}
    select = selectArm
    {-# INLINE update #-}
    update = updateLTS
    continue _ = True

instance Environment GaussianArms where 
    {-# SPECIALIZE INLINE getReward :: GaussianArms -> ActionId -> RandomStateIO Reward #-}
    {-# SPECIALIZE INLINE getReward :: GaussianArms -> ActionId -> RandomState Reward #-}
    {-# INLINE getReward #-}
    getReward arms idx = gaussian (arms ! idx)

instance Solver LTS where
    make = LTS
    getArms (LTS arms _ _) = arms
    getCumulativeReward (LTS _ r _) = r
    getObservationNoise (LTS _ _ ob) = ob


{-# SPECIALIZE INLINE runOne :: (Environment e, Solver s) => e -> Int -> s -> RandomState s #-}
runOne :: (MonadRandom m, Environment e, Solver s) => e -> Int -> s -> m s
runOne realArms rounds startSolver = run 0 startSolver
    where run n !solver 
            | n == rounds = return solver
            | otherwise   = do
                solver' <- fst `liftM` oneRound realArms solver
                if continue solver' 
                    then run (n+1) solver'
                    else return solver'

{-# SPECIALIZE INLINE runAvg :: (Environment e, Solver s) => e -> Int -> Int 
 -> s -> RandomState Reward #-}
runAvg :: (MonadRandom m, Environment e, Solver s) => e -> Int -> Int -> s -> m Reward
runAvg realArms repetitions rounds startSolver = do
    res <- mapM (runOne realArms rounds) (replicate repetitions startSolver)
    return . mean $ map getCumulativeReward res

mean :: Fractional a => [a] -> a
mean = go 0 0
  where go len num [] = num / len
        go len num (x:xs) = go (len + 1) (num + x) xs

runAveragedLTS :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> WriterT [(Int, Double, Double)] IO () -- Checkpoint, ob, mean of cumulative reward
runAveragedLTS bestArm badArm armEstimate numArms rounds repetitions ob = do
    threads <- lift getNumCapabilities
    let chunk = repetitions `div` threads
    chans <- lift $ replicateM threads newChan
    _ <- lift $ mapM (forkIO . runAveragedLTSIO bestArm badArm armEstimate numArms rounds chunk ob) chans
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
    gen <- newPureMT
    evalStateT (runEnsembleIO realArms rounds solvers chan) gen

runEnsembleIO :: (Environment e, Solver s) =>  e -> Int -> [s] ->
    Chan Double -> RandomStateIO ()
runEnsembleIO environment rounds startSolvers chan = run 1 startSolvers
  where run :: Solver s => Int -> [s] -> RandomStateIO () 
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

runAveragedInstantRewards :: GA -> GA -> GA -> Int
    -> Int -> Int -> Double -> PureMT -> [(Int, Reward, Double)]
runAveragedInstantRewards bestArm badArm armEstimate numArms rounds repetitions ob gen =
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agents = replicate repetitions (makeLTS myArmEstimate numArms ob)
    in  evalState (execWriterT $ runInstantRewards realArms agents rounds) gen

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

{-# SPECIALIZE INLINE oneRound :: GaussianArms -> LTS -> RandomStateIO (LTS, Reward) #-}
{-# SPECIALIZE INLINE oneRound :: GaussianArms -> LTS -> RandomState (LTS, Reward) #-}
{-# INLINE oneRound #-}
oneRound :: (MonadRandom m, Environment e, Solver s) => e -> s -> m (s, Reward)
oneRound env solver = do
    selected <- select solver
    reward   <- getReward env selected
    let !solver' = update solver selected reward
    return (solver', reward)

{-# SPECIALIZE INLINE selectArm :: LTS -> RandomStateIO ActionId #-}
{-# SPECIALIZE INLINE selectArm :: LTS -> RandomState ActionId #-}
{-# INLINE selectArm #-}
selectArm :: (MonadRandom m, Solver s) => s -> m ActionId
selectArm solver = do
   let arms = getArms solver
       start = V.length arms - 1
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


{-# SPECIALIZE INLINE updateLTS :: LTS -> ActionId -> Reward -> LTS #-}
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

{-# SPECIALIZE INLINE gaussian :: GaussianArm -> RandomStateIO Reward #-}
{-# SPECIALIZE INLINE gaussian :: GaussianArm -> RandomState Reward #-}
{-# INLINABLE gaussian #-}
gaussian :: (MonadRandom m) => GaussianArm -> m Reward
gaussian (GaussianArm mu sigma) =
    sampleRVar $ normal mu sigma

