{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Data.BanditSolver.LTS 

(   GaussianArms, GaussianArm(..), LTS(..),
    runAveragedLTS, runAveragedInstantRewards, runAverageCumulativeReward,
    runArms,
    updateLTS, makeLTS,
    makeGaussianArm
) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan.Strict
import Control.Monad.State
import Control.Monad.Writer
import Data.Random (MonadRandom)
import Data.Vector ((!))
import Data.Vector.Generic.Mutable (write)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT)
import qualified Data.Vector as V
import Text.Printf
import Data.BanditSolver.BanditSolver

data LTS = LTS !GaussianArms !Double !Double deriving (Show)-- arms, cumulative reward, obnoise

makeLTS :: GaussianArm -> Int -> Double -> LTS
makeLTS armEstimate numArms ob = LTS startEstimates 0 ob
  where startEstimates = V.fromList $ replicate numArms armEstimate

instance Solver LTS where
    {-# INLINE select #-}
    select = selectArm
    {-# INLINE update #-}
    update = updateLTS
    getCumulativeReward (LTS _ r _) = r



runAveragedLTS :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> WriterT [String] IO () -- Checkpoint, ob, mean of cumulative reward
runAveragedLTS bestArm badArm armEstimate numArms rounds repetitions ob = do
    threads <- lift getNumCapabilities
    let chunks = evenlyDistribute repetitions threads
    chans <- lift $ replicateM threads newChan
    _ <- lift $ zipWithM (\chan reps -> forkIO $ runAveragedLTSIO bestArm badArm armEstimate numArms rounds reps ob chan) chans chunks
    forM_ (makeCheckpoints rounds) $ \n -> do
        results <- liftIO $ mapM readChan chans
        let m = sum results / fromIntegral repetitions
        tell [ unwords [show n, showDouble ob, showDouble m]]
    return ()

evenlyDistribute :: Int -> Int -> [Int]
evenlyDistribute tasks threads = 
    let (chunk, rest) = tasks `divMod` threads
    in  zipWith (+) (replicate threads chunk) (replicate rest 1 ++ repeat 0)

showDouble :: Double -> String
showDouble = printf "%f" 

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

runAveragedInstantRewards :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> IO [String] -- Checkpoint, mean of instant rewards
runAveragedInstantRewards bestArm badArm armEstimate numArms rounds repetitions ob = do
    threads <- getNumCapabilities
    let chunks = evenlyDistribute repetitions threads
    chans <- replicateM threads newChan
    zipWithM_ (\chan reps -> forkIO $ runAveragedInstantRewardsIO bestArm badArm armEstimate numArms rounds reps ob chan) chans chunks
    forM (makeCheckpoints rounds) $ \n -> do
        results <- mapM readChan chans
        let m = sum results / fromIntegral repetitions
        return $ unwords [show n, showDouble m]


runAveragedInstantRewardsIO :: GA -> GA -> GA -> Int
    -> Int -> Int -> Double -> Chan Double -> IO ()
runAveragedInstantRewardsIO bestArm badArm armEstimate numArms rounds repetitions ob chan = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agents = replicate repetitions (makeLTS myArmEstimate numArms ob)
    gen <- newPureMT
    evalStateT (runInstantRewards realArms agents rounds chan) gen

{-# SPECIALIZE runAverageCumulativeReward :: 
    GA -> GA -> GA -> Int -> Int -> Int -> Double -> RandomStateIO String #-}
runAverageCumulativeReward :: MonadRandom m =>
    GA -> GA -> GA -> Int -> Int -> Int -> Double -> m String
runAverageCumulativeReward  bestArm badArm armEstimate numArms rounds repetitions ob = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agent = makeLTS myArmEstimate numArms ob
    res <- runAvg realArms repetitions rounds agent
    return $ show numArms ++ " " ++ show res


runArms :: GA -> GA -> GA -> Int -> Int -> Double -> PureMT -> [String]
runArms bestArm badArm armEstimate numArms rounds ob gen = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agent = makeLTS myArmEstimate numArms ob
    return $ evalState (execWriterT $ logArms realArms agent rounds) gen

logArms :: (Environment e) => e -> LTS -> Int 
    -> WriterT String (State PureMT) ()
logArms environment startAgent rounds = run 1 startAgent
  where run n agent
            | n > rounds  = return ()
            | otherwise   = do
                agent'@(LTS arms _ _) <- lift $ fst `liftM` oneRound environment agent
                let as = map (\(GaussianArm m s) -> unwords [show n, show m, show s]) (V.toList arms)
                tell $ unlines as ++ "\n"
                run (n + 1) agent'


{-# SPECIALIZE INLINE selectArm :: LTS -> RandomStateIO ActionId #-}
{-# SPECIALIZE INLINE selectArm :: LTS -> RandomState ActionId #-}
{-# INLINE selectArm #-}
selectArm :: (MonadRandom m) => LTS -> m ActionId
selectArm (LTS arms _ _) = do
   let start = V.length arms - 1
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
updateLTS :: LTS -> ActionId -> Reward -> LTS
updateLTS (LTS arms creward ob) !index !reward = 
    let (GaussianArm mu sigma) = arms ! index
        armVariance = sigma * sigma
        obVariance  = ob * ob
        !mu'      = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        !sigma'   = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        !arms'    =  V.modify (\v -> write v index (GaussianArm mu' sigma')) arms
        !creward' = creward + reward
    in LTS arms' creward' ob

