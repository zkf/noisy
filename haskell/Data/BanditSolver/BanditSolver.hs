{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.BanditSolver 
( runOne
, runAvg
, runInstantRewards
, runEnsembleIO
, oneRound
, makeCheckpoints
, ActionId
, Reward
, Environment (..)
, Solver (..)
, RandomStateIO
, RandomState
) where 

import Control.Concurrent.Chan.Strict
import Control.Exception (evaluate)
import Control.Monad.State
import Control.Monad.Writer
import Control.Parallel (pseq)
import Data.List
import Data.Random (MonadRandom)
import System.Random.Mersenne.Pure64
import qualified Data.Vector as V
import qualified Statistics.Sample as S

type ActionId = Int
type Reward   = Double
type RandomStateIO = StateT PureMT IO
type RandomState = State PureMT

class Environment e where
    getReward :: (MonadRandom m) => e -> ActionId -> m Reward
    
class Solver s where
    select :: (MonadRandom m) => s -> m ActionId
    update :: s -> ActionId -> Reward -> s
    continue :: s -> Bool -- condition on which to stop early
    getCumulativeReward :: s -> Double
    continue _ = True

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

force :: [a] -> () 
force (x:xs) = x `pseq` force xs
force []     = ()


makeCheckpoints :: Int -> [Int]
makeCheckpoints rounds =
    nub
    . sort
    $ rounds : [c | y <- [1..99]
          , x <- [0 :: Int .. floor ( logBase 10 (fromIntegral rounds) - 1 :: Double)]
          , let c = y * 10^x
          , c < rounds]

{-# INLINE oneRound #-}
oneRound :: (MonadRandom m, Environment e, Solver s) => e -> s -> m (s, Reward)
oneRound env solver = do
    selected <- select solver
    reward   <- getReward env selected
    let !solver' = update solver selected reward
    return (solver', reward)

mean :: Fractional a => [a] -> a
mean = go 0 0
  where go len num [] = num / len
        go len num (x:xs) = go (len + 1) (num + x) xs

