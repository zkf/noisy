{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Data.BanditSolver.LTS (runAveragedLTS, runOne, Environment(..),
    updateLTS, LTS(..), makeLTS, runAvg, Solver(..), GaussianArms, GaussianArm)
where
import Control.Monad.Writer
import Control.Monad.State
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT)
import Data.RVar (sampleRVar)
import qualified Statistics.Sample as S (mean, meanVarianceUnb)
import Data.Vector.Generic.Mutable (write)
import Control.Parallel (pseq)
import Data.Vector ((!))
import qualified Data.Vector as V

type ActionId = Int
type Reward   = Double
type GaussianArm = (Double, Double) -- Mean, standard deviation
type GaussianArms = V.Vector GaussianArm
data LTS = LTS !(GaussianArms, Double, Double) deriving (Show)-- arms, cumulative reward, obnoise

makeLTS :: GaussianArms -> Double -> LTS
makeLTS startEstimates ob = LTS (startEstimates, 0, ob)

class Environment e where
    getReward :: e -> ActionId -> State PureMT Double
    
class Solver s where
    select :: s -> State PureMT ActionId
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
    getReward arms idx = gaussian (arms ! idx)

instance Solver LTS where
    make a r o = LTS (a,r,o)
    getArms (LTS (arms, _, _)) = arms
    getCumulativeReward (LTS (_, r, _)) = r
    getObservationNoise (LTS(_,_,ob)) = ob


runOne :: (Environment e, Solver s) => e -> Int -> s -> State PureMT s
runOne realArms rounds startSolver = run 0 startSolver
    where run n !solver 
            | n == rounds = return solver
            | otherwise   = do
                solver' <- oneRound realArms solver
                if continue solver' 
                    then run (n+1) solver'
                    else return solver'

runAvg :: (Environment e, Solver s) => e -> Int -> Int -> s -> State PureMT Double
runAvg realArms n rounds startSolver = do
    res <- mapM (runOne realArms rounds) (replicate n startSolver)
    return . S.mean . V.fromList $ map getCumulativeReward res

runAveragedLTS :: GaussianArms -> GaussianArms -> Int -> Int -> Double -> PureMT
    -> [(Int, Double, Double, Double)] -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTS realArms startEstimates rounds repetitions ob randomgen = 
    let result = snd $ evalState (runWriterT $ runEnsemble realArms rounds solvers) randomgen
    in force result `seq` result
  where solvers = replicate repetitions (makeLTS startEstimates ob)
        force ((!a,!b,!c,!d):xs) = (a,b,c,d) `pseq` force xs
        force []                 = ()

runEnsemble :: (Environment e, Solver s) =>  e -> Int -> [s] ->
    WriterT [(Int, Double, Double, Double)] (State PureMT) ()
runEnsemble environment rounds startSolvers = run 1 startSolvers
  where run n solvers = do
            solvers' <- lift $ oneRoundEnsemble environment solvers
            when   (n `elem` checkpoints) $ writeLog solvers' n
            unless (n == rounds) $ force solvers' `seq` run (n + 1) solvers'
        checkpoints = rounds : [y * 10^x | y <- [1, 5],
              x <- [1 :: Int .. floor (logBase 10 $
                                         fromIntegral rounds/5.0 :: Double)]]
        force (x:xs) = x `pseq` force xs
        force []     = ()
        writeLog s n =
            let crewards    = map getCumulativeReward s
                (mean, var) = S.meanVarianceUnb $ V.fromList crewards
                ob          = getObservationNoise $ head s
            in tell [(n, ob, mean, sqrt var)]

oneRoundEnsemble :: (Environment e, Solver s) => e -> [s] -> State PureMT [s]
oneRoundEnsemble environment solvers = mapM (oneRound environment) solvers
   

oneRound :: (Environment e, Solver s) => e -> s -> State PureMT s
oneRound env solver = do
    selected <- select solver
    reward   <- getReward env selected
    let !solver' = update solver selected reward
    return solver'

selectArm :: Solver s => s -> State PureMT ActionId
selectArm solver = do
   let arms = getArms solver
       start = V.length arms - 1
       go :: ActionId -> (Double, ActionId) -> State PureMT ActionId
       go 0 (maxValue, index) = do
           newValue <- gaussian (arms ! 0)
           if newValue > maxValue 
               then return 0
               else return index 
       go n (maxValue, index) = do
           newValue <- gaussian (arms ! n)
           if newValue > maxValue 
               then go (n - 1) (newValue, n)
               else go (n - 1) (maxValue, index)
   firstValue <- gaussian (arms ! start)
   go (start - 1) (firstValue, start)

-- Alternative to the `go' function above is this:
--      V.maxIndex `fmap` V.mapM gaussian arms
-- The downside is that it is much slower.


updateLTS :: Solver s => s -> ActionId -> Reward -> s
updateLTS solver !index !reward = 
    let arms    = getArms solver
        creward = getCumulativeReward solver
        ob      = getObservationNoise solver
        (mu, sigma) = arms ! index
        armVariance = sigma*sigma
        obVariance  = ob*ob
        !mu'      = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        !sigma'   = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        !arms'    =  V.modify (\v -> write v index (mu', sigma')) arms
        !creward' = creward + reward
    in make arms' creward' ob

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma
