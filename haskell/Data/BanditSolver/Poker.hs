{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.Poker where
import Data.List (foldl')
import Data.Number.Erf (normcdf)
import Data.Vector ((!))
import Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V
import Data.BanditSolver.BanditSolver
import System.Random.Mersenne.Pure64
import Data.RVar (sampleRVar)
import Data.Random (shuffle)
import Control.Monad.State
import Control.Monad.Writer


data Poker = Poker Int -- Horizon h
                (V.Vector Int) -- Number of pulls per arm - n
                (V.Vector Double) -- Reward per arm       - r
                (V.Vector Double) -- Squared rewards per arm - r2
                Double            -- δ
                Double            -- µ* 

{-# INLINE square #-}
square :: (Num a) => a -> a
square a = a^(2::Int)

makePoker :: (Environment e) =>  Int -> Int -> e -> State PureMT Poker
makePoker rounds numArms environment = do
  is@[i0, i1] <- take 2 `liftM` (sampleRVar $ shuffle [0..(numArms - 1)])
  [x0, x1]    <- mapM (getReward environment) is
  let
    r  = modifyVector i1 x1 . modifyVector i0 x0 $ V.replicate numArms 0
    r2 = modifyVector i1 (square x1) . modifyVector i0 (square x0) $ V.replicate numArms 0
    n  = modifyVector i1 1 . modifyVector i0 1 $ V.replicate numArms 0
    !δ     = x0 - x1
    μstar = mean r n i0  -- XXX ?
  return $ Poker (rounds - 1) n r r2 δ μstar
    

{-# INLINE pickedMoreThan #-}
pickedMoreThan k = V.findIndices ( > k)

estimateMu r n = if elemCount == 0 then 0 else meanSum / fromIntegral elemCount
  where 
    elemCount = V.length which
    which      = pickedMoreThan 0 n
    meanSum   = V.foldl' (\acc i -> acc + mean r n i) 0 which

estimateSigma r r2 n = if elemCount == 0 then 0 else devSum / fromIntegral elemCount
  where
    elemCount = V.length which
    which     = pickedMoreThan 1 n
    devSum    = V.foldl' (\acc i -> acc + dev r r2 n i) 0 which

mean r n i = (r ! i) / fromIntegral (n ! i)

dev  r r2 n i =
    let ns = fromIntegral (n ! i)
    in  sqrt $ (r2 ! i) / ns - (square (r ! i)) / (square ns)

instance Solver Poker where
    select (Poker h n r r2 δ µstar) = do
        let numArms = V.length n
            em     = estimateMu r n
            es     = estimateSigma r r2 n
            go (-1) _ imax = imax
            go i pmax imax =
                let µ = if n ! i > 0 then mean r n i else em
                    σ = if n ! i > 1 then dev r r2 n i  else es
                    p = µ + δ * fromIntegral h * (1 - 
                             normcdf ((µstar + δ - µ) * (sqrt $ fromIntegral (n ! i)) / σ))
                in if p > pmax 
                   then go (i - 1) p i
                   else go (i - 1) pmax imax
            start = numArms - 1
        return $ go start (-(1/0)) undefined

    update (Poker h n r r2 _ _) index reward =
      let
        q      = V.length (pickedMoreThan 0 n) - 1
        qsqrt  = sqrt (fromIntegral q)
        i0     = V.maxIndex $ V.imap (\i _ -> mean r n i) n
        i1     = floor qsqrt -- XXX ? 
        δ'     = let res = (mean r n i0 - mean r n i1) / qsqrt
                 in if isNaN res || isInfinite res then error ("out of cheese error: " ++ show res) else res
        µstar' = mean r n i0  -- XXX ?
        reward'= reward + r ! index
        reward2= square reward + r2 ! index
        num    = n ! index + 1
        !r'    = modifyVector index reward' r
        !r2'   = modifyVector index reward2 r2 
        !n'    = modifyVector index num n 
      in Poker (h - 1) n' r' r2' δ' µstar'

    getCumulativeReward (Poker _ _ r _ _ _) = V.sum r

modifyVector index elem = V.modify (\v -> write v index elem)

runAveragedInstantRewards :: GA -> GA -> Int
    -> Int -> Int -> PureMT -> [String]
runAveragedInstantRewards bestArm badArm numArms rounds repetitions gen =
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        (agents, gen') = runState (replicateM repetitions (makePoker rounds numArms realArms)) gen 
    in  evalState (execWriterT $ runInstantRewards realArms agents rounds) gen'

