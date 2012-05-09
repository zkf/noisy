{-# LANGUAGE BangPatterns #-}
module Poker where
import Data.BanditSolver.LTS
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (write)
import Data.Vector ((!))
import Data.Number.Erf (normcdf)
import Data.List (foldl')



data Poker = Poker Int -- Horizon h
                (V.Vector Int) -- Number of pulls per arm - n
                (V.Vector Double) -- Reward per arm       - r
                (V.Vector Double) -- Squared rewards per arm - r2
                Double            -- δ
                Double            -- μ* 

pickedMoreThan k n = filter (\i -> n ! i > k) [0..V.length n - 1]

estimateMu r n = meanSum / fromIntegral (length which) 
  where 
    which   = pickedMoreThan 0 n
    meanSum = foldl' (\acc i -> acc + mean r n i) 0 which

estimateSigma r r2 n = devSum / fromIntegral (length which) 
  where
    which  = pickedMoreThan 1 n
    devSum =  foldl' (\acc i -> acc + dev r r2 n i) 0 which

mean r n i = (r ! i) / fromIntegral (n ! i)

dev  r r2 n i =
    let ns = fromIntegral (n ! i)
    in  sqrt $ (r2 ! i) / ns - ((r ! i)^2) / (ns^2)

instance Solver Poker where
    select (Poker h n r r2 δ μ') = do
        let numArms = V.length n
            em     = estimateMu r n
            es     = estimateSigma r r2 n
            go (-1) _ imax = imax
            go i pmax imax =
                let μ = if n ! i > 0 then mean r n i else em
                    σ = if n ! i > 1 then dev r r2 n i  else es
                    p = μ + δ * fromIntegral h * (1 - 
                             normcdf (((μ' + δ) - μ) * (sqrt $ fromIntegral (n ! i)) / σ))
                in if p > pmax 
                   then go (i - 1) p i
                   else go (i - 1) pmax imax
            start = numArms - 1
        return $ go start (-(1/0)) undefined

    update (Poker h n r r2 _ _) index reward =
      let
        q      = length (pickedMoreThan 0 n) - 1
        qsqrt  = sqrt (fromIntegral q)
        i0     = V.maxIndex r
        i1     = floor qsqrt -- XXX ? 
        δ'     = (mean r n i0 - mean r n i1) / qsqrt
        μstar' = mean r n i0  -- XXX ?
        reward'= reward + r ! index
        reward2= reward^(2::Int) + r2 ! index
        num    = n ! index + 1
        !r'    =  V.modify (\v -> write v index reward') r
        !r2'   =  V.modify (\v -> write v index reward2) r2
        !n'    =  V.modify (\v -> write v index num) n
      in Poker (h - 1) n' r' r2' δ' μstar'

    getCumulativeReward (Poker _ _ r _ _ _) = V.sum r
