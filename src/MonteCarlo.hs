module MonteCarlo (simpleMonteCarlo, stratifiedMonteCarlo, importanceMonteCarlo) where

import Control.Monad (replicateM)
import System.Random (randomRIO)

simpleMonteCarlo :: (Double -> Double) -> (Double, Double) -> IO Double
simpleMonteCarlo f (a, b) = do
    values <- replicateM 100000 (randomRIO (a, b))
    return $ (*) ((b - a) / 100000) $ sum $ map f values

stratifiedMonteCarlo :: (Double -> Double) -> (Double, Double) -> Double -> IO Double
stratifiedMonteCarlo f (a, b) step = sum <$> sequence [simpleMonteCarlo f (a', a' + step) | a' <- [a, a + step .. b - step]]

importanceMonteCarlo :: (Double -> Double) -> (Double, Double) -> (Double -> Double) -> IO Double
importanceMonteCarlo f (a, b) p = do
    int <- simpleMonteCarlo p (a, b)
    xs <- replicateM 100000 (randomRIO (a, b))
    return $ (*) (1 / 100000) $ sum [f x / (p x / int) | x <- xs]