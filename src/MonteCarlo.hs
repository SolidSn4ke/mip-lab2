module MonteCarlo (simpleMonteCarlo, stratifiedMonteCarlo, importanceMonteCarlo) where

import Control.Monad (replicateM)
import System.Random (randomRIO)

simpleMonteCarlo :: (Double -> Double) -> (Double, Double) -> Int -> IO Double
simpleMonteCarlo f (a, b) n = do
    values <- replicateM n (randomRIO (a, b))
    return $ (*) ((b - a) / fromIntegral n) $ sum $ map f values

stratifiedMonteCarlo :: (Double -> Double) -> (Double, Double) -> Double -> Int -> IO Double
stratifiedMonteCarlo f (a, b) step n = sum <$> sequence [simpleMonteCarlo f (a', a' + step) n | a' <- [a, a + step .. b - step]]

importanceMonteCarlo :: (Double -> Double) -> (Double, Double) -> (Double -> Double) -> Int -> IO Double
importanceMonteCarlo f (a, b) p n = do
    int <- simpleMonteCarlo p (a, b) n
    xs <- replicateM n (randomRIO (a, b))
    return $ (*) (1 / fromIntegral n) $ sum [f x / (p x / int) | x <- xs]