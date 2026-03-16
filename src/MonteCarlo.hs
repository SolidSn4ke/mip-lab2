module MonteCarlo (simpleMonteCarlo, stratifiedMonteCarlo, importanceMonteCarlo, multiImportanceMonteCarlo) where

import Control.Monad (replicateM)
import System.Random (randomRIO)

simpleMonteCarlo :: (Double -> Double) -> (Double, Double) -> Int -> IO Double
simpleMonteCarlo f (a, b) n = do
    values <- replicateM n $ randomRIO (a, b)
    return $ (*) ((b - a) / fromIntegral n) $ sum $ map f values

stratifiedMonteCarlo :: (Double -> Double) -> (Double, Double) -> Double -> Int -> IO Double
stratifiedMonteCarlo f (a, b) step n = sum <$> sequence [simpleMonteCarlo f (a', a' + step) n | a' <- [a, a + step .. b - step]]

importanceMonteCarlo :: (Double -> Double) -> (Double, Double) -> (Double -> Double) -> Int -> IO Double
importanceMonteCarlo f (a, b) p n = do
    int <- simpleMonteCarlo p (a, b) n
    xs <- replicateM n $ randomRIO (a, b)
    return $ (*) (1 / fromIntegral n) $ sum [f x / (p x / int) | x <- xs]

multiImportanceMonteCarlo :: (Double -> Double) -> (Double, Double) -> (Double -> Double) -> (Double -> Double) -> Int -> IO (Double, Double)
multiImportanceMonteCarlo f (a, b) p1 p2 n = do
    int1 <- simpleMonteCarlo p1 (a, b) n
    int2 <- simpleMonteCarlo p2 (a, b) n
    let p1' x = p1 x / int1
    let p2' x = p2 x / int2
    xs <- replicateM n $ randomRIO (a, b)
    return $ (\(sum1, sum2) -> (sum1 / fromIntegral n, sum2 / fromIntegral n)) $ foldl (\(old1, old2) x -> (old1 + f x / (p1' x + p2' x), old2)) (0, 0) xs
