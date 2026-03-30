module MonteCarlo (simpleMonteCarlo, importanceMonteCarlo, multiImportanceMonteCarlo, russianRoulette, stratifiedMonteCarlo) where

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
    int <- if p 2 == 4 then return 39 else simpleMonteCarlo p (a, b) n
    taus <- replicateM n $ randomRIO (0 :: Double, 1)
    let xi tau
            | p 2 == 2 = sqrt $ tau * (b ** 2 - a ** 2) + a ** 2
            | p 2 == 4 = flip (**) (1 / 3) $ tau * (b ** 3 - a ** 3) + a ** 3
            | otherwise = flip (**) (1 / 4) $ tau * (b ** 4 - a ** 4) + a ** 4
    return $ flip (/) (fromIntegral n) $ foldl (\acc t -> acc + f (xi t) / p (xi t) * int) 0 taus

multiImportanceMonteCarlo :: (Double -> Double) -> (Double, Double) -> (Double -> Double) -> (Double -> Double) -> Int -> IO (Double, Double)
multiImportanceMonteCarlo f (a, b) p1 p2 n = do
    int1 <- if p1 2 == 4 then return 39 else simpleMonteCarlo p1 (a, b) n
    int2 <- if p2 2 == 4 then return 39 else simpleMonteCarlo p2 (a, b) n
    let xi p tau
            | p 2 == 2 = sqrt $ tau * (b ** 2 - a ** 2) + a ** 2
            | p 2 == 4 = flip (**) (1 / 3) $ tau * (b ** 3 - a ** 3) + a ** 3
            | otherwise = flip (**) (1 / 4) $ tau * (b ** 4 - a ** 4) + a ** 4
    let p1' x = p1 x / int1
    let p2' x = p2 x / int2
    let wlin p x = p x / (p1' x + p2' x)
    let wsqr p x = p x ** 2 / (p1' x ** 2 + p2' x ** 2)
    taus <- replicateM n $ randomRIO (0 :: Double, 1)
    return $
        (\(a1, a2) -> (a1 / fromIntegral n, a2 / fromIntegral n)) $
            foldl
                ( \(acc1, acc2) t ->
                    ( acc1 + wlin p1' t * f (xi p1 t) / p1' (xi p1 t) + wlin p2' t * f (xi p2 t) / p2' (xi p2 t)
                    , acc2 + wsqr p1' t * f (xi p1 t) / p1' (xi p1 t) + wsqr p2' t * f (xi p2 t) / p2' (xi p2 t)
                    )
                )
                (0, 0)
                taus

russianRoulette :: (Double -> Double) -> (Double, Double) -> Double -> Int -> IO Double
russianRoulette f (a, b) prob n = do
    values <- replicateM n $ randomRIO (a, b)
    mapM
        ( \x -> do
            rand <- randomRIO (0 :: Double, 1)
            if rand < prob
                then
                    return $ f x / prob
                else return 0
        )
        values
        >>= (\x -> return $ (b - a) * sum x / fromIntegral n)
