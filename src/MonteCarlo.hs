module MonteCarlo (simpleMonteCarlo) where

import System.Random (Random (randomR), newStdGen)

simpleMonteCarlo :: (Double -> Double) -> (Double, Double) -> IO Double
simpleMonteCarlo f (a, b) = do
    gen <- newStdGen
    return $ (*) ((b - a) / 100000) $ sum $ map (\_ -> f . fst $ randomR (a, b) gen) [1 :: Integer .. 100000]

-- stratifiedMonteCarlo :: (Double -> Double) -> (Double, Double) -> Double -> IO Double
-- stratifiedMonteCarlo f (a, b) step = do
--     gen <- newStdGen
--     mapM_ (\x -> x) [simpleMonteCarlo f (a', a' + step) | a' <- [a, a + step .. b]]