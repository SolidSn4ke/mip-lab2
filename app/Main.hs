module Main (main) where

import MonteCarlo (importanceMonteCarlo, multiImportanceMonteCarlo, russianRoulette, simpleMonteCarlo, stratifiedMonteCarlo)
import Text.Printf (printf)

main :: IO ()
main = do
    let ns = [100 :: Int, 1000, 10000, 100000]
    let f x = x ** 2
    let interval = (2, 5)
    let trueVal = 39 :: Double
    let absErr = abs . (-) 39
    let estErr x = trueVal / sqrt (fromIntegral x)

    putStrLn "Simple Monte Carlo"
    putStrLn "Размрер выборки | Расчитанное значение интеграла | Абсолютная ошибка | Оценка погрешности"
    mapM_
        ( \n -> do
            int <- simpleMonteCarlo f interval n
            putStrLn $ printf "%15d | %30.3f | %17.3f | %18.3f" n int (absErr int) (estErr n)
        )
        ns

    putStrLn ""
    putStrLn "Stratified Monte Carlo"
    putStrLn "Размрер выборки |  Шаг  | Расчитанное значение интеграла | Абсолютная ошибка | Оценка погрешности"
    mapM_
        ( \n -> do
            mapM_
                ( \step -> do
                    int <- stratifiedMonteCarlo f interval step n
                    putStrLn $ printf "%15d | %5.2f | %30.3f | %17.3f | %18.3f" n step int (absErr int) (estErr n)
                )
                [0.5 :: Double, 1]
        )
        ns

    putStrLn ""
    putStrLn "Importance Monte Carlo"
    putStrLn "Размрер выборки | Функция плотности | Расчитанное значение интеграла | Абсолютная ошибка | Оценка погрешности"
    mapM_
        ( \n -> do
            mapM_
                ( \p -> do
                    int <- importanceMonteCarlo f interval p n
                    let funcToStr
                            | p 2 == 2 = " x "
                            | p 2 == 4 = "x^2"
                            | p 2 == 8 = "x^3"
                            | otherwise = error ""
                    putStrLn $ printf "%15d | %17s | %30.3f | %17.3f | %18.3f" n funcToStr int (absErr int) (estErr n)
                )
                [id, \x -> (x :: Double) ** 2, \x -> (x :: Double) ** 3]
        )
        ns

    putStrLn ""
    putStrLn "Multi Importance Monte Carlo"
    putStrLn "Выборка | Плотность 1 | Плотность 2 | Интеграл (ср. плотность) | Интеграл (ср. квадрат плотности) | Абс. ошибка (ср. плотность) | Абс. ошибка (ср. квадрат плотности) | Оценка погрешности"
    mapM_
        ( \n -> do
            mapM_
                ( \(p1, p2) -> do
                    (int1, int2) <- multiImportanceMonteCarlo f interval p1 p2 n
                    let funcToStr p
                            | p 2 == 2 = " x "
                            | p 2 == 4 = "x^2"
                            | p 2 == 8 = "x^3"
                            | otherwise = error ""
                    putStrLn $ printf "%7d | %11s | %11s | %24.3f | %32.3f | %27.3f | %35.3f | %18.3f" n (funcToStr p1) (funcToStr p2) int1 int2 (absErr int1) (absErr int2) (estErr n)
                )
                [(id, id), (id, \x -> (x :: Double) ** 2), (id, \x -> (x :: Double) ** 3), (\x -> (x :: Double) ** 2, \x -> (x :: Double) ** 2), (\x -> (x :: Double) ** 2, \x -> (x :: Double) ** 3), (\x -> (x :: Double) ** 3, \x -> (x :: Double) ** 3)]
        )
        ns

    putStrLn ""
    putStrLn "Russian Roulette Monte Carlo"
    putStrLn "Размрер выборки | Отсечка | Расчитанное значение интеграла | Абсолютная ошибка | Оценка погрешности"
    mapM_
        ( \n -> do
            mapM_
                ( \a -> do
                    int <- russianRoulette f interval a n
                    putStrLn $ printf "%15d | %7.2f | %30.3f | %17.3f | %18.3f" n a int (absErr int) (estErr n)
                )
                [0.5, 0.75, 0.95]
        )
        ns