{- | A more direct port of the C version. -}
module Main where

-- | Computes partial alternating series converging
--   to π/4=arctan(1) up n terms.
leibniz :: Int -> Double
leibniz n = partial 2 1.0 where
  partial :: Int -> Double -> Double
  partial i qpi
    | i == n + 2 = qpi
    | otherwise  = partial (i + 1) $ qpi - powNeg1 i * inv (2*i - 1)
  inv = recip . fromIntegral
  powNeg1 i = 1 - 2 * fromIntegral (i `mod` 2)

main :: IO ()
main = rounds >>= putStrLn . show . (4 * ) . leibniz
  where rounds :: IO Int
        rounds = read <$> readFile "rounds.txt"
