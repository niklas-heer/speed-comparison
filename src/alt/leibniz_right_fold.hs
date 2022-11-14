{- | A more idiomatic, but slower implementation. -}

module Main where

-- | Computes partial alternating series converging
--   to π/4=arctan(1) up n terms.
leibniz :: Int -> Double
leibniz n = partial 1 where
  partial k | k > n = 0
            | otherwise = inv (2*k - 1) - partial (k + 1)
  inv = recip . fromIntegral

-- Notice:
--   1 - (1 / 3 - (1/5 - (1/7 - (1/9 - ...))))
-- = 1 - 1/3 + (1/5 - (1/7 - (1/9 - ...)))
-- = 1 - 1/3 + 1/5 - 1/7 + (1/9 - ...)
-- = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...

main :: IO ()
main = rounds >>= putStrLn . show . (4 * ) . leibniz
  where rounds :: IO Int
        rounds = read <$> readFile "rounds.txt"
