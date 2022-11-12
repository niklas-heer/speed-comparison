{- | Approach using infinite lists. -}
module Main where

-- | Computes partial alternating series converging
--   to π/4=arctan(1) up n terms.
leibniz :: Int -> Double
leibniz n = sum . take n $ zipWith (*) (cycle [1, -1]) sequence
  where sequence = [inv (2*i - 1) | i <- [1..]]
        inv = recip . fromIntegral

main :: IO ()
main = rounds >>= putStrLn . show . (4 * ) . leibniz
  where rounds :: IO Int
        rounds = read <$> readFile "rounds.txt"
