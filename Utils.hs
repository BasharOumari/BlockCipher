module Utils where

import Data.Char ()

-- Splits list in to chunks of decired length (list length must be multiple of chunk length)
chunksOf :: Int -> [e] -> [[e]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

bitStrToInt :: String -> Int
bitStrToInt x = bitStrToInt' $ reverse x
  where
    bitStrToInt' [] = 0
    bitStrToInt' (x : xs) = (read [x] :: Int) + 2 * bitStrToInt' xs

octStrToBitStr :: String -> String
octStrToBitStr = concatMap octDigitToBin

octDigitToBin :: Char -> String
octDigitToBin n
  | n == '0' = "000"
  | n == '1' = "001"
  | n == '2' = "010"
  | n == '3' = "011"
  | n == '4' = "100"
  | n == '5' = "101"
  | n == '6' = "110"
  | n == '7' = "111"

xorBits :: String -> String -> String
xorBits (a : as) (b : bs)
  | a == b = "0" ++ as `xorBits` bs
  | otherwise = "1" ++ as `xorBits` bs
xorBits [] [] = []