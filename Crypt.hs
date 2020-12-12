module Crypt where

import Data.List ()
import Utils (bitStrToInt, chunksOf, octStrToBitStr, xorBits)

-- Encryption function E
encerypt :: String -> String -> String
encerypt k m = executeRounds (k `xorBits` m) (keyScheduler k)
  where
    executeRounds ct (k : ks) = executeRounds (roundFunction ct k) ks
    executeRounds ct [] = ct

-- Decryption function D
decrypt :: String -> String -> String
decrypt = undefined

-- Phi
keyOctalPermutation :: String -> String
keyOctalPermutation "000" = "011" -- Phi(0) = 3
keyOctalPermutation "001" = "101" -- Phi(1) = 5
keyOctalPermutation "010" = "111" -- Phi(2) = 7
keyOctalPermutation "011" = "001" -- Phi(3) = 1
keyOctalPermutation "100" = "010" -- Phi(4) = 2
keyOctalPermutation "101" = "000" -- Phi(5) = 0
keyOctalPermutation "110" = "110" -- Phi(6) = 6
keyOctalPermutation "111" = "100" -- Phi(7) = 4

-- Key Scheduler H takes k0 and returns r-1 round keys [k1,...,k10]
keyScheduler :: String -> [String]
keyScheduler k0 = keyScheduler' (nextRoundKey k0) (10 -1)
  where
    keyScheduler' k 0 = [k]
    keyScheduler' k r = k : keyScheduler' (nextRoundKey k) (r -1)

-- Function for obtaining ki+1 from ki
nextRoundKey :: String -> String
nextRoundKey ki = concat $ reverse $ map keyOctalPermutation $ splitToOctals ki

-- Takes a string and splits it in segments of 3 (octals when given a bit string)
splitToOctals :: String -> [String]
splitToOctals = chunksOf 3

-- Roundfunction R
roundFunction ct k = k `xorBits` linearMap (applyPermutationMap ct)

s = chunksOf 2 "71057361762763417545466606045174643201774010235547241170253013435007524436142054031256340237162660356522175362425700156721337231"

-- inverted s [57, 18, 44, 40, 13, 1, 12, 33, 21, 26, 41, 30, 37, 58, 46, 52, 38, 60, 51, 22, 25, 28, 47, 5, 29, 63, 17, 61, 43, 49, 36, 45, 20, 7, 55, 31, 35, 9, 10, 24, 32, 14, 34, 53, 39, 23, 42, 56, 48, 3, 54, 6, 16, 50, 11, 59, 27, 0, 62, 2, 15, 8, 4, 19]

-- Substring permutation function S
permutationMap :: String -> String
permutationMap x = octStrToBitStr $ s !! bitStrToInt x

-- Application function for S on whole cyphertext
applyPermutationMap :: String -> String
applyPermutationMap x = concatMap permutationMap (chunksOf 6 x)

-- Linear map function P
linearMap [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17] = [b16, b14, b8, b2, b4, b11, b17, b5, b10, b3, b12, b15, b6, b9, b1, b7, b0, b13]