module Crypt where

import Data.List ()
import Utils (bitStrToInt, chunksOf, octStrToBitStr, xorBits)

-- Encryption function E
encrypt :: String -> String -> String
encrypt k m = executeRounds (k `xorBits` m) (drop 1 $ keyScheduler k)
  where
    executeRounds ct (k : ks) = executeRounds (roundFunction ct k) ks
    executeRounds ct [] = ct

-- Decryption function D
decrypt :: String -> String -> String
decrypt k c = k `xorBits` executeRounds c (reverse $ drop 1 $ keyScheduler k)
  where
    executeRounds ct (k : ks) = executeRounds (inverseRoundFunction ct k) ks
    executeRounds ct [] = ct

-- Key octal permutation function Phi
keyOctalPermutation :: String -> String
keyOctalPermutation "000" = "011" -- Phi(0) = 3
keyOctalPermutation "001" = "101" -- Phi(1) = 5
keyOctalPermutation "010" = "111" -- Phi(2) = 7
keyOctalPermutation "011" = "001" -- Phi(3) = 1
keyOctalPermutation "100" = "010" -- Phi(4) = 2
keyOctalPermutation "101" = "000" -- Phi(5) = 0
keyOctalPermutation "110" = "110" -- Phi(6) = 6
keyOctalPermutation "111" = "100" -- Phi(7) = 4

-- Key Scheduler H takes k0 and returns r+1 round keys [k0, k1,...,k10]
keyScheduler :: String -> [String]
keyScheduler k0 = keyScheduler' k0 10
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
roundFunction :: String -> String -> String
roundFunction ct k = k `xorBits` linearMap (applyPermutationMap ct)

-- Inverse of round function R^-1
inverseRoundFunction :: String -> String -> String
inverseRoundFunction ct k = applyInversePermutationMap $ inverseLinearMap (k `xorBits` ct)

-- S written in compressed octal notation
s :: [String]
s = chunksOf 2 "71057361762763417545466606045174643201774010235547241170253013435007524436142054031256340237162660356522175362425700156721337231"

-- Invesrse of S written in compressed octal notation
inverteseS :: [String]
inverteseS = chunksOf 2 "71225450150114412532513645725664467463263134570535772175536144552407673743111230401642654727527060036606206213733300760217100423"

-- Substring permutation function S
permutationMap :: String -> String
permutationMap x = octStrToBitStr $ s !! bitStrToInt x

-- Inverse substring permutation function S^-1
inversePermutationMap :: String -> String
inversePermutationMap x = octStrToBitStr $ inverteseS !! bitStrToInt x

-- Application function for S on whole cyphertext
applyPermutationMap :: String -> String
applyPermutationMap x = concatMap permutationMap (chunksOf 6 x)

-- Application function for S^-1 on whole cyphertext
applyInversePermutationMap :: String -> String
applyInversePermutationMap x = concatMap inversePermutationMap (chunksOf 6 x)

-- Linear map function P
linearMap :: [a] -> [a]
linearMap [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17] = [b16, b14, b8, b2, b4, b11, b17, b5, b10, b3, b12, b15, b6, b9, b1, b7, b0, b13]

-- Inverse linear map function P^-1
inverseLinearMap :: [a] -> [a]
inverseLinearMap [b16, b14, b8, b2, b4, b11, b17, b5, b10, b3, b12, b15, b6, b9, b1, b7, b0, b13] = [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17]
