module Main where

import Crypt (decrypt, encrypt)
import Data.List ()
import System.Environment
import Utils ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-e" : k : m : _) ->
      putStrLn $ encrypt k m
    ("-d" : k : c : _) ->
      putStrLn $ decrypt k c
    _ ->
      putStrLn "Usage: -e <18bit key> <18bit message> | -d <18bit key> <18bit cyphertext>"
