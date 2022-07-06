module Main where

import Lib

main :: IO ()
main = loadConfiguration >>= print
