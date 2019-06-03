module Main where

import Server
import Environment(makeEnv)

main :: IO ()
main = startApp makeEnv
