module Main where

import Server
import Environment(users, makeEnv)

main :: IO ()
main = startApp =<< users makeEnv
