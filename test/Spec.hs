module Main (main) where

import Prolens (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
