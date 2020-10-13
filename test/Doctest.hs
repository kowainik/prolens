module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
    $ "-XLambdaCase"
    : "-XInstanceSigs"
    : "-XScopedTypeVariables"
    : "-XTupleSections"
    : "-XTypeApplications"
    :
    [ "src/Prolens.hs"
    ]
