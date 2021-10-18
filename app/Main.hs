module Main where

import Lib
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
    setLocaleEncoding utf8
    requestBot 0

