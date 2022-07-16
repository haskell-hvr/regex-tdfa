module Main where

import System.Environment
         ( getArgs )
import Test.DocTest
         ( mainFromLibrary )
import Test.DocTest.Helpers
         ( extractSpecificCabalLibrary, findCabalPackage )

main :: IO ()
main = do
  args <- getArgs
  pkg  <- findCabalPackage "regex-tdfa"
  -- Need to give the library name, otherwise the parser does not find it.
  lib  <- extractSpecificCabalLibrary Nothing pkg
  mainFromLibrary lib args
