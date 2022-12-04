#!/usr/bin/env bash

d=day-${1}

module_header="module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath
  if read @Int part == 1
    then do
      print \"solution to problem 1 is:\"
      print \"not implemented\"
    else do
      print \"solution to problem 2 is:\"
      print \"not implemented\"
"


cabal_day="executable ${d}
  main-is: ${d}.hs
  hs-source-dirs:
      solutions
  import: deps

"

mkdir -p inputs
mkdir -p solutions

echo "$module_header" > ./solutions/${d}.hs
touch ./inputs/${d}.example
touch ./inputs/${d}.input
echo "$cabal_day" >> advent-of-code.cabal