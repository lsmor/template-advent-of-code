#!/usr/bin/env bash

d=day-${1}

module_header="module Main where

main :: IO ()
main = do
  putStrLn \"Solution to part 1 is:\"
  print $ \"part one not implemented\"
  putStrLn \"Solution to part 2 is:\"
  print $ \"part two not implemented\"
"

cabal_day="executable ${d}
  main-is: ${d}.hs
  import: deps

"

echo "$module_header" > ./solutions/${d}.hs
touch ./inputs/${d}.example
touch ./inputs/${d}.input
echo "$cabal_day" >> advent-of-code.cabal