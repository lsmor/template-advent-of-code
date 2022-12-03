# advent-of-code

The structure of this repo is:

```txt
./inputs
  |- day-1.input
  |- day-2.input
  |- ...
./solutions
  |- day-1.hs
  |- day-2.hs
  |- ...
other-files
```

You can use new-day-gen.sh to set up a new day for advent of code. It creates new file under inputs and solutions and modifies `.cabal` to create a new executable. In other to run one particular day you use `cabal run day-x -- args`
