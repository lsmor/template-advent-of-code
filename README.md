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

You can use `new-day-gen.sh` to set up a new day for advent of code with `./new-day-gen.sh n` where `n` is the day number. It creates folders `inputs` ands `solutions` and new files under those and modifies the `advent-of-code.cabal` to create a new executable called `day-n`. To run one particular day you use `cabal run day-x -- args`
