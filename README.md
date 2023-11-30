# Advent of Code for Haskell

This repository is a GitHub template for Advent of Code solutions in Haskell. It provides a simple CLI tool and VSCode tasks to streamline your Advent of Code experience in Haskell, without the need to learn `cabal` and with a sensible set of packages.

## Features

- A sensible set of dependencies for AoC.
- A bash command-line interface (CLI) that allows you to:
  - Create Haskell files for each new Advent of Code day.
  - Run solutions for each day with ease.
  - Automatically manage Cabal files, reducing setup hassles.
- VSCode tasks for seamless interaction with the CLI.

## Getting Started

To get started with Advent of Code in Haskell using this template:

1. Install your Haskell tooling via [`ghcup`](https://www.haskell.org/ghcup/).

2. Click on "Use this Template" to create a new project/repository based on this template.

3. Clone the newly created repository to your local machine:

   ```bash
   git clone https://github.com/<yourusername>/your-new-project.git
   cd your-new-project
   ```

4. Optionally, create an `.env` file with your Advent of Code session cookie. If you're unsure how to obtain the cookie, check [this guide](https://github.com/wimglenn/advent-of-code-wim/issues/1). If you don't create an `.env` file you'll be prompted to provided the cookie.

    ```env
    AOC_SESSION=your-cookie-here
    ```

5. Create `day-1` files. You can do this in one of two ways:
   - Via the CLI: Run `./aoc-hs new -d 1` (you may need to run `chmod +x aoc-hs` before this).
   - Via VSCode tasks: Press `CTRL+Shift+P`, type "Run Task," and select "create new day template." Follow the instructions.

After following these steps, you'll find newly created folders named `solutions` and `inputs` in your project directory. Files `input/day-x.input` should contain your personal AoC input and `input/day-x.example` should be empty. You can copy paste the public example given by AoC in those `.example` files

Start Coding. Probably you have to wait a little bit for `HLS` to download and build some dependencies the first time you open the project.

## CLI Usage

Here's how to use the CLI for common tasks:

- Create a new solution for a specific day:

  ```bash
  ./aoc-hs new -d <day>
  ```

- Run a solution for a specific day and part:
  
  ```bash
  ./aoc-hs run -d <day> -p <part> --input
  ```

Below there is the complete description of the CLI (you can run `./aoc-hs --help` to get it in you local machine):

```bash
Usage: aoc-hs [new -d <day> [--no-curl] | run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]]

Description:
  This tool simplifies Advent of Code solutions in Haskell by creating templates and handling input files. No need to learn Cabal!

Subcommand: new

Create a new Advent of Code solution for the specified day. It creates a main module, modifies the .cabal file, and downloads the input data.

Usage: aoc-hs new -d <day>
Example: aoc-hs new -d 3
         aoc-hs new -d 3 --no-curl
Options:
  -d <day>       Specify the day for the Advent of Code puzzle (1-25).
  --no-curl      It wont download your personal AoC input file. You don't have you provide a cookie with this option

Subcommand: run

Run an Advent of Code solution for the specified day and part. The input data is read from a file which can be supplied via -f or you can 
use shortcuts --example and --input. Default --input

Usage: aoc-hs run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]
Example: aoc-hs run -d 3 -p 2 --example
         aoc-hs run -d 3 -p 3 -e
         aoc-hs run -d 3 -p 2 --input
         aoc-hs run -d 3 -p 2 -i
         aoc-hs run -d 3 -p 2 -f my-input-file.txt
Options:
  -d <day>       Specify the day for the Advent of Code puzzle (1-25).
  -p <part>      Specify the part of the puzzle (1 or 2).
  -f <file-name> Specify a custom input file to use.
  --example, -e  Use the example input file (./inputs/day-<day>.example) as input.
  --input, -i    (Default) Use the puzzle input file (./inputs/day-<day>.input) as input.
```

## Haskell details

After running `./aoc-hs new -d x`, you'll find files `day-x.hs` in the `solutions` folder. Such files should contain the haskell code for that day. Each day is built as an stand alone executable, so code isn't shared between two days. In practise, this is not a problem since it is rare that AoC days requiere code sharing. Each executable depends on these libraries:

- [attoparsec](https://hackage.haskell.org/package/attoparsec): for parsing inputs
- [base >=4.7 && <5](https://hackage.haskell.org/package/base): the base package
- [bytestring](https://hackage.haskell.org/package/bytestring): bytestring is the string format used by attoparsec
- [containers](https://hackage.haskell.org/package/containers): containers for generic containers like Map, Set, etc...
- [matrix](https://hackage.haskell.org/package/matrix): because AoC love two-dimensional discrete problems
- [vector](https://hackage.haskell.org/package/vector): `Int` based array. Oftenly a replacement for Haskell's Lists
- [split](https://hackage.haskell.org/package/split): algorithms to split lists
- [search-algorithms](https://hackage.haskell.org/package/search-algorithms): a blessed interface to bfs, dfs, dijkstra, etc...
- [mtl](https://hackage.haskell.org/package/mtl): Just in case you need the state monad
- [pointedlist](https://hackage.haskell.org/package/pointedlist): because AoC loves circual arrays.

The dependencies are chosen to match a typical AoC season.

## Contributing

Contributions are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request.

## License

This project is licensed under the WTFPL License - see the [LICENSE](LICENSE) file for details.
