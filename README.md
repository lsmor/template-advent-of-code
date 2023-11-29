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

4. Optionally, create an `.env` file with your Advent of Code session cookie. If you're unsure how to obtain the cookie, check [this guide](https://github.com/wimglenn/advent-of-code-wim/issues/1).

    ```env
    AOC_SESSION=your-cookie-here
    ```

5. Create `day-1` files. You can do this in one of two ways:
   - Via the CLI: Run `./aoc-hs.sh new -d 1` (you may need to run `chmod +x aoc-hs.sh` before this).
   - Via VSCode tasks: Press `CTRL+Shift+P`, type "Run Task," and select "create new day template." Follow the instructions.

6. Start Coding. Probably you have to wait a little bit for `HLS` to download and build some dependencies the first time you open the project.

After following these steps, you'll find newly created folders named `solutions` and `inputs` in your project directory. Files `input/day-x.input` should contain your personal AoC input and `input/day-x.example` should be empty. You can copy paste the public example given by AoC in those `.example` files

## Usage

Here's how to use the CLI for common tasks:

- Create a new solution for a specific day:

  ```bash
  ./aoc-hs.sh new -d <day>
  ```

- Run a solution for a specific day and part:
  
  ```bash
  ./aoc-hs.sh run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]
  ```

For more details on using the CLI run `./aoc-hs.sh --help`

## Haskell details

Each day is built as an executable. Each executable depends on these libraries

- [attoparsec](https://hackage.haskell.org/package/attoparsec): for parsing inputs
- [base >=4.7 && <5](https://hackage.haskell.org/package/base): the base package
- [bytestring](https://hackage.haskell.org/package/bytestring): bytestring is the string format used by attoparsec
- [containers](https://hackage.haskell.org/package/containers): containers for generic containers like Map, Set, etc...
- [matrix](https://hackage.haskell.org/package/matrix): because AoC love two-dimensional discrete problems
- [vector](https://hackage.haskell.org/package/vector): `Int` based array. Oftenly a replacement for Haskell's Lists
- [split](https://hackage.haskell.org/package/split): algorithms to split lists
- [search-algorithms](https://hackage.haskell.org/package/search-algorithms): a blessed interface to bfs, dfs, dijkstra, etc...
- [mtl](https://hackage.haskell.org/package/mtl): Just in case you need the state monad

The dependencies are chosen to match a typical AoC season. 

## Contributing

Contributions are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request.

## License

This project is licensed under the WTFPL License - see the [LICENSE](LICENSE) file for details.
