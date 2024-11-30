# aoc2024

[Advent of Code](https://adventofcode.com/) 2024 usign BEAM languages.

# Usage
```sh
gleam run day <day> part <part> <input_file>
```

# Use as a lib

[![Package Version](https://img.shields.io/hexpm/v/aoc2024)](https://hex.pm/packages/aoc2024)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/aoc2024/)

```sh
gleam add aoc2024@1
```
```gleam
import aoc2024.{get_implementation}
import gleam/int
import gleam/io

@external(erlang, "Elixir.File", "read")
fn read(path: String) -> Result(String, String)

pub fn main() {
  let assert Ok(in) = read("./data.in")
  in
  |> get_implementation(day, part)
  |> int.to_string()
  |> io.println()
}
```

Further documentation can be found at <https://hexdocs.pm/aoc2024>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
