import gleam/io
import gleam/int
import gleam/list
import gleam/option
import gleam/regex
import gleam/result
import gleam/string

pub fn part_1(in: String) -> Int {
  let assert Ok(reg) = regex.from_string("mul\\(([0-9]+),([0-9]+)\\)")
  regex.scan(reg, in)
  |> list.map(fn(x) { x.submatches })
  |> list.map(fn(x) {
    list.map(x, fn(y) { option.unwrap(y, "-1") })
    |> list.map(int.parse)
    |> list.map(fn(y) { result.unwrap(y, 0) })
  })
  |> list.map(fn(x) {
    let assert [y, z] = x
    y * z
  })
  |> int.sum
}

pub fn part_2(in: String) -> Int {
  let assert [first, ..parts] = string.split(in, "don't()")
  io.debug(first)
  io.debug(parts)
  part_1(first)
  + {
    list.map(parts, fn(x) {
      case string.split_once(x, "do()") {
        Ok(#(_, dos)) -> dos
        Error(_) -> ""
      }
    })
    |> list.map(part_1)
    |> int.sum
  }
}
