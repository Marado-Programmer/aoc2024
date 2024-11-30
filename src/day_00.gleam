import gleam/int
import gleam/list
import gleam/string

@external(erlang, "day_00_impl", "part_1")
pub fn part_1(in: String) -> Int

//@external(erlang, "Elixir.Day_00_impl", "part_2")
pub fn part_2(in: String) -> Int {
  string.trim(in)
  |> string.split("\n")
  |> find_calibration_values()
  |> list.fold(0, int.add)
}

fn find_calibration_values(lines: List(String)) {
  find_calibration_values_loop(lines, [])
}

fn find_calibration_values_loop(lines: List(String), acc: List(Int)) {
  case lines {
    [head, ..rest] ->
      find_calibration_values_loop(rest, [calibration_value(head), ..acc])
    [] -> acc
  }
}

fn calibration_value(line: String) -> Int {
  first_digit_from_left(line) * 10 + first_digit_from_right(line)
}

const numbers = [
  "0", "zero", "1", "one", "2", "two", "3", "three", "4", "four", "5", "five",
  "6", "six", "7", "seven", "8", "eight", "9", "nine",
]

fn first_digit_from_left(line: String) -> Int {
  case list.find(numbers, string.starts_with(line, _)) {
    Error(Nil) -> first_digit_from_left(string.drop_left(line, 1))
    Ok(digit) ->
      case digit {
        "0" | "zero" -> 0
        "1" | "one" -> 1
        "2" | "two" -> 2
        "3" | "three" -> 3
        "4" | "four" -> 4
        "5" | "five" -> 5
        "6" | "six" -> 6
        "7" | "seven" -> 7
        "8" | "eight" -> 8
        "9" | "nine" -> 9
        _ -> panic
      }
  }
}

fn first_digit_from_right(line: String) -> Int {
  case list.find(numbers, string.ends_with(line, _)) {
    Error(_) -> first_digit_from_right(string.drop_right(line, 1))
    Ok(digit) ->
      case digit {
        "0" | "zero" -> 0
        "1" | "one" -> 1
        "2" | "two" -> 2
        "3" | "three" -> 3
        "4" | "four" -> 4
        "5" | "five" -> 5
        "6" | "six" -> 6
        "7" | "seven" -> 7
        "8" | "eight" -> 8
        "9" | "nine" -> 9
        _ -> panic
      }
  }
}
