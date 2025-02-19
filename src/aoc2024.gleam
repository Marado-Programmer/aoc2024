import argv
import day_00
import day_01
import day_02
import day_03
import day_04
import day_05
import day_06
import day_07
import day_08
import day_09
import day_10
import day_11
import day_12
import day_13
import day_14
import day_15
import day_16
import gleam/int
import gleam/io
import gleam/result

@external(erlang, "Elixir.File", "read")
fn read(path: String) -> Result(String, String)

pub fn main() {
  case argv.load().arguments {
    ["day", day, "part", part, in] -> {
      let #(day, part) = validate_day_and_part(day, part)
      let assert Ok(in) = read(in)
      in |> get_implementation(day, part)
    }
    _ -> panic as "day <day> part <part> <input_file>"
  }
  |> int.to_string()
  |> io.println()
}

fn validate_day_and_part(day, part) {
  #(
    {
      use day <- result.map(int.parse(day))
      #(day, day >= 0 && day <= 31)
    }
      |> result.unwrap(#(-1, False)),
    {
      use part <- result.map(int.parse(part))
      #(part, part == 1 || part == 2)
    }
      |> result.unwrap(#(-1, False)),
  )
}

pub fn get_implementation(day, part) {
  case day, part {
    #(day, True), #(part, True) ->
      case day, part {
        0, 1 -> day_00.part_1
        0, 2 -> day_00.part_2
        1, 1 -> day_01.part_1
        1, 2 -> day_01.part_2
        2, 1 -> day_02.part_1
        2, 2 -> day_02.part_2
        3, 1 -> day_03.part_1
        3, 2 -> day_03.part_2
        4, 1 -> day_04.part_1
        4, 2 -> day_04.part_2
        5, 1 -> day_05.part_1
        5, 2 -> day_05.part_2
        6, 1 -> day_06.part_1
        6, 2 -> day_06.part_2
        7, 1 -> day_07.part_1
        7, 2 -> day_07.part_2
        8, 1 -> day_08.part_1
        8, 2 -> day_08.part_2
        9, 1 -> day_09.part_1
        9, 2 -> day_09.part_2
        10, 1 -> day_10.part_1
        10, 2 -> day_10.part_2
        11, 1 -> day_11.part_1
        11, 2 -> day_11.part_2
        12, 1 -> day_12.part_1
        12, 2 -> day_12.part_2
        13, 1 -> day_13.part_1
        13, 2 -> day_13.part_2
        14, 1 -> day_14.part_1
        14, 2 -> day_14.part_2
        15, 1 -> day_15.part_1
        15, 2 -> day_15.part_2
        16, 1 -> day_16.part_1
        16, 2 -> day_16.part_2
        _, _ -> panic as "not implemented"
      }
    #(_, False), #(_, False) -> panic as "invalid day and part"
    #(_, False), _ -> panic as "invalid day"
    _, #(_, False) -> panic as "invalid part"
  }
}
