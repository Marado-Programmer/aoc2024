import argv
import day_00
import gleam/int
import gleam/io
import gleam/result

@external(erlang, "Elixir.File", "read")
pub fn read(path: String) -> Result(String, String)

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
      #(day, day >= 0 || day <= 31)
    }
      |> result.unwrap(#(-1, False)),
    {
      use part <- result.map(int.parse(part))
      #(part, part == 1 || part == 2)
    }
      |> result.unwrap(#(-1, False)),
  )
}

fn get_implementation(day, part) {
  case day, part {
    #(day, True), #(part, True) ->
      case day, part {
        0, 1 -> day_00.part_1
        0, 2 -> day_00.part_2
        _, _ -> panic as "not implemented"
      }
    #(_, False), #(_, False) -> panic as "invalid day and part"
    #(_, False), _ -> panic as "invalid day"
    _, #(_, False) -> panic as "invalid part"
  }
}
