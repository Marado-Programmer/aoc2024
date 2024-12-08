import gleam/option

pub fn nth(list: List(a), pos: Int) -> option.Option(a) {
  case list, pos {
    [x, ..], 0 -> option.Some(x)
    [_, ..rest], pos if pos > 0 -> nth(rest, pos - 1)
    [_, ..], _ -> option.None
    [], _ -> option.None
  }
}

pub fn index_of(list: List(a), func: fn(a) -> Bool) -> option.Option(Int) {
  index_of_loop(list, func, 0)
}

fn index_of_loop(
  list: List(a),
  func: fn(a) -> Bool,
  pos: Int,
) -> option.Option(Int) {
  case list {
    [x, ..rest] ->
      case func(x) {
        True -> option.Some(pos)
        _ -> index_of_loop(rest, func, pos + 1)
      }
    [] -> option.None
  }
}

pub fn index_of_map(
  list: List(a),
  func: fn(a, Int) -> option.Option(b),
) -> option.Option(b) {
  index_of_map_loop(list, func, 0)
}

fn index_of_map_loop(
  list: List(a),
  func: fn(a, Int) -> option.Option(b),
  pos: Int,
) -> option.Option(b) {
  case list {
    [x, ..rest] ->
      case func(x, pos) {
        option.Some(_) as res -> res
        _ -> index_of_map_loop(rest, func, pos + 1)
      }
    [] -> option.None
  }
}
