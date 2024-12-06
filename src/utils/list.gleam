pub fn nth(list: List(a), pos: Int) -> Result(a, Nil) {
  case list, pos {
    [x, ..], 0 -> Ok(x)
    [_, ..rest], pos if pos > 0 -> nth(rest, pos - 1)
    [_, ..], _ -> Error(Nil)
    [], _ -> Error(Nil)
  }
}

pub fn index_of(list: List(a), func: fn(a) -> Bool) -> Result(Int, Nil) {
  index_of_loop(list, func, 0)
}

fn index_of_loop(
  list: List(a),
  func: fn(a) -> Bool,
  pos: Int,
) -> Result(Int, Nil) {
  case list {
    [x, ..rest] ->
      case func(x) {
        True -> Ok(pos)
        _ -> index_of_loop(rest, func, pos + 1)
      }
    [] -> Error(Nil)
  }
}

pub fn index_of_map(
  list: List(a),
  func: fn(a, Int) -> Result(b, Nil),
) -> Result(b, Nil) {
  index_of_map_loop(list, func, 0)
}

fn index_of_map_loop(
  list: List(a),
  func: fn(a, Int) -> Result(b, Nil),
  pos: Int,
) -> Result(b, Nil) {
  case list {
    [x, ..rest] ->
      case func(x, pos) {
        Ok(_) as res -> res
        _ -> index_of_map_loop(rest, func, pos + 1)
      }
    [] -> Error(Nil)
  }
}
