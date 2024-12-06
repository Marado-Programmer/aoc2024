import gleam/set
import utils/list.{index_of_map}
import utils/matrix.{
  type Matrix2, type Point2, create_matrix_from_string, in_matrix_or,
  matrix_change,
}

pub fn part_1(in: String) -> Int {
  let map = create_matrix_from_string(in)
  let assert Ok(init) = find_init_pos(map, "^")
  let dir = get_dir_by_char("^")
  save_path(map, init, dir)
  |> set.size()
}

pub fn part_2(in: String) -> Int {
  let map = create_matrix_from_string(in)
  let assert Ok(init) = find_init_pos(map, "^")
  let dir = get_dir_by_char("^")
  save_obstructions_to_loop(map, init, dir)
  |> set.delete(init)
  |> set.size()
}

fn find_init_pos(map: Matrix2(String), init: String) -> Result(Point2, Nil) {
  use line, y <- index_of_map(map)
  use char, x <- index_of_map(line)
  case char == init {
    True -> Ok(#(x, y))
    _ -> Error(Nil)
  }
}

fn save_path(
  map: List(List(String)),
  pos: #(Int, Int),
  dir: #(Int, Int),
) -> set.Set(#(Int, Int)) {
  save_path_loop(map, pos, dir, set.new())
}

fn save_path_loop(
  map: List(List(String)),
  pos: #(Int, Int),
  dir: #(Int, Int),
  steps: set.Set(#(Int, Int)),
) -> set.Set(#(Int, Int)) {
  let steps = steps |> set.insert(pos)
  let #(dir_x, dir_y) = dir
  let #(x, y) = pos
  let new_pos = #(x + dir_x, y + dir_y)
  use char <- in_matrix_or(map, new_pos, _, steps)
  case char {
    "#" -> save_path_loop(map, pos, get_next_dir(dir), steps)
    _ -> save_path_loop(map, new_pos, dir, steps)
  }
}

fn save_obstructions_to_loop(
  map: List(List(String)),
  pos: #(Int, Int),
  dir: #(Int, Int),
) -> set.Set(#(Int, Int)) {
  save_obstructions_to_loop_loop(map, pos, dir, set.new(), set.new())
}

fn save_obstructions_to_loop_loop(
  map: List(List(String)),
  pos: #(Int, Int),
  dir: #(Int, Int),
  steps: set.Set(#(#(Int, Int), #(Int, Int))),
  acc: set.Set(#(Int, Int)),
) -> set.Set(#(Int, Int)) {
  steps |> set.insert(#(pos, dir))
  let #(dir_x, dir_y) = dir
  let #(x, y) = pos
  let new_pos = #(x + dir_x, y + dir_y)
  let new_dir = get_next_dir(dir)

  use char <- in_matrix_or(map, new_pos, _, acc)
  case char {
    "#" -> save_obstructions_to_loop_loop(map, pos, new_dir, steps, acc)
    _ ->
      save_obstructions_to_loop_loop(map, new_pos, dir, steps, case
        map |> add_wall(new_pos) |> does_loop(pos, new_dir, steps)
      {
        True -> acc |> set.insert(new_pos)
        False -> acc
      })
  }
}

fn does_loop(
  map: List(List(String)),
  pos: #(Int, Int),
  dir: #(Int, Int),
  steps: set.Set(#(#(Int, Int), #(Int, Int))),
) -> Bool {
  case steps |> set.contains(#(pos, dir)) {
    True -> True
    False -> {
      let steps = steps |> set.insert(#(pos, dir))
      let #(x, y) = pos
      let #(dir_x, dir_y) = dir
      let new_pos = #(x + dir_x, y + dir_y)
      use char <- in_matrix_or(map, new_pos, _, False)
      case char {
        "#" -> does_loop(map, pos, get_next_dir(dir), steps)
        _ -> does_loop(map, new_pos, dir, steps)
      }
    }
  }
}

fn add_wall(map: List(List(String)), pos: #(Int, Int)) -> List(List(String)) {
  matrix_change(map, pos, "#")
}

fn get_dir_by_char(char: String) -> #(Int, Int) {
  case char {
    "^" -> #(0, -1)
    _ -> panic
  }
}

fn get_next_dir(dir: #(Int, Int)) -> #(Int, Int) {
  case dir {
    #(0, -1) -> #(1, 0)
    #(1, 0) -> #(0, 1)
    #(0, 1) -> #(-1, 0)
    #(-1, 0) -> #(0, -1)
    _ -> panic
  }
}
