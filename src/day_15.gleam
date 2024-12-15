import gleam/io
import gleam/option
import gleam/string
import utils/matrix
import utils/utils

pub fn part_1(in: String) -> Int {
  let assert [map_str, moves] = string.trim(in) |> string.split("\n\n")
  let map = matrix.create_matrix_from_string(map_str)
  let assert option.Some(init) =
    matrix.matrix_first_index(map, fn(x) { x == "@" })
  walk(map, init, moves, push)
  |> matrix.matrix_fold_index(0, fn(acc, x, i) {
    case x, i {
      "O", #(x, y) -> acc + x + 100 * y
      _, _ -> acc
    }
  })
}

pub fn part_2(in: String) -> Int {
  let assert [map_str, moves] = string.trim(in) |> string.split("\n\n")
  let map =
    matrix.create_matrix_from_string(
      map_str
      |> string.replace("#", "##")
      |> string.replace("O", "[]")
      |> string.replace(".", "..")
      |> string.replace("@", "@."),
    )
  let assert option.Some(init) =
    matrix.matrix_first_index(map, fn(x) { x == "@" })
  walk(map, init, moves, push_big)
  |> matrix.matrix_fold_index(0, fn(acc, x, i) {
    case x, i {
      "[", #(x, y) -> acc + x + 100 * y
      _, _ -> acc
    }
  })
}

type Map =
  matrix.Matrix2(String)

fn walk(
  map: Map,
  pos: matrix.Point2,
  moves: String,
  push: fn(Map, matrix.Point2, matrix.Point2) -> #(Map, matrix.Point2),
) -> Map {
  case moves {
    "" -> map
    "^" <> rest -> {
      let #(map, pos) = push(map, pos, #(0, -1))
      walk(map, pos, rest, push)
    }
    "v" <> rest -> {
      let #(map, pos) = push(map, pos, #(0, 1))
      walk(map, pos, rest, push)
    }
    ">" <> rest -> {
      let #(map, pos) = push(map, pos, #(1, 0))
      walk(map, pos, rest, push)
    }
    "<" <> rest -> {
      let #(map, pos) = push(map, pos, #(-1, 0))
      walk(map, pos, rest, push)
    }
    _ -> walk(map, pos, string.slice(moves, 1, string.length(moves)), push)
  }
}

fn push(
  map: Map,
  pos: matrix.Point2,
  dir: matrix.Point2,
) -> #(Map, matrix.Point2) {
  let #(x, y) = pos
  let #(dir_x, dir_y) = dir
  let new_pos = #(x + dir_x, y + dir_y)
  case matrix.in_matrix_or(map, new_pos, utils.return, "#") {
    "#" -> #(map, pos)
    "O" -> {
      let map = push_loop(map, new_pos, dir)
      case matrix.in_matrix_or(map, new_pos, utils.return, "#") {
        "#" | "O" -> #(map, pos)
        _ -> #(
          map
            |> matrix.matrix_change(pos, ".")
            |> matrix.matrix_change(new_pos, "@"),
          new_pos,
        )
      }
    }
    _ -> #(
      map
        |> matrix.matrix_change(pos, ".")
        |> matrix.matrix_change(new_pos, "@"),
      new_pos,
    )
  }
}

fn push_loop(map: Map, pos: matrix.Point2, dir: matrix.Point2) -> Map {
  let #(x, y) = pos
  let #(dir_x, dir_y) = dir
  let new_pos = #(x + dir_x, y + dir_y)
  case matrix.in_matrix_or(map, new_pos, fn(x) { x }, "#") {
    "#" -> map
    "O" -> {
      let map = push_loop(map, new_pos, dir)
      case matrix.in_matrix_or(map, new_pos, utils.return, "#") {
        "#" | "O" -> map
        _ ->
          map
          |> matrix.matrix_change(new_pos, "O")
          |> matrix.matrix_change(pos, ".")
      }
    }
    _ ->
      map
      |> matrix.matrix_change(new_pos, "O")
      |> matrix.matrix_change(pos, ".")
  }
}

fn push_big(
  map: Map,
  pos: matrix.Point2,
  dir: matrix.Point2,
) -> #(Map, matrix.Point2) {
  let #(x, y) = pos
  let #(dir_x, dir_y) = dir
  let new_pos = #(x + dir_x, y + dir_y)
  case matrix.in_matrix_or(map, new_pos, utils.return, "#") {
    "#" -> #(map, pos)
    "[" -> {
      let map = push_big_loop(map, new_pos, #(x + dir_x + 1, y + dir_y), dir)
      case matrix.in_matrix_or(map, new_pos, utils.return, "#") {
        "#" | "[" | "]" -> #(map, pos)
        _ -> #(
          map
            |> matrix.matrix_change(pos, ".")
            |> matrix.matrix_change(new_pos, "@"),
          new_pos,
        )
      }
    }
    "]" -> {
      let map = push_big_loop(map, #(x + dir_x - 1, y + dir_y), new_pos, dir)
      case matrix.in_matrix_or(map, new_pos, utils.return, "#") {
        "#" | "[" | "]" -> #(map, pos)
        _ -> #(
          map
            |> matrix.matrix_change(pos, ".")
            |> matrix.matrix_change(new_pos, "@"),
          new_pos,
        )
      }
    }
    _ -> #(
      map
        |> matrix.matrix_change(pos, ".")
        |> matrix.matrix_change(new_pos, "@"),
      new_pos,
    )
  }
}

fn push_big_loop(
  map: Map,
  pos1: matrix.Point2,
  pos2: matrix.Point2,
  dir: matrix.Point2,
) -> Map {
  let #(x1, y1) = pos1
  let #(x2, y2) = pos2
  let #(dir_x, dir_y) = dir
  let new_pos1 = #(x1 + dir_x, y1 + dir_y)
  let new_pos2 = #(x2 + dir_x, y2 + dir_y)
  case
    matrix.in_matrix_or(map, new_pos1, fn(x) { x }, "#"),
    matrix.in_matrix_or(map, new_pos2, fn(x) { x }, "#"),
    dir_x
  {
    "#", _, 0 | _, "#", 0 -> map
    "[", "]", 0 -> {
      let map = push_big_loop(map, new_pos1, new_pos2, dir)
      case matrix.in_matrix_or(map, new_pos1, utils.return, "#") {
        "#" | "[" | "]" -> map
        _ ->
          map
          |> matrix.matrix_change(new_pos1, "[")
          |> matrix.matrix_change(new_pos2, "]")
          |> matrix.matrix_change(pos1, ".")
          |> matrix.matrix_change(pos2, ".")
      }
    }
    "]", "[", 0 -> {
      let new_map1 =
        push_big_loop(map, #(x1 + dir_x - 1, y1 + dir_y), new_pos1, dir)
      let new_map2 = case new_map1 == map {
        False ->
          push_big_loop(new_map1, new_pos2, #(x2 + dir_x + 1, y2 + dir_y), dir)
        _ -> map
      }
      let map = case new_map2 == new_map1 {
        False -> new_map2
        _ -> map
      }
      case
        matrix.in_matrix_or(map, new_pos1, utils.return, "#"),
        matrix.in_matrix_or(map, new_pos2, utils.return, "#")
      {
        ".", "." ->
          map
          |> matrix.matrix_change(new_pos1, "[")
          |> matrix.matrix_change(new_pos2, "]")
          |> matrix.matrix_change(pos1, ".")
          |> matrix.matrix_change(pos2, ".")
        _, _ -> map
      }
    }
    "]", _, 0 -> {
      let map = push_big_loop(map, #(x1 + dir_x - 1, y1 + dir_y), new_pos1, dir)
      case matrix.in_matrix_or(map, new_pos1, utils.return, "#") {
        "#" | "[" | "]" -> map
        _ ->
          map
          |> matrix.matrix_change(new_pos1, "[")
          |> matrix.matrix_change(new_pos2, "]")
          |> matrix.matrix_change(pos1, ".")
          |> matrix.matrix_change(pos2, ".")
      }
    }
    _, "[", 0 -> {
      let map = push_big_loop(map, new_pos2, #(x2 + dir_x + 1, y2 + dir_y), dir)
      case matrix.in_matrix_or(map, new_pos2, utils.return, "#") {
        "#" | "[" | "]" -> map
        _ ->
          map
          |> matrix.matrix_change(new_pos1, "[")
          |> matrix.matrix_change(new_pos2, "]")
          |> matrix.matrix_change(pos1, ".")
          |> matrix.matrix_change(pos2, ".")
      }
    }
    _, _, 0 ->
      map
      |> matrix.matrix_change(new_pos1, "[")
      |> matrix.matrix_change(new_pos2, "]")
      |> matrix.matrix_change(pos1, ".")
      |> matrix.matrix_change(pos2, ".")
    "#", _, -1 | _, "#", 1 -> map
    "]", _, -1 -> {
      let map = push_big_loop(map, #(x1 + dir_x - 1, y1 + dir_y), new_pos1, dir)
      case matrix.in_matrix_or(map, new_pos1, utils.return, "#") {
        "#" | "[" | "]" -> map
        _ ->
          map
          |> matrix.matrix_change(pos1, ".")
          |> matrix.matrix_change(pos2, ".")
          |> matrix.matrix_change(new_pos1, "[")
          |> matrix.matrix_change(new_pos2, "]")
      }
    }
    _, "[", 1 -> {
      let map = push_big_loop(map, new_pos2, #(x2 + dir_x + 1, y2 + dir_y), dir)
      case matrix.in_matrix_or(map, new_pos2, utils.return, "#") {
        "#" | "[" | "]" -> map
        _ ->
          map
          |> matrix.matrix_change(pos1, ".")
          |> matrix.matrix_change(pos2, ".")
          |> matrix.matrix_change(new_pos1, "[")
          |> matrix.matrix_change(new_pos2, "]")
      }
    }
    _, _, _ ->
      map
      |> matrix.matrix_change(pos1, ".")
      |> matrix.matrix_change(pos2, ".")
      |> matrix.matrix_change(new_pos1, "[")
      |> matrix.matrix_change(new_pos2, "]")
  }
}
