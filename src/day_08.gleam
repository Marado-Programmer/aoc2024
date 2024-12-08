import gleam/dict
import gleam/list
import gleam/option
import gleam/set
import utils/matrix.{type Matrix2, type Point2, create_matrix_from_string}

pub fn part_1(in: String) -> Int {
  let mat = create_matrix_from_string(in)
  {
    use acc, x, point <- matrix.matrix_fold_index(mat, dict.new())
    case x {
      "." -> acc
      _ ->
        case acc |> dict.get(x) {
          Ok(l) -> dict.insert(acc, x, [point, ..l])
          _ -> dict.insert(acc, x, [point])
        }
    }
  }
  |> dict.fold(set.new(), fn(acc, _, points) {
    find_antinodes(mat, option.None, points, acc)
  })
  |> set.size()
}

pub fn part_2(in: String) -> Int {
  let mat = create_matrix_from_string(in)
  {
    use acc, x, point <- matrix.matrix_fold_index(mat, dict.new())
    case x {
      "." -> acc
      _ ->
        case acc |> dict.get(x) {
          Ok(l) -> dict.insert(acc, x, [point, ..l])
          _ -> dict.insert(acc, x, [point])
        }
    }
  }
  |> dict.fold(set.new(), fn(acc, _, points) {
    find_antinodes_forever(mat, option.None, points, acc)
  })
  |> set.size()
}

fn valid_distance(x: Point2, y: Point2) -> Bool {
  let #(xx, xy) = x
  let #(yx, yy) = y
  xx != yx && xy != yy
}

fn find_antinodes(
  mat: Matrix2(String),
  init: option.Option(Point2),
  points: List(Point2),
  antinodes: set.Set(Point2),
) -> set.Set(Point2) {
  case points {
    [a, ..rest] ->
      case init {
        option.None ->
          find_antinodes(
            mat,
            option.None,
            rest,
            find_antinodes(mat, option.Some(a), rest, antinodes),
          )
        option.Some(b) -> {
          find_antinodes(mat, init, rest, case valid_distance(a, b) {
            True -> calc_antinode(mat, a, b, antinodes)
            False -> antinodes
          })
        }
      }
    [] -> antinodes
  }
}

fn find_antinodes_forever(
  mat: Matrix2(String),
  init: option.Option(Point2),
  points: List(Point2),
  antinodes: set.Set(Point2),
) -> set.Set(Point2) {
  case points {
    [a, ..rest] ->
      case init {
        option.None ->
          find_antinodes_forever(
            mat,
            option.None,
            rest,
            find_antinodes_forever(mat, option.Some(a), rest, antinodes),
          )
        option.Some(b) -> {
          find_antinodes_forever(mat, init, rest, case valid_distance(a, b) {
            True -> calc_antinodes(mat, a, b, antinodes)
            False -> antinodes
          })
        }
      }
    [] -> antinodes
  }
}

fn calc_antinode(
  mat: Matrix2(String),
  x: Point2,
  y: Point2,
  acc: set.Set(Point2),
) -> set.Set(Point2) {
  let #(xx, xy) = x
  let #(yx, yy) = y
  let dir_x = yx - xx
  let dir_y = yy - xy
  calc_antinodes_loop(mat, y, #(dir_x, dir_y), acc, option.Some(1))
  |> calc_antinodes_loop(mat, x, #(-dir_x, -dir_y), _, option.Some(1))
}

fn calc_antinodes(
  mat: Matrix2(String),
  a: Point2,
  b: Point2,
  acc: set.Set(Point2),
) -> set.Set(Point2) {
  let #(ax, ay) = a
  let #(bx, by) = b
  let dir_x = bx - ax
  let dir_y = by - ay
  acc
  |> set.insert(a)
  |> set.insert(b)
  |> calc_antinodes_loop(mat, b, #(dir_x, dir_y), _, option.None)
  |> calc_antinodes_loop(mat, a, #(-dir_x, -dir_y), _, option.None)
}

fn calc_antinodes_loop(
  mat: Matrix2(String),
  point: Point2,
  dir: Point2,
  acc: set.Set(Point2),
  count: option.Option(Int),
) -> set.Set(Point2) {
  case count {
    option.Some(0) -> acc
    _ -> {
      let h = list.length(mat)
      let assert [line, ..] = mat
      let w = list.length(line)
      let #(x, y) = point
      let #(dir_x, dir_y) = dir
      let new = #(x + dir_x, y + dir_y)
      case new {
        #(x, y) if x < 0 || y < 0 || x >= w || y >= h -> acc
        _ ->
          calc_antinodes_loop(
            mat,
            new,
            dir,
            acc |> set.insert(new),
            option.map(count, fn(c) { c - 1 }),
          )
      }
    }
  }
}
