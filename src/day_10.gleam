import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import utils/matrix
import utils/utils

pub fn part_1(in: String) -> Int {
  let mat =
    matrix.create_matrix_from_string(in)
    |> matrix.matrix_map(fn(x) { #(result.unwrap(int.parse(x), -1), False) })
  matrix.matrix_fold_index(mat, [], fn(acc, x, i) {
    case x {
      #(0, False) -> [i, ..acc]
      _ -> acc
    }
  })
  |> list.map(calc_trailhead_score(mat, _))
  |> list.fold([], list.append)
  |> set.from_list()
  |> set.size()
}

pub fn part_2(in: String) -> Int {
  let mat =
    matrix.create_matrix_from_string(in)
    |> matrix.matrix_map(fn(x) { #(result.unwrap(int.parse(x), -1), False) })
  matrix.matrix_fold_index(mat, [], fn(acc, x, i) {
    case x {
      #(0, False) -> [i, ..acc]
      _ -> acc
    }
  })
  |> list.map(calc_trailhead_score(mat, _))
  |> list.fold([], list.append)
  |> list.length()
}

type Matrix =
  matrix.Matrix2(#(Int, Bool))

const dirs = [#(1, 0), #(0, 1), #(-1, 0), #(0, -1)]

fn calc_trailhead_score(mat: Matrix, head: matrix.Point2) -> List(#(Int, Int)) {
  case matrix.in_matrix_or(mat, head, utils.return, #(-1, True)) {
    #(0, False) -> {
      let #(x, y) = head
      let mat = matrix.matrix_change(mat, head, #(0, True))
      use acc, #(x_dir, y_dir) <- list.fold(dirs, [])
      list.append(
        calc_trailhead_score_loop(mat, #(x + x_dir, y + y_dir), 1),
        acc,
      )
    }
    _ -> []
  }
}

fn calc_trailhead_score_loop(
  mat: Matrix,
  head: matrix.Point2,
  level: Int,
) -> List(#(Int, Int)) {
  case matrix.in_matrix_or(mat, head, utils.return, #(-1, True)) {
    #(a, False) if a == 9 && a == level -> [head]
    #(a, False) if a == level -> {
      let #(x, y) = head
      let mat = matrix.matrix_change(mat, head, #(a, True))
      use acc, #(x_dir, y_dir) <- list.fold(dirs, [])
      list.append(
        calc_trailhead_score_loop(mat, #(x + x_dir, y + y_dir), level + 1),
        acc,
      )
    }
    _ -> []
  }
}
