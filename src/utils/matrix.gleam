import gleam/list
import gleam/option
import gleam/string
import utils/list.{nth} as _

pub type Matrix2(a) =
  List(List(a))

pub type Point2 =
  #(Int, Int)

pub fn create_matrix_from_string(str: String) -> Matrix2(String) {
  string.trim(str)
  |> string.split("\n")
  |> list.map(string.to_graphemes)
}

pub fn in_matrix_or(mat: Matrix2(a), pos: Point2, do: fn(a) -> b, or: b) -> b {
  let #(x, y) = pos
  case nth(mat, y) {
    option.Some(line) ->
      case nth(line, x) {
        option.Some(a) -> do(a)
        _ -> or
      }
    _ -> or
  }
}

pub fn matrix_change(mat: Matrix2(a), pos: Point2, v: a) -> Matrix2(a) {
  let #(x, y) = pos
  let #(_, new) = {
    use #(cur_y, acc), line <- list.fold(mat, #(0, []))
    #(cur_y + 1, [
      case cur_y == y {
        True -> {
          let #(_, new) = {
            use #(cur_x, acc), a <- list.fold(line, #(0, []))
            #(cur_x + 1, [
              case cur_x == x {
                True -> v
                False -> a
              },
              ..acc
            ])
          }
          list.reverse(new)
        }
        False -> line
      },
      ..acc
    ])
  }
  list.reverse(new)
}

pub fn matrix_fold(mat: Matrix2(a), acc: b, f: fn(b, a) -> b) -> b {
  use acc, line <- list.fold(mat, acc)
  list.fold(line, acc, f)
}

pub fn matrix_fold_index(mat: Matrix2(a), acc: b, f: fn(b, a, Point2) -> b) -> b {
  let #(_, b) = {
    use #(cur_y, acc), line <- list.fold(mat, #(0, acc))
    #(cur_y + 1, {
      let #(_, b) =
        list.fold(line, #(0, acc), fn(acc, x) {
          let #(cur_x, acc) = acc
          #(cur_x + 1, f(acc, x, #(cur_x, cur_y)))
        })
      b
    })
  }
  b
}

pub fn to_string(mat: Matrix2(String)) -> String {
  use acc, line <- list.fold(mat, "")
  acc <> list.fold(line, "", fn(acc, x) { acc <> x }) <> "\n"
}
