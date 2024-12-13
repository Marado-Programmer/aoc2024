import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/set
import utils/matrix
import utils/utils

pub fn part_1(in: String) -> Int {
  let #(list, _) =
    matrix.create_matrix_from_string(in)
    |> matrix.matrix_map(fn(x) { #(x, False) })
    |> find_areas([], dict.new())
  list.fold(list, 0, fn(acc, v) {
    let #(a, p, _) = v
    acc + { a * p }
  })
}

pub fn part_2(in: String) -> Int {
  let #(list, _) =
    matrix.create_matrix_from_string(in)
    |> matrix.matrix_map(fn(x) { #(x, False) })
    |> find_areas([], dict.new())
  list.fold(list, 0, fn(acc, v) {
    let #(a, _, s) = v
    acc + { a * s }
  })
}

type Matrix =
  matrix.Matrix2(#(String, Bool))

const dirs = [#(1, 0), #(0, 1), #(-1, 0), #(0, -1)]

fn find_areas(
  mat: Matrix,
  list: List(#(Int, Int, Int)),
  dict: dict.Dict(String, #(Int, Int, Int)),
) -> #(List(#(Int, Int, Int)), dict.Dict(String, #(Int, Int, Int))) {
  case
    {
      use #(_, walked) <- matrix.matrix_first_index(mat)
      !walked
    }
  {
    option.None -> {
      #(list, dict)
    }
    option.Some(pos) -> {
      let #(id, _) = matrix.in_matrix_or(mat, pos, utils.return, #("", True))
      let #(walked_mat, id, areas, h_perimetre, v_perimetre) =
        create_area(mat, pos, id, set.new(), dict.new(), dict.new())

      let h_perimetre = h_perimetre |> dict.filter(fn(_, i) { i == 1 })
      let v_perimetre = v_perimetre |> dict.filter(fn(_, i) { i == 1 })

      let area = areas |> set.size()
      let perimetre =
        { h_perimetre |> dict.size() } + { v_perimetre |> dict.size() }

      io.debug(id)
      let sides =
        {
          h_perimetre
          |> dict.keys()
          |> calc_sides_h(v_perimetre |> dict.keys())
        }
        + {
          v_perimetre
          |> dict.keys()
          |> calc_sides_v(h_perimetre |> dict.keys())
        }
      io.debug(id)

      find_areas(
        walked_mat,
        [#(area, perimetre, sides), ..list],
        dict
          |> dict.upsert(id, fn(v) {
            case v {
              option.None -> #(area, perimetre, sides)
              option.Some(#(last_area, last_perimeter, last_sides)) -> #(
                area + last_area,
                perimetre + last_perimeter,
                sides + last_sides,
              )
            }
          }),
      )
    }
  }
}

fn create_area(
  mat: Matrix,
  pos: #(Int, Int),
  cur_id: String,
  p: set.Set(#(Int, Int)),
  hp: dict.Dict(#(Int, Int), Int),
  vp: dict.Dict(#(Int, Int), Int),
) -> #(
  Matrix,
  String,
  set.Set(#(Int, Int)),
  dict.Dict(#(Int, Int), Int),
  dict.Dict(#(Int, Int), Int),
) {
  case matrix.in_matrix_or(mat, pos, utils.return, #("", True)) {
    #(id, False) if cur_id == id -> {
      let mat = matrix.matrix_change(mat, pos, #(id, True))
      let p = p |> set.insert(pos)
      let #(x, y) = pos
      let vp =
        vp
        |> dict.upsert(pos, fn(v) {
          case v {
            option.None -> 1
            option.Some(v) -> v + 1
          }
        })
        |> dict.upsert(#(x + 1, y), fn(v) {
          case v {
            option.None -> 1
            option.Some(v) -> v + 1
          }
        })
      let hp =
        hp
        |> dict.upsert(pos, fn(v) {
          case v {
            option.None -> 1
            option.Some(v) -> v + 1
          }
        })
        |> dict.upsert(#(x, y + 1), fn(v) {
          case v {
            option.None -> 1
            option.Some(v) -> v + 1
          }
        })
      let #(mat, p, hp, vp) =
        list.fold(dirs, #(mat, p, hp, vp), fn(acc, dir) {
          let #(mat, p, hp, vp) = acc
          let #(x_dir, y_dir) = dir
          let #(mat, _, p, hp, vp) =
            create_area(mat, #(x + x_dir, y + y_dir), cur_id, p, hp, vp)
          #(mat, p, hp, vp)
        })
      #(mat, id, p, hp, vp)
    }
    _ -> #(mat, cur_id, p, hp, vp)
  }
}

fn calc_sides_h(sides: List(#(Int, Int)), osides: List(#(Int, Int))) -> Int {
  sides
  |> list.group(fn(pos) {
    let #(_, y) = pos
    y
  })
  |> dict.values()
  |> list.map(fn(x) {
    let #(_, n) =
      list.fold(x, #(option.None, 1), fn(acc, pos) {
        let #(x, y) = pos
        #(option.Some(x), case acc {
          #(option.None, acc) -> acc
          #(option.Some(last_x), acc) -> {
            let diff = last_x - x
            case
              int.absolute_value(diff) > 1
              || {
                list.any(osides, fn(a) {
                  case a {
                    #(a, b) if a == x + diff && b == y - 1 -> True
                    _ -> False
                  }
                })
                && list.any(osides, fn(a) {
                  case a {
                    #(a, b) if a == x + diff && b == y -> True
                    _ -> False
                  }
                })
              }
            {
              True -> acc + 1
              False -> acc
            }
          }
        })
      })
    n
  })
  |> int.sum()
}

fn calc_sides_v(sides: List(#(Int, Int)), osides: List(#(Int, Int))) -> Int {
  sides
  |> list.group(fn(pos) {
    let #(x, _) = pos
    x
  })
  |> dict.values()
  |> list.map(fn(x) {
    let #(_, n) =
      list.fold(x, #(option.None, 1), fn(acc, pos) {
        let #(x, y) = pos
        #(option.Some(y), case acc {
          #(option.None, acc) -> acc
          #(option.Some(last_y), acc) -> {
            let diff = last_y - y
            case
              int.absolute_value(diff) > 1
              || {
                list.any(osides, fn(a) {
                  case a {
                    #(a, b) if a == x - 1 && b == y + diff -> True
                    _ -> False
                  }
                })
                && list.any(osides, fn(a) {
                  case a {
                    #(a, b) if a == x && b == y + diff -> True
                    _ -> False
                  }
                })
              }
            {
              True -> acc + 1
              False -> acc
            }
          }
        })
      })
    n
  })
  |> int.sum()
}
