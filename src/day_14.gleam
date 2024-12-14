import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import utils/matrix

//const w = 11
const w = 101

//const h = 7
const h = 103

pub fn part_1(in: String) -> Int {
  let halfx1 = { float.truncate(float.floor(int.to_float(w) /. 2.0)) } - 1
  let halfx2 = {
    float.truncate(float.ceiling(int.to_float(w) /. 2.0))
  }
  let halfy1 = { float.truncate(float.floor(int.to_float(h) /. 2.0)) } - 1
  let halfy2 = {
    float.truncate(float.ceiling(int.to_float(h) /. 2.0))
  }
  let assert Ok(reg) = regex.from_string("(-?[0-9]+)")
  let #(q1, q2, q3, q4) =
    string.trim(in)
    |> string.split("\n")
    |> list.fold(#(0, 0, 0, 0), fn(acc, x) {
      let #(q1, q2, q3, q4) = acc
      let assert [x0, y0, vx, vy] =
        regex.scan(reg, x)
        |> list.map(fn(x) { result.unwrap(int.parse(x.content), -1) })

      let x =
        case { x0 + 100 * vx } {
          x if x < 0 -> w + { x % w }
          x -> x
        }
        % w

      let y =
        case { y0 + 100 * vy } {
          y if y < 0 -> h + { y % h }
          y -> y
        }
        % h

      case x, y {
        x, y if x >= halfx2 && x <= w - 1 && y >= 0 && y <= halfy1 -> #(
          q1 + 1,
          q2,
          q3,
          q4,
        )
        x, y if x >= 0 && x <= halfx1 && y >= 0 && y <= halfy1 -> #(
          q1,
          q2 + 1,
          q3,
          q4,
        )
        x, y if x >= 0 && x <= halfx1 && y >= halfy2 && y <= h - 1 -> #(
          q1,
          q2,
          q3 + 1,
          q4,
        )
        x, y if x >= halfx2 && x <= w - 1 && y >= halfy2 && y <= h - 1 -> #(
          q1,
          q2,
          q3,
          q4 + 1,
        )
        _, _ -> acc
      }
    })
  q1 * q2 * q3 * q4
}

pub fn part_2(in: String) -> Int {
  let assert Ok(reg) = regex.from_string("(-?[0-9]+)")
  let robots =
    string.trim(in)
    |> string.split("\n")
    |> list.map(fn(x) {
      regex.scan(reg, x)
      |> list.map(fn(x) { result.unwrap(int.parse(x.content), -1) })
    })

  list.range(1000, 10000)
  |> list.each(fn(lvl) {
    io.println("\n------------------------------------------------------------" <> int.to_string(lvl) <> "------------------------------------------------------------\n")
    {
      use acc, robot <- list.fold(robots, matrix.create_matrix(w, h, " "))
      let assert [x0, y0, vx, vy] = robot
      let x =
        case { x0 + lvl * vx } {
          x if x < 0 -> w + { x % w }
          x -> x
        }
        % w

      let y =
        case { y0 + lvl * vy } {
          y if y < 0 -> h + { y % h }
          y -> y
        }
        % h

      //matrix.matrix_change(acc, #(x, y), int.to_string(lvl))
      matrix.matrix_change(acc, #(x, y), "#")
    } |> matrix.to_string() |> io.println()
    io.println("\n------------------------------------------------------------" <> int.to_string(lvl) <> "------------------------------------------------------------\n")
  })
  -1
}
