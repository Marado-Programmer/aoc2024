import gleam/int
import gleam/list
import gleam/regex
import gleam/result
import gleam/string

// [94 22][x] = [8400]
// [34 67][y] = [5400]

pub fn part_1(in: String) -> Int {
  solutions(in, 0)
  |> list.fold(0, fn(acc, x) {
    let #(a, b, deta, detb, det) = x
    case a, b {
      a, b
        if a > 100
        || b > 100
        || a < 0
        || b < 0
        || deta % det != 0
        || detb % det != 0
      -> acc
      _, _ -> acc + { a * 3 + b * 1 }
    }
  })
}

pub fn part_2(in: String) -> Int {
  solutions(in, 10_000_000_000_000)
  |> list.fold(0, fn(acc, x) {
    let #(a, b, deta, detb, det) = x
    case a, b {
      a, b if a < 0 || b < 0 || deta % det != 0 || detb % det != 0 -> acc
      _, _ -> acc + { a * 3 + b * 1 }
    }
  })
}

fn solutions(in: String, add: Int) -> List(#(Int, Int, Int, Int, Int)) {
  let assert Ok(reg) = regex.from_string("([0-9]+)")
  string.trim(in)
  |> string.split("\n\n")
  |> list.fold([], fn(acc, x) {
    let assert [xa, ya, xb, yb, x, y] =
      regex.scan(reg, x)
      |> list.map(fn(x) { result.unwrap(int.parse(x.content), -1) })
    let x = x + add
    let y = y + add
    let det = xa * yb - ya * xb

    case det {
      0 -> acc
      _ -> {
        let deta = x * yb - y * xb
        let detb = xa * y - ya * x
        let a = deta / det
        let b = detb / det

        [#(a, b, deta, detb, det), ..acc]
      }
    }
  })
}
