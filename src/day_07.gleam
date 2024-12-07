import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string

@external(erlang, "math", "log10")
fn log10(x: Int) -> Float

pub fn part_1(in: String) -> Int {
  calc_calibration(in, [int.add, int.multiply])
}

pub fn part_2(in: String) -> Int {
  calc_calibration(in, [int.add, int.multiply, concat])
}

fn calc_calibration(in: String, ops: List(Op)) -> Int {
  {
    use x <- list.map(
      string.trim(in)
      |> string.split("\n"),
    )
    let assert [res, vals] = string.split(x, ": ")
    #(
      result.unwrap(int.parse(res), -1),
      string.split(vals, " ")
        |> list.map(fn(x) { result.unwrap(int.parse(x), -1) }),
    )
  }
  |> list.filter(fn(x) { valid_expression(x, ops) })
  |> list.map(fn(x) {
    let #(res, _) = x
    res
  })
  |> int.sum()
}

type Expression =
  #(Int, List(Int))

type Op =
  fn(Int, Int) -> Int

fn valid_expression(expression: Expression, operations: List(Op)) -> Bool {
  let assert #(result, [first, ..operands]) = expression
  valid_expression_loop(operands, result, operations, first)
}

fn valid_expression_loop(
  operands: List(Int),
  result: Int,
  operations: List(Op),
  acc: Int,
) -> Bool {
  case operands, acc {
    [], acc if acc == result -> True
    [], _ -> False
    [_, ..], acc if acc > result -> False
    [operand, ..rest], acc -> {
      use op <- list.any(operations, _)
      valid_expression_loop(rest, result, operations, op(acc, operand))
    }
  }
}

fn concat(x, y) -> Int {
  x
  * {
    case
      log10(y + 1)
      |> float.ceiling()
      |> int.power(10, _)
    {
      Ok(a) -> float.truncate(a)
      _ -> panic
    }
  }
  + y
}
