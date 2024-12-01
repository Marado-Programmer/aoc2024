import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn part_1(in: String) -> Int {
  string.trim(in)
  |> string.split("\n")
  |> list.map(fn(x: String) -> #(Int, Int) {
    let assert [first, last] = string.split(x, "   ")
    #(result.unwrap(int.parse(first), -1), result.unwrap(int.parse(last), -1))
  })
  |> firsts_and_lasts
  |> fn(x: #(List(Int), List(Int))) -> #(List(Int), List(Int)) {
    let #(firsts, lasts) = x
    #(list.sort(firsts, int.compare), list.sort(lasts, int.compare))
  }
  |> calc_distances
  |> int.sum
}

pub fn part_2(in: String) -> Int {
  string.trim(in)
  |> string.split("\n")
  |> list.map(fn(x: String) -> #(Int, Int) {
    let assert [first, last] = string.split(x, "   ")
    #(result.unwrap(int.parse(first), -1), result.unwrap(int.parse(last), -1))
  })
  |> firsts_and_lasts
  |> get_count()
  |> multiply
  |> int.sum
}

fn firsts_and_lasts(list: List(#(Int, Int))) -> #(List(Int), List(Int)) {
  let firsts =
    list.map(list, fn(x: #(Int, Int)) -> Int {
      let #(first, _) = x
      first
    })
  let lasts =
    list.map(list, fn(x: #(Int, Int)) -> Int {
      let #(_, last) = x
      last
    })
  #(firsts, lasts)
}

fn calc_distances(pairs: #(List(Int), List(Int))) -> List(Int) {
  let #(firsts, lasts) = pairs
  distance(firsts, lasts, [])
}

fn distance(firsts: List(Int), lasts: List(Int), acc: List(Int)) -> List(Int) {
  case firsts, lasts {
    [x, ..rest_x], [y, ..rest_y] ->
      distance(rest_x, rest_y, [int.absolute_value(x - y), ..acc])
    [_, ..], [] -> panic
    [], [_, ..] -> panic
    [], [] -> acc
  }
}

fn get_count(pairs: #(List(Int), List(Int))) -> #(List(Int), List(Int)) {
  let #(firsts, lasts) = pairs
  #(firsts, list.map(firsts, fn(x) { list.count(lasts, fn(y) { x == y }) }))
}

fn multiply(pairs: #(List(Int), List(Int))) -> List(Int) {
  let #(firsts, lasts) = pairs
  multiply_loop(firsts, lasts, [])
}

fn multiply_loop(
  firsts: List(Int),
  lasts: List(Int),
  acc: List(Int),
) -> List(Int) {
  case firsts, lasts {
    [x, ..rest_x], [y, ..rest_y] ->
      multiply_loop(rest_x, rest_y, [x * y, ..acc])
    [_, ..], [] -> panic
    [], [_, ..] -> panic
    [], [] -> acc
  }
}
