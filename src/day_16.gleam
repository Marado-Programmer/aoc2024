import gleam/set
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/queue
import gleam/string
import utils/matrix
import utils/utils

pub fn part_1(in: String) -> Int {
  let map =
    string.trim(in)
    |> matrix.create_matrix_from_string()
  let assert option.Some(init) =
    matrix.matrix_first_index(map, fn(x) { x == "S" })
  let assert option.Some(end) =
    matrix.matrix_first_index(map, fn(x) { x == "E" })
  walk(map, init, end, #(1, 0))
}

pub fn part_2(in: String) -> Int {
  part_1(in)
}

type Map =
  matrix.Matrix2(String)

type MapT =
  matrix.Matrix2(#(String, Bool))

fn walk(
  map: Map,
  pos: matrix.Point2,
  end: matrix.Point2,
  dir: matrix.Point2,
) -> Int {
  let assert option.Some(solution) =
    walk_loop(
      map
        |> matrix.matrix_map(fn(x) { #(x, False) })
        |> matrix.matrix_change(pos, #("S", True)),
      end,
      queue.new() |> queue.push_back(#(pos, dir, 0)),
      set.new(),
      option.None,
    )
  solution
}

fn walk_loop(
  map: MapT,
  end: matrix.Point2,
  nexts: queue.Queue(#(matrix.Point2, matrix.Point2, Int)),
  rotated: set.Set(matrix.Point2),
  solution: option.Option(Int),
) -> option.Option(Int) {
  case
    nexts
    |> queue.to_list
    |> list.sort(fn(x, y) {
      let #(_, _, x) = x
      let #(_, _, y) = y
      int.compare(x, y)
    })
    |> queue.from_list()
    |> queue.pop_back()
  {
    Error(_) -> solution
    Ok(#(pos, rest)) ->
      case pos {
        #(pos, _, points) if pos == end -> {
          let goal = case solution {
            option.None -> points
            option.Some(prev) if prev <= points -> prev
            _ -> points
          }
          walk_loop(
            map,
            end,
            rest
              |> queue.to_list()
              |> list.filter(fn(x) {
                let #(_, _, x) = x
                x < goal
              })
              |> queue.from_list(),
            rotated,
            option.Some(goal),
          )
        }
        #(pos, dir, points) -> {
          let #(x, y) = pos
          let #(dirx, diry) = dir
          let new_pos = #(x + dirx, y + diry)
          let rest = case
            rotated |> set.contains(pos)
          {
            True -> rest
            _ -> {
              let rest = case solution {
                option.Some(x) if x <= points + 1000 -> rest
                _ ->
                  rest
                  |> queue.push_back(#(pos, #(diry, dirx), points + 1000))
                  |> queue.push_back(#(pos, #(-diry, -dirx), points + 1000))
              }
              case solution {
                option.Some(x) if x <= points + 2000 -> rest
                _ ->
                  rest
                  |> queue.push_back(#(pos, #(-dirx, -diry), points + 2000))
              }
            }
          }
          let #(rest, map) = case
            matrix.in_matrix_or(map, new_pos, utils.return, #("#", True))
          {
            #("#", _) -> #(rest, map)
            #(_, True) -> #(rest, map)
            _ -> {
              case solution {
                option.Some(x) if x <= points + 1 -> #(rest, map)
                _ -> #(
                  rest
                    |> queue.push_back(#(new_pos, dir, points + 1)),
                  map
                    |> matrix.matrix_change(new_pos, case
                      matrix.in_matrix_or(map, pos, utils.return, #("#", True))
                    {
                      #(x, _) -> #(x, True)
                    }),
                )
              }
            }
          }
          matrix.matrix_map(map, fn (x) {
            case x {
                #("#", _) -> "#"
                #(_, True) -> ";"
                #(_, False) -> " "
            }
          }) |> matrix.to_string() |> io.println()
          walk_loop(map, end, rest, rotated |> set.insert(pos), solution)
        }
      }
  }
}
