import gleam/list
import gleam/string

pub fn part_1(in: String) -> Int {
  string.trim(in)
  |> string.split("\n")
  |> list.map(string.to_graphemes)
  |> find_word("XMAS")
}

pub fn part_2(in: String) -> Int {
  string.trim(in)
  |> string.split("\n")
  |> list.map(string.to_graphemes)
  |> find_xword("MAS")
}

const dirs = [
  #(-1, -1), #(-1, 0), #(-1, 1), #(0, -1), #(0, 1), #(1, -1), #(1, 0), #(1, 1),
]

const dirs_pairs = [
  #(#(-1, -1), #(-1, 1)), #(#(-1, -1), #(1, -1)), #(#(1, 1), #(-1, 1)),
  #(#(1, 1), #(1, -1)),
]

fn find_word(letters: List(List(String)), word: String) -> Int {
  let h = list.length(letters)
  let assert [line, ..] = letters
  let w = list.length(line)
  use acc, y <- list.fold(list.range(0, h - 1), 0)
  use acc, x <- list.fold(list.range(0, w - 1), acc)
  acc + find_word_in(x, y, letters, string.to_graphemes(word), dirs)
}

fn find_xword(letters: List(List(String)), word: String) -> Int {
  let len = string.length(word)
  case string.length(word) % 2 {
    0 -> 0
    _ -> {
      // 1
      let half = { len - 1 } / 2
      let h = list.length(letters)
      let assert [line, ..] = letters
      let w = list.length(line)
      use acc, y <- list.fold(list.range(half, h - 1 - half), 0)
      use acc, x <- list.fold(list.range(half, w - 1 - half), acc)
      acc + find_x_word_in(x, y, letters, string.to_graphemes(word), dirs_pairs)
    }
  }
}

fn find_word_in(
  x: Int,
  y: Int,
  letters: List(List(String)),
  word: List(String),
  dirs: List(#(Int, Int)),
) -> Int {
  use acc, dir <- list.fold(dirs, 0)
  acc + find_word_in_loop(x, y, dir, letters, word)
}

fn find_word_in_loop(
  x: Int,
  y: Int,
  dir: #(Int, Int),
  letters: List(List(String)),
  word: List(String),
) -> Int {
  let #(dir_x, dir_y) = dir
  case word {
    [] -> 1
    _ ->
      case nth(letters, y) {
        Ok(line) ->
          case nth(line, x) {
            Ok(cur) ->
              case word {
                [first, ..rest] if first == cur ->
                  find_word_in_loop(x + dir_x, y + dir_y, dir, letters, rest)
                _ -> 0
              }
            _ -> 0
          }
        _ -> 0
      }
  }
}

fn find_x_word_in(
  x: Int,
  y: Int,
  letters: List(List(String)),
  word: List(String),
  dir_pairs: List(#(#(Int, Int), #(Int, Int))),
) -> Int {
  let half = { list.length(word) - 1 } / 2
  case
    {
      use #(#(dir0_x, dir0_y) as dir0, #(dir1_x, dir1_y) as dir1) <- list.any(
        dir_pairs,
      )
      {
        find_word_in_loop(
          x - half * dir0_x,
          y - half * dir0_y,
          dir0,
          letters,
          word,
        )
        * find_word_in_loop(
          x - half * dir1_x,
          y - half * dir1_y,
          dir1,
          letters,
          word,
        )
      }
      > 0
    }
  {
    False -> 0
    True -> 1
  }
}

fn nth(list: List(a), pos: Int) -> Result(a, Nil) {
  case list, pos {
    [x, ..], 0 -> Ok(x)
    [_, ..rest], pos if pos > 0 -> nth(rest, pos - 1)
    [_, ..], _ -> Error(Nil)
    [], _ -> Error(Nil)
  }
}
