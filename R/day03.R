#' Day 03: Gear Ratios
#'
#' [Gear Ratios](https://adventofcode.com/2023/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' You and the Elf eventually reach a [gondola
#' lift](https://en.wikipedia.org/wiki/Gondola_lift){target="_blank"}
#' station; he says the gondola lift will take you up to the *water
#' source*, but this is as far as he can bring you. You go inside.
#'
#' It doesn\'t take long to find the gondolas, but there seems to be a
#' problem: they\'re not moving.
#'
#' \"Aaah!\"
#'
#' You turn around to see a slightly-greasy Elf with a wrench and a look of
#' surprise. \"Sorry, I wasn\'t expecting anyone! The gondola lift isn\'t
#' working right now; it\'ll still be a while before I can fix it.\" You
#' offer to help.
#'
#' The engineer explains that an engine part seems to be missing from the
#' engine, but nobody can figure out which one. If you can *add up all the
#' part numbers* in the engine schematic, it should be easy to work out
#' which part is missing.
#'
#' The engine schematic (your puzzle input) consists of a visual
#' representation of the engine. There are lots of numbers and symbols you
#' don\'t really understand, but apparently *any number adjacent to a
#' symbol*, even diagonally, is a \"part number\" and should be included in
#' your sum. (Periods (`.`) do not count as a symbol.)
#'
#' Here is an example engine schematic:
#'
#'     467..114..
#'     ...*......
#'     ..35..633.
#'     ......#...
#'     617*......
#'     .....+.58.
#'     ..592.....
#'     ......755.
#'     ...$.*....
#'     .664.598..
#'
#' In this schematic, two numbers are *not* part numbers because they are
#' not adjacent to a symbol: `114` (top right) and `58` (middle right).
#' Every other number is adjacent to a symbol and so *is* a part number;
#' their sum is *`4361`*.
#'
#' Of course, the actual engine schematic is much larger. *What is the sum
#' of all of the part numbers in the engine schematic?*
#'
#' **Part Two**
#'
#' The engineer finds the missing part and installs it in the engine! As
#' the engine springs to life, you jump in the closest gondola, finally
#' ready to ascend to the water source.
#'
#' You don\'t seem to be going very fast, though. Maybe something is still
#' wrong? Fortunately, the gondola has a phone labeled \"help\", so you
#' pick it up and the engineer answers.
#'
#' Before you can explain the situation, she suggests that you look out the
#' window. There stands the engineer, holding a phone in one hand and
#' waving with the other. You\'re going so slowly that you haven\'t even
#' left the station. You exit the gondola.
#'
#' The missing part wasn\'t the only issue - one of the gears in the engine
#' is wrong. A *gear* is any `*` symbol that is adjacent to *exactly two
#' part numbers*. Its *gear ratio* is the result of
#' [multiplying]{title="They're magic gears."} those two numbers together.
#'
#' This time, you need to find the gear ratio of every gear and add them
#' all up so that the engineer can figure out which gear needs to be
#' replaced.
#'
#' Consider the same engine schematic again:
#'
#'     467..114..
#'     ...*......
#'     ..35..633.
#'     ......#...
#'     617*......
#'     .....+.58.
#'     ..592.....
#'     ......755.
#'     ...$.*....
#'     .664.598..
#'
#' In this schematic, there are *two* gears. The first is in the top left;
#' it has part numbers `467` and `35`, so its gear ratio is `16345`. The
#' second gear is in the lower right; its gear ratio is `451490`. (The `*`
#' adjacent to `617` is *not* a gear because it is only adjacent to one
#' part number.) Adding up all of the gear ratios produces *`467835`*.
#'
#' *What is the sum of all of the gear ratios in your engine schematic?*
#'
#' @param x some data
#' @return For Part One, `f03a_sum_part_numbers(x)` returns the sum of part
#'   numbers. For Part Two, `f03b_sum_gear_ratios(x)` returns the sum of gear
#'   ratios.
#' @export
#' @examples
#' f03a_sum_part_numbers(example_data_03())
#' f03b_sum_gear_ratios(example_data_03())
f03a_sum_part_numbers <- function(x) {
  d <- f03_helper(x)
  d[c("id", "number", "number_id")] |>
    unique() |>
    vec_element("number") |>
    as.numeric() |>
    sum()
}


#' @rdname day03
#' @export
f03b_sum_gear_ratios <- function(x) {
  d <- f03_helper(x)
  d |>
    split(~ id) |>
    list_filter(function(x) all(x$symbol == "*")) |>
    list_filter(function(x) length(unique(x$number_id)) == 2) |>
    lapply(function(x) unique(x[c("number", "number_id")])) |>
    lapply(function(x) x$number |> as.numeric() |> prod()) |>
    unlist() |>
    sum()
}


f03_helper <- function(x) {
  rle_seq <- function(x) {
    r <- rle(x)
    r$input <- x
    r$ends <- cumsum(r$lengths)
    r$starts <- (r$ends - r$lengths) + 1
    r$seq <- list_map2(r$starts, r$ends, seq)
    class(r) <- "list"
    r
  }

  create_number_dataframe <- function(row) {
    chars <- row |> strsplit("") |> unlist()
    r <- grepl("\\d", chars) |> rle_seq()

    r$seq |>
      vec_index(which(r$values)) |>
      lapply(function(x) {
        data.frame(col = x, number = paste0(chars[x], collapse = ""))
      }) |>
      list_map2(
        r$values |> which() |> seq_along(),
        df_add_column,
        name = "number_id"
      ) |>
      list_invoke(rbind)
  }

  create_symbol_dataframe <- function(x) {
    chars <- x |> strsplit("") |> unlist()
    col <- grep("[^0-9.]", chars)
    data.frame(col = col, symbol = chars[col])
  }

  data_numbers <- x |>
    lapply(create_number_dataframe) |>
    list_map2(seq_along(x), df_add_column, name = "row") |>
    list_filter(is.data.frame) |>
    list_invoke(rbind)
  data_numbers$number_id <- paste0(data_numbers$row, "_", data_numbers$number_id)

  data_symbols <- x |>
    lapply(create_symbol_dataframe) |>
    list_map2(seq_along(x), df_add_column, name = "row") |>
    list_invoke(rbind) |>
    df_add_rowid(name = "id")

  mask <- data.frame(
    row = c(-1, 0, 1, -1, 0, 1, -1, 0, 1),
    col = c(-1, -1, -1, 0, 0, 0, 1, 1, 1)
  )

  data_to_check <- data_symbols |>
    split(~id) |>
    lapply(
      function(x) {
        m <- mask
        m$row <- m$row + x$row
        m$col <- m$col + x$col
        m$symbol <- x$symbol
        m$id <- x$id
        m
      }
    ) |>
    list_invoke(rbind)

  merge(
    data_to_check,
    data_numbers
  )
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  l <- list(
    a = c(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )
  )
  l[[example]]
}
