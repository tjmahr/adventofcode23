#' Day 14: Parabolic Reflector Dish
#'
#' [Parabolic Reflector Dish](https://adventofcode.com/2023/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' You reach the place where all of the mirrors were pointing: a massive
#' [parabolic reflector
#' dish](https://en.wikipedia.org/wiki/Parabolic_reflector){target="_blank"}
#' [attached]{title="Why, where do you attach YOUR massive parabolic reflector dishes?"}
#' to the side of another large mountain.
#'
#' The dish is made up of many small mirrors, but while the mirrors
#' themselves are roughly in the shape of a parabolic reflector dish, each
#' individual mirror seems to be pointing in slightly the wrong direction.
#' If the dish is meant to focus light, all it\'s doing right now is
#' sending it in a vague direction.
#'
#' This system must be what provides the energy for the lava! If you focus
#' the reflector dish, maybe you can go where it\'s pointing and use the
#' light to fix the lava production.
#'
#' Upon closer inspection, the individual mirrors each appear to be
#' connected via an elaborate system of ropes and pulleys to a large metal
#' platform below the dish. The platform is covered in large rocks of
#' various shapes. Depending on their position, the weight of the rocks
#' deforms the platform, and the shape of the platform controls which ropes
#' move and ultimately the focus of the dish.
#'
#' In short: if you move the rocks, you can focus the dish. The platform
#' even has a control panel on the side that lets you *tilt* it in one of
#' four directions! The rounded rocks (`O`) will roll when the platform is
#' tilted, while the cube-shaped rocks (`#`) will stay in place. You note
#' the positions of all of the empty spaces (`.`) and rocks (your puzzle
#' input). For example:
#'
#'     O....#....
#'     O.OO#....#
#'     .....##...
#'     OO.#O....O
#'     .O.....O#.
#'     O.#..O.#.#
#'     ..O..#O..O
#'     .......O..
#'     #....###..
#'     #OO..#....
#'
#' Start by tilting the lever so all of the rocks will slide *north* as far
#' as they will go:
#'
#'     OOOO.#.O..
#'     OO..#....#
#'     OO..O##..O
#'     O..#.OO...
#'     ........#.
#'     ..#....#.#
#'     ..O..#.O.O
#'     ..O.......
#'     #....###..
#'     #....#....
#'
#' You notice that the support beams along the north side of the platform
#' are *damaged*; to ensure the platform doesn\'t collapse, you should
#' calculate the *total load* on the north support beams.
#'
#' The amount of load caused by a single rounded rock (`O`) is equal to the
#' number of rows from the rock to the south edge of the platform,
#' including the row the rock is on. (Cube-shaped rocks (`#`) don\'t
#' contribute to load.) So, the amount of load caused by each rock in each
#' row is as follows:
#'
#'     OOOO.#.O.. 10
#'     OO..#....#  9
#'     OO..O##..O  8
#'     O..#.OO...  7
#'     ........#.  6
#'     ..#....#.#  5
#'     ..O..#.O.O  4
#'     ..O.......  3
#'     #....###..  2
#'     #....#....  1
#'
#' The total load is the sum of the load caused by all of the *rounded
#' rocks*. In this example, the total load is *`136`*.
#'
#' Tilt the platform so that the rounded rocks all roll north. Afterward,
#' *what is the total load on the north support beams?*
#'
#' **Part Two**
#'
#' The parabolic reflector dish deforms, but not in a way that focuses the
#' beam. To do that, you\'ll need to move the rocks to the edges of the
#' platform. Fortunately, a button on the side of the control panel labeled
#' \"*spin cycle*\" attempts to do just that!
#'
#' Each *cycle* tilts the platform four times so that the rounded rocks
#' roll *north*, then *west*, then *south*, then *east*. After each tilt,
#' the rounded rocks roll as far as they can before the platform tilts in
#' the next direction. After one cycle, the platform will have finished
#' rolling the rounded rocks in those four directions in that order.
#'
#' Here\'s what happens in the example above after each of the first few
#' cycles:
#'
#'     After 1 cycle:
#'     .....#....
#'     ....#...O#
#'     ...OO##...
#'     .OO#......
#'     .....OOO#.
#'     .O#...O#.#
#'     ....O#....
#'     ......OOOO
#'     #...O###..
#'     #..OO#....
#'
#'     After 2 cycles:
#'     .....#....
#'     ....#...O#
#'     .....##...
#'     ..O#......
#'     .....OOO#.
#'     .O#...O#.#
#'     ....O#...O
#'     .......OOO
#'     #..OO###..
#'     #.OOO#...O
#'
#'     After 3 cycles:
#'     .....#....
#'     ....#...O#
#'     .....##...
#'     ..O#......
#'     .....OOO#.
#'     .O#...O#.#
#'     ....O#...O
#'     .......OOO
#'     #...O###.O
#'     #.OOO#...O
#'
#' This process should work if you leave it running long enough, but
#' you\'re still worried about the north support beams. To make sure
#' they\'ll survive for a while, you need to calculate the *total load* on
#' the north support beams after `1000000000` cycles.
#'
#' In the above example, after `1000000000` cycles, the total load on the
#' north support beams is *`64`*.
#'
#' Run the spin cycle for `1000000000` cycles. Afterward, *what is the
#' total load on the north support beams?*
#'
#' @param x some data
#' @return For Part One, `f14a_find_total_load(x)` returns the total load on the
#'   northern beams. For Part Two, `f14b_find_load_after_many_cycles(x)` returns ....
#' @export
#' @examples
#' f14a_find_total_load(example_data_14())
#' f14b_find_load_after_many_cycles(example_data_14())
f14a_find_total_load <- function(x) {
  m <- f14_helper(x)
  m |>
    f14_helper_roll_rocks("N") |>
    apply(2, rev) |>
    apply(2, vec_which_value, "O") |>
    unlist() |>
    sum()
}


#' @rdname day14
#' @export
f14b_find_load_after_many_cycles <- function(x) {
  do_cycle <- function(m) {
    m |>
      f14_helper_roll_rocks("N") |>
      f14_helper_roll_rocks("W") |>
      f14_helper_roll_rocks("S") |>
      f14_helper_roll_rocks("E")
  }
  compute_load <- function(m) {
    m |>
      apply(2, rev) |>
      apply(2, vec_which_value, "O") |>
      unlist() |>
      sum()
  }
  as_string <- function(m) paste0(m, collapse = "")

  m <- f14_helper(x)
  cycle_history <- character(0)
  load_history <- numeric()

  while (!anyDuplicated(cycle_history)) {
    m <- do_cycle(m)
    cycle_history <- c(cycle_history, as_string(m))
    load_history <- c(load_history, compute_load(m))
  }

  first_dupe <- anyDuplicated(cycle_history)
  cycle_start <- cycle_history |>
    vec_which_value(cycle_history[first_dupe]) |>
    vec_head(1)
  cycle_end <- first_dupe - 1
  cycle_length <- length(cycle_start:cycle_end)

  target <- 1000000000
  k <- target - cycle_start
  check <- cycle_start + (k %% cycle_length)
  load_history[check]

}


f14_helper_roll_rocks <- function(x, dir) {
  sort_rocks <- function(xs, side = "left") {
    i <- seq_along(xs)

    if (side == "right") {
      # Want sequences of #,x,x
      runs <- cumsum(xs == "#")
    } else {
      # Want sequences of x,x,#

      # split() sorts the sublists so make sure they have
      # an increasing order with * -1
      runs <- rev(cumsum(rev(xs) == "#")) * -1
    }

    xs |>
      split(runs) |>
      lapply(sort, decreasing = side == "left") |>
      unlist(use.names = FALSE)
  }

  axis <- c("N" = 2, "S" = 2, "E" = 1, "W" = 1)
  side <- c("N" = "left", "S" = "right", "E" = "right", "W" = "left")
  f <- if(axis[dir] == 1) t else I
  x |>
    apply(axis[dir], sort_rocks, side = side[dir]) |>
    f()
}

f14_helper <- function(x) {
  x |>
    strsplit("") |>
    list_invoke(rbind)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = c(
      "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#...."
    )
  )
  l[[example]]
}
