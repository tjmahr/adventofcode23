test_that("day 14", {
  c1 <- f14_helper(c(
    ".....#....",
    "....#...O#",
    "...OO##...",
    ".OO#......",
    ".....OOO#.",
    ".O#...O#.#",
    "....O#....",
    "......OOOO",
    "#...O###..",
    "#..OO#...."
  ))
  c2 <- f14_helper(c(
    ".....#....",
    "....#...O#",
    ".....##...",
    "..O#......",
    ".....OOO#.",
    ".O#...O#.#",
    "....O#...O",
    ".......OOO",
    "#..OO###..",
    "#.OOO#...O"
  ))
  c3 <- f14_helper(c(
    ".....#....",
    "....#...O#",
    ".....##...",
    "..O#......",
    ".....OOO#.",
    ".O#...O#.#",
    "....O#...O",
    ".......OOO",
    "#...O###.O",
    "#.OOO#...O"
  ))

  example_data_14() |>
    f14a_find_total_load() |>
    expect_equal(136)

  example_data_14() |>
    f14_helper() |>
    f14_helper_roll_rocks("N") |>
    f14_helper_roll_rocks("W") |>
    f14_helper_roll_rocks("S") |>
    f14_helper_roll_rocks("E") |>
    expect_equal(c1) |>
    f14_helper_roll_rocks("N") |>
    f14_helper_roll_rocks("W") |>
    f14_helper_roll_rocks("S") |>
    f14_helper_roll_rocks("E") |>
    expect_equal(c2) |>
    f14_helper_roll_rocks("N") |>
    f14_helper_roll_rocks("W") |>
    f14_helper_roll_rocks("S") |>
    f14_helper_roll_rocks("E") |>
    expect_equal(c3)

  example_data_14() |>
    f14b_find_load_after_many_cycles() |>
    expect_equal(64)
})
