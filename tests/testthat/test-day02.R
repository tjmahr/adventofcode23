test_that("day 02", {
  example_data_02() |>
    f02a_sum_valid_game_ids() |>
    expect_equal(8)

  example_data_02() |>
    f02b_sum_game_powers() |>
    expect_equal(2286)
})
