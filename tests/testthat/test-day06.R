test_that("day 06", {
  example_data_06() |>
    f06a_multiply_winning_distances() |>
    expect_equal(288)

  example_data_06() |>
    f06b_multiply_winning_distances() |>
    expect_equal(71503)
})
