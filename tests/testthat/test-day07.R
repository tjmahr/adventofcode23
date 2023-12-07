test_that("day 07", {
  data <- example_data_07() |> f07_helper()
  expect_equal(data$rank, c(1, 4, 3, 2, 5))
  expect_equal(data$jk_rank, c(1, 3, 2, 5, 4))

  example_data_07() |>
    f07a_calculate_winnings() |>
    expect_equal(6440)

  example_data_07() |>
    f07b_calculate_jk_winnings() |>
    expect_equal(5905)
})

