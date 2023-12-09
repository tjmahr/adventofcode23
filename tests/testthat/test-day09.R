test_that("day 09", {
  example_data_09() |>
    f09a_oasis_forecast() |>
    expect_equal(114)

  example_data_09() |>
    f09b_oasis_backcast() |>
    expect_equal(2)
})
