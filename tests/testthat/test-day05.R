test_that("day 05", {
  example_data_05() |>
    f05a_find_lowest_location() |>
    expect_equal(35)

  example_data_05() |>
    f05b_find_lowest_location() |>
    expect_equal(46)
})
