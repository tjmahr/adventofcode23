test_that("day 10", {
  example_data_10() |>
    f10a_find_farthest_distance() |>
    expect_equal(4)

  example_data_10(2) |>
    f10a_find_farthest_distance() |>
    expect_equal(8)
})
