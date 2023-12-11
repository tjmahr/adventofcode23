test_that("day 11", {
  example_data_11() |>
    f11a_sum_galaxy_distances() |>
    expect_equal(374)

  example_data_11() |>
    f11a_sum_galaxy_distances(10) |>
    expect_equal(1030)

  example_data_11() |>
    f11a_sum_galaxy_distances(100) |>
    expect_equal(8410)
})
