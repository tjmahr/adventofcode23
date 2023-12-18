test_that("day 15", {
  example_data_15() |>
    f15a_sum_hashes() |>
    expect_equal(1320)

  example_data_15() |>
    f15b_compute_focusing_power() |>
    expect_equal(145)
})
