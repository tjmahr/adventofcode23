test_that("day 03", {
  example_data_03() |>
    f03a_sum_part_numbers() |>
    expect_equal(4361)
  example_data_03() |>
    f03b_sum_gear_ratios() |>
    expect_equal(467835)
})
