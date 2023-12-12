test_that("day 12", {
  example_data_12() |>
    f12a_count_completions() |>
    expect_equal(21)

  # Correctness of each line
  example_data_12() |>
    lapply(f12a_count_completions) |>
    unlist() |>
    expect_equal(c(1, 4, 1, 1, 4, 10))
})
