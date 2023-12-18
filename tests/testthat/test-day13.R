test_that("day 13", {
  example_data_13() |>
    f13a_summarize_mirrors() |>
    expect_equal(405)

  example_data_13() |>
    f13b_summarize_adjusted_mirrors() |>
    expect_equal(400)
})
