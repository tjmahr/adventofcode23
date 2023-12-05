test_that("day 04", {
  example_data_04() |>
    f04a_score_cards() |>
    expect_equal(13)

  example_data_04() |>
    f04b_count_cards() |>
    expect_equal(30)
})
