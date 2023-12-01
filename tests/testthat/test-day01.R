test_that("day 01", {
  expect_equal(
    example_data_01() |> f01a_sum_hidden_numbers(),
    142
  )
  expect_equal(
    example_data_01(2) |> f01b_decode_and_sum_hidden_numbers(),
    281
  )
})
