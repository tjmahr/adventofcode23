test_that("utils", {
  vec_tail(1:4,  7) |> expect_equal(1:4)
  vec_tail(1:4, -1) |> expect_equal(2:4)
  vec_tail(1:4,  3) |> expect_equal(2:4)

  vec_head(1:4,  7) |> expect_equal(1:4)
  vec_head(1:4, -1) |> expect_equal(1:3)
  vec_head(1:4,  3) |> expect_equal(1:3)

})
