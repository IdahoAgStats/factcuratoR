test_that("find_col_info() returns expected output", {
  cols <-  c("a","b", "c")
  df <- data.frame(year = c(91, 92,92),
                   a = 1:3,
                   b = c(-9, -9, 2),
                   c = c(NA, "a", "b") )

  ans <- data.frame(n = c(3, 1, 2),
                    contained_in = c("91;92", "92", "92"),
                    variable = cols,
                    example = c("1", "2", "a"))

  test <- find_col_info(df, cols, year)
  expect_equal(test, ans)

})
