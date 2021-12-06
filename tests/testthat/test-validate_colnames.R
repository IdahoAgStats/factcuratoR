test_that("validate_colnames() returns correct comparison", {
  df <- data.frame(year = c(2000, 1900))
  test <- validate_colnames(df, "locations")
  expect_equal(nrow(test), 5)
})

test_that("validate_colnames() returns correct comparison", {
  df <- data.frame(year = c(2000, 1900))
  test <- validate_colnames(df, "trial_data")
  expect_true(all(c("trial", "heading_date") %in% test$colname_codebook))
})
