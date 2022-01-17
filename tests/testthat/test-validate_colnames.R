controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")

test_that("validate_colnames() returns correct comparison", {
  df <- data.frame(year = c(2000, 1900))
  test <- validate_colnames(df, "locations", controlled_vocab_folder)
  expect_equal(nrow(test), 5)
})

test_that("validate_colnames() returns correct comparison", {
  df <- data.frame(state = NA, loca = NA)
  test <- validate_colnames(df, "locations", controlled_vocab_folder)
  expect_equal(nrow(test), 5)
})

test_that("validate_colnames() returns correct comparison", {
  df <- data.frame(year = c(2000, 1900))
  test <- validate_colnames(df, "trial_data", controlled_vocab_folder)
  expect_true(all(c("trial", "heading_date") %in% test$colname_codebook))
})
