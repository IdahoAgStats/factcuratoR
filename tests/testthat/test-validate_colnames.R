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


test_that("validate_colnames() removes variables from another codebook", {
  df <- data.frame(year = c(2000, 1900), trial = c("a", "b"), moisture = c(1,2))
  test <- validate_colnames(df,
                            codebook_name = "trial_data",
                            controlled_vocab_folder,
                            cb_name_remove = "trials_metadata")
  expect_false("year" %in% test$colname_data )
  expect_true("trial" %in% test$colname_data )
})
