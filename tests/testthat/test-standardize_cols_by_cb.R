#library(magrittr)
controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")

test_that("standardize_cols_by_cb() adds correct columns", {
  df <- data.frame(a = NA, company = NA)
  result <- standardize_cols_by_cb(df, codebook_name = "grower_cooperator",
                                   add_missing_cols = TRUE,
                                   required_only = FALSE,
                                   codebook_cols_only = FALSE,
                                   db_folder = controlled_vocab_folder)
  expect_equal(names(result), c("grower_cooperator", "type", "company", "a"))
})

test_that("standardize_cols_by_cb() handles 'required_only' = TRUE", {
  df <- data.frame(company = NA, a = NA)
  result <- standardize_cols_by_cb(df, codebook_name = "grower_cooperator",
                                   add_missing_cols = TRUE,
                                   required_only = TRUE,
                                   codebook_cols_only = FALSE,
                                   db_folder = controlled_vocab_folder)
  expect_equal(names(result), c("grower_cooperator",  "company", "a"))
})

test_that("standardize_cols_by_cb() handles 'codebook_cols_only' = TRUE", {
  df <- data.frame(company = NA, a = NA)
  result <- standardize_cols_by_cb(df, codebook_name = "grower_cooperator",
                                   add_missing_cols = TRUE,
                                   required_only = FALSE,
                                   codebook_cols_only = TRUE,
                                   db_folder = controlled_vocab_folder)
  expect_equal(names(result), c("grower_cooperator", "type", "company"))
})

test_that("standardize_cols_by_cb() handles 'required_only' and codebook_cols_only' = TRUE", {
  df <- data.frame(company = NA, a = NA)
  result <- standardize_cols_by_cb(df, codebook_name = "grower_cooperator",
                                   add_missing_cols = TRUE,
                                   required_only = TRUE,
                                   codebook_cols_only = TRUE,
                                   db_folder = controlled_vocab_folder)
  expect_equal(names(result), c("grower_cooperator", "company"))
})


test_that("standardize_cols_by_cb() handles 'add_missing_cols' = FALSE", {
  df <- data.frame(company = NA, a = NA)
  result <- standardize_cols_by_cb(df, codebook_name = "grower_cooperator",
                                   add_missing_cols = FALSE,
                                   codebook_cols_only = TRUE,
                                   db_folder = controlled_vocab_folder)
  expect_equal(names(result), c("company"))
})


test_that("standardize_cols_by_cb() handles 'add_missing_cols' = FALSE", {
  df <- data.frame(a = NA, company = NA)
  result <- standardize_cols_by_cb(df, codebook_name = "grower_cooperator",
                                   add_missing_cols = FALSE,
                                   codebook_cols_only = FALSE,
                                   db_folder = controlled_vocab_folder)
  expect_equal(names(result), c("company", "a"))
})

