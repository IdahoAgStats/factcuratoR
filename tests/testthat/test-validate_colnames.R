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

test_that("validate_colnames() handles valid colnames ending in _rep_xx correctly", {
  df <- data.frame(fdk_rep_01 = 10, inf_spikelet_rep_30 = 20)
  test <- validate_colnames(df, "trial_data", controlled_vocab_folder,
                            crop_types = c("wheat", "barley"))
  expect_true(all(c("fdk_rep_01", "inf_spikelet_rep_30") %in% test$colname_codebook))
})

test_that("validate_colnames() returns the correct comment if _rep_xx is used in the data", {
  df <- data.frame(fdk_rep_xx = 2)
  test <- validate_colnames(df, "trial_data", controlled_vocab_folder,
                            crop_types = c("wheat", "barley")) %>%
    filter(colname_data == "fdk_rep_xx")
  expect_true("please replace 'xx' with a valid rep number" %in% test$comment)
})

test_that("validate_colnames() does not accept numbers outside validation range for _rep_xx", {
  df <- data.frame(fdk_rep_99 = 1)
  test <- validate_colnames(df, "trial_data", controlled_vocab_folder,
                            crop_types = c("wheat", "barley"))
  expect_false("fdk_rep_99" %in% test$colname_codebook)
})
