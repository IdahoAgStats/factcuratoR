controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")
library(magrittr)

test_that("create.rules() creates a validator object is created",{
  test <- create.rules("trial_data", controlled_vocab_folder)
  expect_s4_class(test, "validator")

  test <- create.rules("trials_metadata", controlled_vocab_folder)
  expect_s4_class(test, "validator")

})
