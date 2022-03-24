controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")
library(magrittr)

test_that("create_rules() creates a validator object is created",{
  test <- create_rules(df_type = "trial_data",
                       db_folder = controlled_vocab_folder,
                       crop_types = NULL)
  expect_s4_class(test, "validator")

  test <- create_rules("trials_metadata", controlled_vocab_folder, crop_types = NULL)
  expect_s4_class(test, "validator")

})


test_that("create_rules() selects the correct traits",{
  test <- create_rules("trial_data", controlled_vocab_folder, crop_types = c("barley"))
  contains_barley_trait <- any(names(test) %in% "plump_percent_6_64")
  contains_wheat_trait <- any(names(test) %in% "falling_number")

  expect_true(contains_barley_trait & !contains_wheat_trait)

})
