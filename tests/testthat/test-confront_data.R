controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")
library(magrittr)

test_that("confront_data() correctly detects NA and wrong entries in date fields",{
  df <- data.frame(harvest_date = c("2020-10-30", NA, "wrong entry"))
  test <- confront_data(df, "trials_metadata", controlled_vocab_folder)
  expect_equal(as.numeric((test[["summary"]] %>% filter(name == "harvest_date") %>% select(items:nNA))), c(3,1,1,1))

  df <- data.frame(harvest_date = c("2020-10-30", NA, "wrong entry", NA, "2020-10-30"))
  test <- confront_data(df, "trials_metadata", controlled_vocab_folder)
  expect_equal(as.numeric((test[["summary"]] %>% filter(name == "harvest_date") %>% select(items:nNA))), c(5,2,1,2))
})


test_that("confront_data() correctly detects errors in other variables",{
  df <- data.frame(nursery = c("SWW", NA, "wrong entry", NA, "HWW"))
  test <- confront_data(df, "trials_metadata", controlled_vocab_folder)
  expect_equal(as.numeric((test[["summary"]] %>%
                             filter(name == "nursery") %>%
                             select(items:nNA))),
               c(5,2,1,2))
})


test_that("confront_data() creates required column for trial_data",{
  df <- data.frame(trial = c("trial1"))
  test <- confront_data(df, "trial_data", controlled_vocab_folder)
  expect_false(any(is.na(test[["summary"]]$required)))

})

test_that("confront_data() checks variety names",{
  df <- data.frame(variety = c("Bruehl", "wrong name"))
  test <- confront_data(df, "trial_data", controlled_vocab_folder)
  expect_equal(test[[1]]$passes[1], 1)

})

test_that("confront_data() checks blends",{
  df <- data.frame(variety = c("Bruehl;AP Badger",
                               "AP Badger;wrong name2",
                               "Bruehl;wrong name",
                               "wrong name;AP Badger"))
  test <- confront_data(df, "trial_data", controlled_vocab_folder, blends = TRUE)
  expect_equal(sum(test[[1]]$passes), 5)

})
