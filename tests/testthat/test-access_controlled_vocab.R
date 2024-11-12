controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")


test_that("readin_db_init() reads in correct number of files",{
  test <- readin_db_init(controlled_vocab_folder)
  expect_equal(length(test), 10)

})

test_that("readin_db() reads in correct number of files",{
  test <- readin_db(controlled_vocab_folder)
  expect_equal(length(test), 11)
})



test_that("get_variety_db() returns the expected variety names", {
  test <- get_variety_db(controlled_vocab_folder, select_before = "2021-01-01")
  test2 <- test %>% dplyr::select(date_added) %>% unique(.)

  expect_equal(nrow(test2), 1)
})

test_that("get_variety_db() returns the expected crops", {
  test <- get_variety_db(controlled_vocab_folder,
                         select_before = "2021-01-01",
                         select_crops = "wheat")
  test2 <- test %>%
    dplyr::select(crop_db) %>%
    unique(.) %>%
    dplyr::mutate(test = stringr::str_detect(crop_db, "Wheat"))

  expect_equal(all(test2$test), TRUE)
})

test_that("list_db_var() correctly throws error", {
  expect_error(list_db_var(controlled_vocab_folder, "loc", crop_types = NULL),
               "'codebook_name' doesn't match a codebook name")
})

test_that("list_db_var() correctly selects crop_types", {
  test <- list_db_var( controlled_vocab_folder, "trial_data", crop_types = c("barley"))
  contains_barley_trait <- any(test$variable %in% "plump_percent_6_64")
  contains_wheat_trait <- any(test$variable %in% "falling_number")

  expect_true(contains_barley_trait & !contains_wheat_trait)
})


test_that("get_col_index() correctly throws an error", {
  test <- readin_db(controlled_vocab_folder)
  expect_error(get_col_index(test, controlled_vocab_folder,
                             "wrongname", TRUE, crop_types = NULL),
               "'codebook_name' doesn't match a codebook name")
})

test_that("get_col_index() returns integer vectors", {
  test <- readin_db(controlled_vocab_folder)
  test2 <- test[grep("^cultivar_(?!fill).*$", names(test), perl = TRUE)]
  ind <- get_col_index(test2, controlled_vocab_folder, "cultivar",
                       required_only = TRUE, crop_types = NULL)
  expect_type(ind[[1]], "integer")
})


test_that("get_col_index() returns the correct column indices", {
  test <- readin_db(controlled_vocab_folder)
  test2 <- test[grep("^cultivar_(?!fill).*$", names(test), perl = TRUE)]
  ind <- get_col_index(test2, controlled_vocab_folder, "cultivar",
                       required_only = TRUE, crop_types = NULL)
  expect_equal(ind[[1]], c(1, 2, 4, 14))
})
