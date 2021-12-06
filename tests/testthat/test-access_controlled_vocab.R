test_that("get.variety_db returns the expected variety names", {
  test <- get.variety_db(select_before = "2021-01-01")
  test2 <- test %>% select(date_added) %>% unique(.)

  expect_equal(nrow(test2), 1)
})

test_that("get.variety_db returns the expect crops", {
  test <- get.variety_db(select_crops = "wheat")
  test2 <- test %>% select(crop_db) %>% unique(.) %>%
    mutate(test = str_detect(crop_db, "Wheat"))

  expect_equal(all(test2$test), TRUE)
})

test_that("list.db_var() correctly throws error", {
  expect_error(list.db_var("loc"), "'codebook_name' doesn't match a codebook name")
})
