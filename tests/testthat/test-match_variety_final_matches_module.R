outputfolder_test <- testthat::test_path("test_match_variety_files",
                                         "output")

test_that("collect_final_matches() returns correct output", {
  # Create test data in the form that collect_final_matches() expects
  names <-
    data.frame(
      variety = c("name1", "name2", "name3", "name4"),
      var_id = 1:4,
      type = NA,
      crop_type = NA) %>%
    mutate(intid = variety)

  df1 <- names %>% filter(var_id <= 2) %>%
    mutate(variety_db = variety, intid_db = variety, type_db = NA)
  df2 <- names %>% filter(var_id == 3) %>%
    mutate(variety_db = variety, intid_db = variety, type_db = NA)

  df_blank <- data.frame(a = NA, b = NA)

  ls1 <- list(match = df1, nomatch = df_blank)
  ls2 <- list(match = df2, nomatch = df_blank)

  test <-
    collect_final_matches(list(exact = ls1,
                               rename = ls2), names, outputfolder_test)

  expect_equal(test$match_step, c("exact", "exact", "rename", NA))
})



test_that("collect_final_matches() returns correct output for blends", {
  # Create test data in the form that collect_final_matches() expects
  names <-
    data.frame(
      variety = rep(c("name1/name2", "name3/name4"), each = 2),
      var_id = c(1,1,2,2),
      type = "blends",
      crop_type = NA) %>%
    mutate(intid = c("name1", "name2", "name3", "name4"))

  df1 <- names %>%
    mutate(variety_db = intid, intid_db = intid, type_db = NA)

  df_blank <- data.frame(a = NA, b = NA)

  ls1 <- list(match = df1, nomatch = df_blank)

  test <-
    collect_final_matches(list(exact = ls1), names, outputfolder_test, is_blends = TRUE)

  expect_equal(nrow(test), 2)
})
