test_that("do_exactmatch() returns the correct groups",{
  df1 <- tibble(variety = c(var_names, var_names[2]),
                intid = c("testvar001", "testvar2", "tv00002"),
                var_id = c(1,2,2))

  result <-
    do_exactmatch(
      db_folder = testthat::test_path("test_controlled_vocab"),
      df1
    )

  ans1 <- tibble(variety = c(var_names, var_names[2]),
                 intid = c("testvar001", "testvar2", "tv00002"),
                 var_id = c(1,2,2),
                 db_id = as.integer(NA),
                 crop_db = as.character(NA),
                 crop_type_db = as.character(NA),
                 date_added = parse_date_time(NA, "ymd"),
                 type_db = as.character(NA),
                 variety_db = as.character(NA),
                 is_truematch = FALSE,
                 any_match = FALSE) %>%
    mutate(var_id = as.character(var_id))
  emptytibble <- ans1 %>% filter(variety == "a") %>% rename(date_added_db = date_added)
  ans <- list(match = emptytibble ,
              nomatch = ans1 %>% select(c(variety, intid, var_id)),
              check = emptytibble,
              not_needed = emptytibble)



  expect_equal(result, ans)
})
