test_that("do_exactmatch() returns the correct groups",{
  var_names <- c("test VAR 001", "test Var 2 (testvar2/ tv00002)")

  df1 <- tibble(variety = c(var_names, var_names[2]),
                intid = c("testvar001", "testvar2", "tv00002"),
                var_id = c(1,2,2))

  result <-
    do_exactmatch(
      db_folder = testthat::test_path("test_controlled_vocab"),
      df1
    )

  ans1 <- df1 %>%
            mutate(
              db_id = as.integer(NA),
              crop_db = as.character(NA),
              crop_type_db = as.character(NA),
              date_added = parse_date_time(NA, "ymd"),
              type_db = as.character(NA),
              variety_db = as.character(NA),
              intid_db =  c("testvar001", "testvar2", "tv00002"),
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

test_that("do_exactmatch() returns the correct groups for blends",{
  blends_df <- tibble(variety = rep(c("a/b","c/d"), each = 2),
                      intid = c("a", "b", "c", "d"),
                      var_id = c(1,1,2,2),
                      type_db = rep("variety", 4),
                      type = "blends")
  result <-
    do_exactmatch(
      db_folder = testthat::test_path("test_controlled_vocab"),
      blends_df,
      is_blends = FALSE
    )

  expect_equal(nrow(result[[2]]), 4)
})

test_that("do_exactmatch() returns the expected column names",{
  var_names <- c("Bruehl")

  df1 <- tibble(variety = var_names) %>%
    mutate(intid = tolower(variety),
           var_id = 1)

  result <-
    do_exactmatch(
      db_folder = testthat::test_path("test_controlled_vocab"),
      df1
    )

  expect_equal(nrow(result[[1]]), 1)
  expect_true("intid_db" %in% names(result[[1]]))
})

test_that("do_exactmatch() with rename_df = TRUE returns the expected column names",{
  var_names <- c("wb1035cl")

  df1 <- tibble(variety = var_names) %>%
    mutate(intid = tolower(variety),
           var_id = 1)

  result <-
    do_exactmatch(
      db_folder = testthat::test_path("test_controlled_vocab"),
      data_intid = df1,
      rename_df = TRUE
    )

  expect_equal(nrow(result[[1]]), 1)
  expect_true("intid_db" %in% names(result[[1]]))
})

