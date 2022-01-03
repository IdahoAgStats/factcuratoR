# Objects that are used in multiple tests
var_names <- c("test VAR 001", "test Var 2 (testvar2/ tv00002)")
#var <- read_csv(here("tests", "test_example_files", "example_cultivar_matching.csv"))

var <- testthat::test_path("test_example_files",
                                               "example_cultivar_matching.csv")
#library(magrittr)

df1_mult_tests <- tibble(variety = c(var_names, var_names[2]),
                         intid = c("testvar001", "testvar2", "tv00002"),
                         var_id = c(1,2,2),
                         variety_db = c("testvar001", NA, "tv00002"))

df2_mult_tests <- tibble(variety = c(var_names[2]),
                         intid = c("testvar2", "tv00002"),
                         var_id = c(2,2),
                         variety_db = c("testvar", "tv00002"))

##


#' This is a helper function to test return.matchgroups().
#' The helper helps standardize input
#' @param test_df A data.frame to use as input in return.matchgroups()
#' @param is_truematch_vector A vector of logical.  Length must match nrow(test_df)
test_helper_return.matchgroups <- function(test_df, is_truematch_vector){
  df1 <- test_df %>%
    mutate(is_truematch = is_truematch_vector) %>%
    mutate(var_id = as.character(var_id))
  df2 <- check.anymatch(df1, checkfor = variety_db, match_type = "raw")
  return(df2)
}


#' This is a helper function to test return.matchgroups().
#' The helper summarizes output
test_helper2_return.matchgroups <- function(test){
  as.numeric(map(test, ~nrow(.x)) %>% bind_rows())
}

###

test_that("create_intid generates anticipated ids, separating variety names", {

  df1 <- data.frame(variety = var_names)

  ans <- tibble(variety = c(var_names, var_names[2]),
                type = rep("variety", 3),
                    intid = c("testvar001", "testvar2", "tv00002"),
                    var_id = c("1","2","2"))

  test <- create_intid(df1, variety, sep_aliases = "//|/|\\(")
  rownames(test)<- NULL
  expect_equal(test, ans)
})

test_that("create_intid generates anticipated ids, not separating variety names", {

  df1 <- data.frame(variety = var_names)

  ans <- tibble(df1) %>%
          mutate(type = rep("variety",2)) %>%
          mutate(intid = c("testvar001", "testvar2testvar2tv00002")) %>%
          mutate(var_id = c("1","2"))

  test <- create_intid(df1, variety)
  rownames(test)<- NULL
  expect_equal(test, ans)
})

test_that("create_intid returns supplemental columns", {

  df1 <- data.frame(variety = var_names, supp = letters[1:2])

  ans <- tibble(variety = c(var_names, var_names[2]),
                supp = c("a", "b", "b"),
                type = rep("variety", 3),
                intid = c("testvar001", "testvar2", "tv00002"),
                var_id = c("1","2","2"))

  test <- create_intid(df1, variety, sep_aliases = "//|/|\\(", supp)
  rownames(test)<- NULL
  expect_equal(test, ans)
})

test_that("create_intid() creates the correct number of entries", {

  auxiliary_files <- here("tests", "example_cultivar_matching", "db", "auxiliary_files")
  var <- read_csv(here("tests", "test_example_files", "example_cultivar_matching_db.csv"), col_types = cols())

  names_raw <- create_intid(var, variety,
                            sep_aliases = "\\(",
                            crop_type, nursery)

  expect_equal(nrow(names_raw), 10)

})

test_that("create_intid generates the expected number of internal identifiers",{
  intid <- create_intid(var,
                        variety,
                        sep_aliases = "\\(",
                        crop_type, nursery)
  expect_equal(nrow(intid), 17)
})


test_that("check.anymatch() returns the correct information",{

  test <- check.anymatch(df1_mult_tests, checkfor = variety_db, match_type = "raw")

  ans <- df1_mult_tests %>% mutate(is_truematch = c(TRUE, FALSE, TRUE), any_match = c(TRUE, TRUE, TRUE))
  expect_equal(test, ans)
})

test_that("check.anymatch() returns the correct information if is_truematch == ''", {
  df2 <- test_helper_return.matchgroups(df1_mult_tests, c("", "check", TRUE))

  test <- check.anymatch(df2, checkfor = variety_db, match_type = "raw")

  ans <- df2 %>% mutate(is_truematch = c(NA, NA, TRUE), any_match = c(NA, TRUE, TRUE))
  expect_equal(test, ans)
})


test_that("check.anymatch() returns the correct information",{
  df1 <- tibble(variety = c(var_names, var_names[2]),
                intid = c("testvar001", "testvar2", "tv00002"),
                var_id = c(1,2,2),
                variety_db = c(NA, NA, "tv00002"))
  test <- check.anymatch(df1, checkfor = variety_db, match_type = "raw")

  ans <- df1 %>% mutate(is_truematch = c(FALSE, FALSE, TRUE), any_match = c(FALSE, TRUE, TRUE))
  expect_equal(test, ans)
})

test_that("check.anymatch() returns the correct information given an empty is_truematch",{
  df2 <- df1_mult_tests %>%
                mutate(is_truematch = c(NA, FALSE, FALSE))
  test <- check.anymatch(df2, checkfor = variety_db, match_type = "raw")

  ans <- df2 %>% mutate(any_match = c(NA, FALSE, FALSE))
  expect_equal(test, ans)
})

test_that("check.anymatch() returns the correct information given an empty is_truematch",{
  df2 <- df1_mult_tests %>%
    mutate(is_truematch = c(TRUE, FALSE, NA))
  test <- check.anymatch(df2, checkfor = variety_db, match_type = "raw")

  ans <- df2 %>% mutate(any_match = c(TRUE, NA, NA))
  expect_equal(test, ans)
})


test_that("return.matchgroups() returns the correct groups",{

  df2 <- test_helper_return.matchgroups(df1_mult_tests, c(TRUE, FALSE, TRUE))

  test <- return.matchgroups(df2)

  emptytibble <- df2 %>% filter(variety == "a")
  ans <- list(match = df2[c(1,3),],
              nomatch = emptytibble %>% select(-c(variety_db, is_truematch, any_match)),
              check = emptytibble,
              not_needed = df2[2,])

  expect_equal(test, ans)
})

test_that("return.matchgroups() returns the correct groups given empty is_truematch",{
  df2 <- test_helper_return.matchgroups(df2_mult_tests, c(FALSE, FALSE))
  test <- return.matchgroups(df2)
  expect_equal(test_helper2_return.matchgroups(test), c(0, 2, 0, 0))

  # May want return.matchgroups() to return an error if there is more than one is_truematch
  # for a given var_id, but this is not implemented at this time
  df2 <- test_helper_return.matchgroups(df2_mult_tests, c(TRUE, TRUE))
  test <- return.matchgroups(df2)
  expect_equal(test_helper2_return.matchgroups(test), c(2, 0, 0, 0))

  df2 <- test_helper_return.matchgroups(df2_mult_tests, c(FALSE, TRUE))
  test <- return.matchgroups(df2)
  expect_equal(test_helper2_return.matchgroups(test), c(1, 0, 0, 1))

  df2 <- test_helper_return.matchgroups(df2_mult_tests, c(FALSE, NA))
  test <- return.matchgroups(df2)
  expect_equal(test_helper2_return.matchgroups(test), c(0, 0, 1, 1))

  df2 <- test_helper_return.matchgroups(df2_mult_tests, c(TRUE, NA))
  test <- return.matchgroups(df2)
  expect_equal(test_helper2_return.matchgroups(test), c(1, 0, 0, 1))
})




test_that("do_exactmatch() returns the correct groups",{
  df1 <- tibble(variety = c(var_names, var_names[2]),
                intid = c("testvar001", "testvar2", "tv00002"),
                var_id = c(1,2,2))

  result <- do_exactmatch(df1)

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


test_that("stringdist.variety returns correct number of matches",{

  tempdf <- data.frame(intid = c("hi8", "konnnichiwa", "nihao8"), b = 1:3)
  dbdf <-  data.frame(intid = c("hi", "konichiwa", "salut"), z = 1:3)
  test <- stringdist.variety(tempdf,
                             dbdf,
                             intid_col = "intid",
                             best_n = 1,
                             method_stringdist = "lv")

  expect_equal(nrow(test), 6)
})



test_that("stringdist.variety returns correct number of matches",{

  tempdf <- data.frame(intid = c("meredith", "melbclub", "wb1035cl", "meridith"), b = 1:4)
  variety_intid_db <- get.variety_db(select_before = "2021-07-08",
                                                select_crops = NULL)
  test <- stringdist.variety(tempdf,
                             variety_intid_db,
                             intid_col = "intid",
                             best_n = 1,
                             method_stringdist = "lv")


  expect_equal(nrow(test), 11)
})

test_that("rm.alias_dupmatch correctly removes duplicates",{
  df <- data.frame(var_id = c(1,1,2,2,3),
                   db_id = c(1,1,2,3,4),
                   type_db = c("variety", "alias", "alias", "alias", "alias"))
  test <- rm.alias_dupmatch(df)
  ans1 <- df %>% filter(!(var_id == 1 & db_id == 1 & type_db == "alias"))
  ans2 <- df %>% filter((var_id == 1 & db_id == 1 & type_db == "alias"))
  ans <- list(match = ans1, not_needed = as_tibble(ans2))
  expect_equal(test, ans)
})







