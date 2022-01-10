knitroutputfolder <- testthat::test_path("test_match_variety_files",
                                         "output")

test_output_fuzzymatch <-
  read.csv(
    testthat::test_path("test_match_variety_files",
                        "test_file_marshall_output_fuzzymatch.csv"))

test_aux <- read.csv(

  testthat::test_path("test_match_variety_files",
                      "test_file_marshall_fuzzy_aux.csv"),
  stringsAsFactors = FALSE, na.strings = c("NA", ""))


test_that("process_fuzzymatch() returns the correct matches", {

  test <- process_fuzzymatch(output_fuzzymatch_df = test_output_fuzzymatch,
                             aux_fuzzy_status = test_aux,
                             knitroutputfolder = knitroutputfolder)

  ans <- tibble(match = 1, nomatch = 3, check = 1, not_needed = 0)
  expect_equal(test %>% map(~nrow(.)) %>% bind_rows(), ans)
})

test_that("process_fuzzymatch() detects when var_ids don't match", {

  test_aux$var_id[1] <- 0
  expect_warning(
    process_fuzzymatch(output_fuzzymatch_df = test_output_fuzzymatch,
                       aux_fuzzy_status = test_aux,
                       knitroutputfolder = knitroutputfolder),
    "var_id has changed between the two inputs. Setting aux_fuzzy_status var_id to NA")
})


test_that("assure_var_id() detects var_ids don't match", {

  test_aux$var_id[1] <- 0

  test <- assure_var_id(test_output_fuzzymatch, test_aux)

  expect_false(test)
})

