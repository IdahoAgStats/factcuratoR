knitroutputfolder <- testthat::test_path("test_match_variety_files",
                                         "output")

controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")

test_that("find_fuzzymatch() returns output", {

  df1 <- tibble(variety = c("test1", "bruehlw"),
                intid = c("test1", "bruehlw"),
                crop_type = c("wheat", "wheat"),
                var_id = c(1,2),
                type = rep("variety", 2))

  output_fuzzymatch <-
    find_fuzzymatch(df1,
                    "test_data",
                    intid_col = "intid",
                    select_before = "2021-05-26",
                    knitroutputfolder = knitroutputfolder,
                    db_folder = controlled_vocab_folder)

  expect_s3_class(output_fuzzymatch, "data.frame")
})



df <- data.frame(a = c("a1000", "b000002", "c0101", "d"))

test_that("extract_trail_digits() correctly removes digits", {
  test <- extract_trail_digits(df, a, FALSE)
  ans <- df %>% mutate(a_traildigits = c("1000", "2", "101", NA)) %>%
    select(a_traildigits)
  expect_equal(test, ans)
})

test_that("extract_trail_digits() correctly removes digits", {
  test <- extract_trail_digits(df, a, TRUE)
  ans <- df %>% mutate(a_traildigits = c("1000", NA, "101", NA)) %>%
    select(a_traildigits)
  expect_equal(test, ans)
})

df2 <- data.frame(a = c("toast", "bread", "roll", NA), b = c("toasting", "brea", "cheese", "ham"))
test_that("is_string_overlap() correctly removes detects overlap", {
  test <- is_string_overlap(df2, a, b)
  ans <- data.frame(is_string_overlap = c(TRUE, TRUE, FALSE, NA))
  expect_equal(test, ans)
})
