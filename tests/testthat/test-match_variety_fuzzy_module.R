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
