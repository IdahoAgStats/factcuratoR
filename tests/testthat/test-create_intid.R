#library(magrittr)
var_names <- c("test VAR 001", "test Var 2 (testvar2/ tv00002)")

var <- read_csv(testthat::test_path("test_match_variety_files",
                                    "example_cultivar_matching.csv"))

blends <- read_csv(testthat::test_path("test_match_variety_files",
                                    "example_blends.csv"))

test_that("create_intid() generates anticipated ids, separating variety names", {
  df1 <- data.frame(variety = var_names)

  ans <- tibble::tibble(variety = c(var_names, var_names[2]),
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
    dplyr::mutate(type = rep("variety",2)) %>%
    dplyr::mutate(intid = c("testvar001", "testvar2testvar2tv00002")) %>%
    dplyr::mutate(var_id = c("1","2"))

  test <- create_intid(df1, variety)
  rownames(test) <- NULL
  expect_equal(test, ans)
})

test_that("create_intid() returns supplemental columns", {
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
  test_file <- testthat::test_path("test_match_variety_files",
                                   "example_cultivar_matching_db.csv")
  var <- read_csv(test_file, col_types = cols())

  names_raw <- create_intid(var, variety,
                            sep_aliases = "\\(",
                            crop_type, nursery)

  expect_equal(nrow(names_raw), 10)
})

test_that("create_intid() generates the expected number of internal identifiers",{
  intid <- create_intid(var,
                        variety,
                        sep_aliases = "\\(",
                        crop_type, nursery)
  expect_equal(nrow(intid), 17)
})

test_that("create_intid() generates anticipated ids with alias columns", {
  df1 <- data.frame(variety = var_names, alias = c("alias1", "alias2"))

  test <- create_intid(df1, variety, alias_col = alias)

  expect_equal(test$intid, c("alias1",  "alias2", "testvar001", "testvar2testvar2tv00002"))
})

test_that("create_intid() generates anticipated ids with alias columns", {
  df1 <- data.frame(variety = var_names, alias = c("alias1", "alias2"))
  expect_error(create_intid(df1, variety, sep_aliases = "//|/|\\(", alias_col = alias),
  "create_intid() has not been written to handle both sep_alias and an alias_col.  Please provide one or the other.",
  fixed = TRUE)
})

test_that("create_intid() generates anticipated ids and fills type with 'blends'", {

  test <- create_intid(blends, variety, sep_aliases = "\\/", is_blends = TRUE)
  expect_equal(nrow(test), 6)
  expect_equal(unique(test$type), "blends")

})

test_that("create_intid() generates a message when there are duplicate variety names", {
  df1 <- data.frame(variety = c(var_names, "test VAR 001"),
                    crop_type = c(rep("wheat", 2), "barley"))
  expect_message(create_intid(df1, variety, sep_aliases = "//|/|\\(", crop_type, alias_col = NULL),
                 regexp = "Check variety names for possible duplicates.*")
})

test_that("create_intid() does not generate a message if variety names are unique", {
  df1 <- data.frame(variety = c("var 001", "var002", "var_003"),
                    crop_type = c("wheat", "barley", "wheat"))
  expect_no_message(create_intid(df1, variety, sep_aliases = NULL, crop_type, alias_col = NULL),
                    message = "Check variety names for possible duplicates.*", class = NULL)
})
