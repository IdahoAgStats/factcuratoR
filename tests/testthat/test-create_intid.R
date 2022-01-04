library(magrittr)
var_names <- c("test VAR 001", "test Var 2 (testvar2/ tv00002)")

var <- read_csv(testthat::test_path("test_match_variety_files",
                                    "example_cultivar_matching.csv"))

test_that("create_intid() generates anticipated ids, separating variety names", {
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
