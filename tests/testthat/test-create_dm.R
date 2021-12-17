codebook_path <- testthat::test_path(
  "test_controlled_vocab/codebooks_all_db.csv")

test_that("create_dm() creates a dm object", {
  test <- create_dm(codebook_main_path = codebook_path)

  expect_s3_class(test, "grViz")
})

