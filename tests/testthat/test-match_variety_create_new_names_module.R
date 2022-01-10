test_files <- testthat::test_path("test_match_variety_files")

outputfolder_test <- testthat::test_path("test_match_variety_files",
                                         "output")

controlled_vocab_folder <- testthat::test_path("test_controlled_vocab")


test_that("process_std_new_names() returns output", {


  results_fuzzymatch_nomatch_test <-
    tibble(variety = c("01Ab7163 ", "02Ab17373", "02Ab17373 "),
                intid = c("01ab7163", "02ab17373", "02ab17373"),
                crop_type = rep("barley",3),
                var_id = c("2", "8", "9"),
                type = rep("variety", 3),
                source = rep("csv", 3),
                n_var_id = rep(NA, 3))

  test <-
    process_std_new_names(
      output_nomatch_df = results_fuzzymatch_nomatch_test,
      auxiliary_files = test_files,
      filename = "standardize_new_names_aux.csv",
      knitroutputfolder = outputfolder_test,
      db_folder = controlled_vocab_folder)

  expect_s3_class(output_fuzzymatch, "data.frame")
})
