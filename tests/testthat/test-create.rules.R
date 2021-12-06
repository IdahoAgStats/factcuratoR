test_that("create.rules() creates a validator object is created",{
  test <- create.rules("trial_data")
  expect_is(test, "validator")

  test <- create.rules("trials_metadata")
  expect_is(test, "validator")

})
