
context("random forest testing")

test_that("Erroneous input returns error", {
    expect_true(aml_random_forest() == 5)
})


