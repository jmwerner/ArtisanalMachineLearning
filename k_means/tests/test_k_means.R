
library(testthat)

source("../k_means.R")

# Use iris for unit testing
data(iris)


context("Input testing")

test_that("Erroneous input returns error", {
    expect_error(aml_k_means(iris, -1))
    expect_error(aml_k_means(5, 1))
    expect_error(aml_k_means(matrix(1:5), 1))
    expect_error(aml_k_means(data.frame(letters), 1))
    expect_error(aml_k_means(data.frame(letters[1:5], 1:5), 1))
})

context("Internal algorithm testing")

test_that("Initial assignments are returning integers", {
    all_initial_assignment_tests = sapply(1:20, function(k){
        initial_assignments_big = .find_initial_assignments(100, k)
        initial_assignments_small = .find_initial_assignments(1, k)
        (max(initial_assignments_small) <= k) &
        (min(initial_assignments_small) >= 1) &
        (max(initial_assignments_big) <= k) &
        (min(initial_assignments_big) >= 1)
    })
    expect_true(all(all_initial_assignment_tests))
})

