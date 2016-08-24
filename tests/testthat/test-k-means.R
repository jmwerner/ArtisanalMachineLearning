
library(testthat)

# source("../k_means.R")

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

test_that("Centroid calculation returns expected means", {
    expect_true(all(.calculate_centroids(data.frame(c(1, 1)), c(1, 2)) == 
                    c(1, 1)))
    expect_true(all(.calculate_centroids(data.frame(c(1, 1)), c(1, 1)) == 1))
    expect_true(all(.calculate_centroids(data.frame(rep(10, 5)), 1:5) == 
                    rep(10, 5)))
    expect_true(all(.calculate_centroids(iris[, 1:4], 
                                         as.numeric(iris$Species)) == 
                    data.frame(Sepal.Length = c(5.006, 5.936, 6.588),
                               Sepal.Width  = c(3.428, 2.770, 2.974),
                               Petal.Length = c(1.462, 4.260, 5.552),
                               Petal.Width  = c(0.246, 1.326, 2.026))))
    expect_true(.calculate_centroids(data.frame(c(500)), c(1)) == 500)
})


