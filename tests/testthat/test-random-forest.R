# Use iris for unit testing
data(iris)

context("Random Forest testing")

test_that("Erroneous input returns error", {
    expect_error(aml_k_means(iris, -1))
    expect_error(aml_k_means(iris, 4.1))
    expect_error(aml_k_means(5, 1))
    expect_error(aml_k_means(data.frame(), 1))
    expect_error(aml_k_means(matrix(1:5), 1))
    expect_error(aml_k_means(data.frame(letters), 1))
    expect_error(aml_k_means(data.frame(letters[1:5], 1:5), 1))
})


