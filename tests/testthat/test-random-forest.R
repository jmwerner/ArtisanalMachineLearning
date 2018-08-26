
context("random forest testing")

test_that("Single split is calculated correctly", {
    data(iris)
    data = iris[,1:4]
    names(data) = letters[1:4]
    response = data$d
    data$d = NULL
    one_split = .find_one_split(data, response, sum_of_squares)

    expect_equal(one_split$split_column_name, "c")
    expect_equal(one_split$criterion_value, 18.4066)
    expect_equal(one_split$split_value, 2.45)

    expect_type(one_split$split_column_name, "character")
    expect_type(one_split$criterion_value, "double")
    expect_type(one_split$split_value, "double")

    expect_equal(dim(one_split), c(1, 3))
})

test_that("Single column split is calculated correctly", {
    data(iris)
    data = iris[,1:4]
    names(data) = letters[1:4]
    response = data$d
    data$d = NULL

    one_split = .find_one_column_split(data, "a", response, sum_of_squares)
    expect_equal(one_split$split_column_name, "a")
    expect_equal(one_split$criterion_value, 30.59534)
    expect_equal(one_split$split_value, 5.55)
    expect_type(one_split$split_column_name, "character")
    expect_type(one_split$criterion_value, "double")
    expect_type(one_split$split_value, "double")
    expect_equal(dim(one_split), c(1, 3))
    one_split = .find_one_column_split(data, "b", response, sum_of_squares)
    expect_equal(one_split$split_column_name, "b")
    expect_equal(one_split$criterion_value, 67.46669, tolerance = .0001)
    expect_equal(one_split$split_value, 3.35)
    expect_type(one_split$split_column_name, "character")
    expect_type(one_split$criterion_value, "double")
    expect_type(one_split$split_value, "double")
    expect_equal(dim(one_split), c(1, 3))
    one_split = .find_one_column_split(data, "c", response, sum_of_squares)
    expect_equal(one_split$split_column_name, "c")
    expect_equal(one_split$criterion_value, 18.4066)
    expect_equal(one_split$split_value, 2.45)
    expect_type(one_split$split_column_name, "character")
    expect_type(one_split$criterion_value, "double")
    expect_type(one_split$split_value, "double")
    expect_equal(dim(one_split), c(1, 3))
})
