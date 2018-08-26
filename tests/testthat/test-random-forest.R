
context("random forest testing")

data(iris)
data = iris[,1:4]
names(data) = letters[1:4]
response = data$d
data$d = NULL

test_that("Single split is calculated correctly", {
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

test_that("Single tree training is correct", {
    one_tree = create_tree(data, response, evaluation_criterion = sum_of_squares, min_obs = 5, max_depth = 8)
    # dput(one_tree)
    expected_tree = structure(list(split_column = "c", criterion_value = 18.4066,
                split_value = 2.45, left = structure(list(split_column = "a",
                    criterion_value = 0.4575, split_value = 4.95, left = structure(list(
                        split_column_name = "b", criterion_value = 0.0578947368421053,
                        split_value = 2.6, prediction = 0.195, count = 20L), .Names = c("split_column_name",
                    "criterion_value", "split_value", "prediction", "count"
                    ), row.names = 2L, class = "data.frame"), right = structure(list(
                        split_column = "c", criterion_value = 0.314603174603175,
                        split_value = 1.55, left = structure(list(split_column_name = "b",
                            criterion_value = 0.128, split_value = 4.3, prediction = 0.247619047619048,
                            count = 21L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction", "count"
                        ), row.names = 2L, class = "data.frame"), right = structure(list(
                            split_column_name = "b", criterion_value = 0.135,
                            split_value = 3.15, prediction = 0.355555555555556,
                            count = 9L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction", "count"
                        ), row.names = 2L, class = "data.frame")), .Names = c("split_column",
                    "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
                "criterion_value", "split_value", "left", "right")), right = structure(list(
                    split_column = "c", criterion_value = 6.29527272727273,
                    split_value = 4.75, left = structure(list(split_column = "c",
                        criterion_value = 0.80582995951417, split_value = 4.15,
                        left = structure(list(split_column = "b", criterion_value = 0.246309523809524,
                            split_value = 2.65, left = structure(list(split_column_name = "c",
                              criterion_value = 0.07875, split_value = 3.95,
                              prediction = 1.09166666666667, count = 12L), .Names = c("split_column_name",
                            "criterion_value", "split_value", "prediction",
                            "count"), row.names = 3L, class = "data.frame"),
                            right = structure(list(split_column_name = "a",
                              criterion_value = 0.0541666666666667, split_value = 5.75,
                              prediction = 1.25714285714286, count = 7L), .Names = c("split_column_name",
                            "criterion_value", "split_value", "prediction",
                            "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                        "criterion_value", "split_value", "left", "right"
                        )), right = structure(list(split_column = "c", criterion_value = 0.3135,
                            split_value = 4.45, left = structure(list(split_column_name = "a",
                              criterion_value = 0.0433333333333333, split_value = 5.8,
                              prediction = 1.32, count = 10L), .Names = c("split_column_name",
                            "criterion_value", "split_value", "prediction",
                            "count"), row.names = 1L, class = "data.frame"),
                            right = structure(list(split_column_name = "a",
                              criterion_value = 0.177333333333333, split_value = 5.15,
                              prediction = 1.4625, count = 16L), .Names = c("split_column_name",
                            "criterion_value", "split_value", "prediction",
                            "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                        "criterion_value", "split_value", "left", "right"
                        ))), .Names = c("split_column", "criterion_value",
                    "split_value", "left", "right")), right = structure(list(
                        split_column = "c", criterion_value = 3.68673992673993,
                        split_value = 5.05, left = structure(list(split_column_name = "a",
                            criterion_value = 0.281363636363636, split_value = 6.75,
                            prediction = 1.73076923076923, count = 13L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction", "count"
                        ), row.names = 1L, class = "data.frame"), right = structure(list(
                            split_column = "b", criterion_value = 2.47298823529412,
                            split_value = 3.05, left = structure(list(split_column = "a",
                              criterion_value = 1.34404411764706, split_value = 6.35,
                              left = structure(list(split_column_name = "a",
                                criterion_value = 0.294666666666667, split_value = 5.85,
                                prediction = 1.7875, count = 8L), .Names = c("split_column_name",
                              "criterion_value", "split_value", "prediction",
                              "count"), row.names = 1L, class = "data.frame"),
                              right = structure(list(split_column_name = "a",
                                criterion_value = 0.564423076923077, split_value = 7.5,
                                prediction = 2.02941176470588, count = 17L), .Names = c("split_column_name",
                              "criterion_value", "split_value", "prediction",
                              "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                            "criterion_value", "split_value", "left", "right"
                            )), right = structure(list(split_column = "b",
                              criterion_value = 0.690972222222222, split_value = 3.25,
                              left = structure(list(split_column_name = "a",
                                criterion_value = 0.28875, split_value = 7.05,
                                prediction = 2.14444444444444, count = 9L), .Names = c("split_column_name",
                              "criterion_value", "split_value", "prediction",
                              "count"), row.names = 1L, class = "data.frame"),
                              right = structure(list(split_column_name = "a",
                                criterion_value = 0.148333333333333, split_value = 7.45,
                                prediction = 2.3125, count = 8L), .Names = c("split_column_name",
                              "criterion_value", "split_value", "prediction",
                              "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                            "criterion_value", "split_value", "left", "right"
                            ))), .Names = c("split_column", "criterion_value",
                        "split_value", "left", "right"))), .Names = c("split_column",
                    "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
                "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
            "criterion_value", "split_value", "left", "right"), class = c("aml_tree",
            "list"))
    expect_equal(one_tree, expected_tree)

    one_tree = create_tree(data, response, evaluation_criterion = sum_of_squares, min_obs = 20, max_depth = 20)
    # dput(one_tree)
    expected_tree = structure(list(split_column = "c", criterion_value = 18.4066,
            split_value = 2.45, left = structure(list(split_column_name = "a",
                criterion_value = 0.4575, split_value = 4.95, prediction = 0.246,
                count = 50L), .Names = c("split_column_name", "criterion_value",
            "split_value", "prediction", "count"), row.names = 1L, class = "data.frame"),
            right = structure(list(split_column = "c", criterion_value = 6.29527272727273,
                split_value = 4.75, left = structure(list(split_column_name = "c",
                    criterion_value = 0.80582995951417, split_value = 4.15,
                    prediction = 1.3, count = 45L), .Names = c("split_column_name",
                "criterion_value", "split_value", "prediction", "count"
                ), row.names = 3L, class = "data.frame"), right = structure(list(
                    split_column_name = "c", criterion_value = 3.68673992673993,
                    split_value = 5.05, prediction = 1.98363636363636,
                    count = 55L), .Names = c("split_column_name", "criterion_value",
                "split_value", "prediction", "count"), row.names = 3L, class = "data.frame")), .Names = c("split_column",
            "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
        "criterion_value", "split_value", "left", "right"), class = c("aml_tree",
        "list"))
    expect_equal(one_tree, expected_tree)

    one_tree = create_tree(data, response, evaluation_criterion = sum_of_squares, min_obs = 5, max_depth = 20)
    expected_tree = structure(list(split_column = "c", criterion_value = 18.4066,
            split_value = 2.45, left = structure(list(split_column = "a",
                criterion_value = 0.4575, split_value = 4.95, left = structure(list(
                    split_column_name = "b", criterion_value = 0.0578947368421053,
                    split_value = 2.6, prediction = 0.195, count = 20L), .Names = c("split_column_name",
                "criterion_value", "split_value", "prediction", "count"
                ), row.names = 2L, class = "data.frame"), right = structure(list(
                    split_column = "c", criterion_value = 0.314603174603175,
                    split_value = 1.55, left = structure(list(split_column_name = "b",
                        criterion_value = 0.128, split_value = 4.3, prediction = 0.247619047619048,
                        count = 21L), .Names = c("split_column_name",
                    "criterion_value", "split_value", "prediction", "count"
                    ), row.names = 2L, class = "data.frame"), right = structure(list(
                        split_column_name = "b", criterion_value = 0.135,
                        split_value = 3.15, prediction = 0.355555555555556,
                        count = 9L), .Names = c("split_column_name",
                    "criterion_value", "split_value", "prediction", "count"
                    ), row.names = 2L, class = "data.frame")), .Names = c("split_column",
                "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
            "criterion_value", "split_value", "left", "right")), right = structure(list(
                split_column = "c", criterion_value = 6.29527272727273,
                split_value = 4.75, left = structure(list(split_column = "c",
                    criterion_value = 0.80582995951417, split_value = 4.15,
                    left = structure(list(split_column = "b", criterion_value = 0.246309523809524,
                        split_value = 2.65, left = structure(list(split_column_name = "c",
                          criterion_value = 0.07875, split_value = 3.95,
                          prediction = 1.09166666666667, count = 12L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction",
                        "count"), row.names = 3L, class = "data.frame"),
                        right = structure(list(split_column_name = "a",
                          criterion_value = 0.0541666666666667, split_value = 5.75,
                          prediction = 1.25714285714286, count = 7L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction",
                        "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                    "criterion_value", "split_value", "left", "right"
                    )), right = structure(list(split_column = "c", criterion_value = 0.3135,
                        split_value = 4.45, left = structure(list(split_column_name = "a",
                          criterion_value = 0.0433333333333333, split_value = 5.8,
                          prediction = 1.32, count = 10L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction",
                        "count"), row.names = 1L, class = "data.frame"),
                        right = structure(list(split_column_name = "a",
                          criterion_value = 0.177333333333333, split_value = 5.15,
                          prediction = 1.4625, count = 16L), .Names = c("split_column_name",
                        "criterion_value", "split_value", "prediction",
                        "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                    "criterion_value", "split_value", "left", "right"
                    ))), .Names = c("split_column", "criterion_value",
                "split_value", "left", "right")), right = structure(list(
                    split_column = "c", criterion_value = 3.68673992673993,
                    split_value = 5.05, left = structure(list(split_column_name = "a",
                        criterion_value = 0.281363636363636, split_value = 6.75,
                        prediction = 1.73076923076923, count = 13L), .Names = c("split_column_name",
                    "criterion_value", "split_value", "prediction", "count"
                    ), row.names = 1L, class = "data.frame"), right = structure(list(
                        split_column = "b", criterion_value = 2.47298823529412,
                        split_value = 3.05, left = structure(list(split_column = "a",
                          criterion_value = 1.34404411764706, split_value = 6.35,
                          left = structure(list(split_column_name = "a",
                            criterion_value = 0.294666666666667, split_value = 5.85,
                            prediction = 1.7875, count = 8L), .Names = c("split_column_name",
                          "criterion_value", "split_value", "prediction",
                          "count"), row.names = 1L, class = "data.frame"),
                          right = structure(list(split_column_name = "a",
                            criterion_value = 0.564423076923077, split_value = 7.5,
                            prediction = 2.02941176470588, count = 17L), .Names = c("split_column_name",
                          "criterion_value", "split_value", "prediction",
                          "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                        "criterion_value", "split_value", "left", "right"
                        )), right = structure(list(split_column = "b",
                          criterion_value = 0.690972222222222, split_value = 3.25,
                          left = structure(list(split_column_name = "a",
                            criterion_value = 0.28875, split_value = 7.05,
                            prediction = 2.14444444444444, count = 9L), .Names = c("split_column_name",
                          "criterion_value", "split_value", "prediction",
                          "count"), row.names = 1L, class = "data.frame"),
                          right = structure(list(split_column_name = "a",
                            criterion_value = 0.148333333333333, split_value = 7.45,
                            prediction = 2.3125, count = 8L), .Names = c("split_column_name",
                          "criterion_value", "split_value", "prediction",
                          "count"), row.names = 1L, class = "data.frame")), .Names = c("split_column",
                        "criterion_value", "split_value", "left", "right"
                        ))), .Names = c("split_column", "criterion_value",
                    "split_value", "left", "right"))), .Names = c("split_column",
                "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
            "criterion_value", "split_value", "left", "right"))), .Names = c("split_column",
        "criterion_value", "split_value", "left", "right"), class = c("aml_tree",
        "list"))
    expect_equal(one_tree, expected_tree)
})
