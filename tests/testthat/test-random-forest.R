
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

test_that("Single column split is calculated correctly when column has no variation", {
    dummy_data = data.frame(a = rep(3.4, length(response)))
    one_split = .find_one_column_split(dummy_data, "a", response, sum_of_squares)
    expect_equal(one_split$split_column_name, "a")
    expect_equal(one_split$criterion_value, NA)
    expect_equal(one_split$split_value, 3.4)
    expect_type(one_split$split_column_name, "character")
    expect_type(one_split$criterion_value, "logical")
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

test_that("single tree prediction works correctly", {
    one_tree = create_tree(data, response, evaluation_criterion = sum_of_squares, min_obs = 20, max_depth = 20)
    expect_error(predict(one_tree, data.frame(beans=1, pizza=2)), "ERROR: Split column not provided in prediction data row")
    expect_error(predict(one_tree, data.frame(a=rnorm(10), b=rnorm(10), c = rnorm(10))), "ERROR: data must be a data.frame of dimension 1 x p")
    expect_error(predict(one_tree, c(1, 2, 3)), "ERROR: data must be a data.frame")

    expected_predictions = c(0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,0.246000,1.300000,1.300000,1.983636,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.983636,1.300000,1.983636,1.300000,1.300000,1.300000,1.983636,1.983636,1.300000,1.300000,1.300000,1.300000,1.300000,1.983636,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.300000,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.300000,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636,1.983636)
    predictions = sapply(1:nrow(data), function(i){predict(one_tree, data[i,])})
    expect_equal(expected_predictions, predictions, tolerance = .00001)

    expect_equal(predict(one_tree, data.frame(c=1)), .246, tolerance = .00001)
    expect_equal(predict(one_tree, data.frame(c=3)), 1.3, tolerance = .00001) 
    expect_equal(predict(one_tree, data.frame(c=10)), 1.983636, tolerance = .00001) 

})

test_that("random forest prediction works correctly", {
    set.seed(1337)
    forest = aml_random_forest(data, response, evaluation_criterion = sum_of_squares, b = 25, m = 2, min_obs = 20, max_depth = 20)
    expect_error(predict(forest, data.frame(beans=1, pizza=2)), "ERROR: Split column not provided in prediction data row")
    expect_error(predict(forest, data.frame(a=rnorm(10), b=rnorm(10), c = rnorm(10))), "ERROR: data must be a data.frame of dimension 1 x p")
    expect_error(predict(forest, c(1, 2, 3)), "ERROR: data must be a data.frame")

    expected_predictions = c(1.19124714218882, 1.22298896784481, 1.21150233682877, 1.21104603980511,1.20929176693527, 1.1690221455812, 1.224876994208, 1.19734883889557,1.22298896784481, 1.21104603980511, 1.15802921771698, 1.2129340661683,1.22298896784481, 1.22298896784481, 1.16997214575668, 1.15802921771698,1.16997214575668, 1.19124714218882, 1.1690221455812, 1.17930421414912,1.1690221455812, 1.17930421414912, 1.224876994208, 1.19238736757725,1.23670881221433, 1.19546081253238, 1.19734883889557, 1.17930421414912,1.19124714218882, 1.19955940878906, 1.21104603980511, 1.15802921771698,1.17930421414912, 1.16997214575668, 1.21104603980511, 1.19591710955604,1.16997214575668, 1.224876994208, 1.22298896784481, 1.17930421414912,1.20929176693527, 1.21310171294285, 1.21150233682877, 1.19734883889557,1.20307896019515, 1.22298896784481, 1.17930421414912, 1.21150233682877,1.17930421414912, 1.21138199249918, 1.18907102305795, 1.16974940143633,1.20055765407399, 1.17060029270193, 1.19067039917203, 1.18999423209587,1.22318136054655, 1.22493353094918, 1.20055765407399, 1.18880959190027,1.20934830367645, 1.19988148699783, 1.17060029270193, 1.21920310861945,1.16806688079605, 1.18123603245237, 1.19988148699783, 1.17060029270193,1.18999423209587, 1.16753459546813, 1.20771647760341, 1.17060029270193,1.20931585371749, 1.20931585371749, 1.18123603245237, 1.18123603245237,1.19067039917203, 1.18880007831642, 1.19988148699783, 1.17002868249786,1.16753459546813, 1.16753459546813, 1.16753459546813, 1.21761818415779,1.19988148699783, 1.18460467819618, 1.20055765407399, 1.18999423209587,1.18048754760389, 1.17060029270193, 1.18999423209587, 1.21920310861945,1.17060029270193, 1.20934830367645, 1.18999423209587, 1.19988148699783,1.19988148699783, 1.19988148699783, 1.19130367893, 1.17060029270193,1.23075287396293, 1.21761818415779, 1.20812916749037, 1.22677462203583,1.20812916749037, 1.20812916749037, 1.21615017157921, 1.20812916749037,1.19824191258841, 1.20459594843232, 1.19737335349825, 1.19897272961233,1.20885998451429, 1.19755827795991, 1.21761818415779, 1.19737335349825,1.20885998451429, 1.20459594843232, 1.19824191258841, 1.19755827795991,1.19664253647433, 1.20931585371749, 1.19824191258841, 1.20931585371749,1.21210741941747, 1.19664253647433, 1.20931585371749, 1.21920310861945,1.19824191258841, 1.20812916749037, 1.19824191258841, 1.20459594843232,1.19824191258841, 1.21761818415779, 1.21688736713387, 1.20812916749037,1.22324140297777, 1.20885998451429, 1.21920310861945, 1.20885998451429,1.20812916749037, 1.20885998451429, 1.21761818415779, 1.19664253647433,1.21210741941747, 1.20885998451429, 1.19755827795991, 1.20885998451429,1.22397222000169, 1.22750543905975)
    predictions = sapply(1:nrow(data), function(i){predict(forest, data[i,])})
    expect_equal(expected_predictions, predictions, tolerance = .00001)

    set.seed(8675309)
    forest = aml_random_forest(data, response, evaluation_criterion = sum_of_squares, b = 2, m = 2, min_obs = 20, max_depth = 20)

    expect_equal(predict(forest, data.frame(a=1, b=1, c=1)), mean(c(1.215254, 0.9857143)), tolerance = .00001)
    expect_equal(predict(forest, data.frame(a=1, b=10, c=1)), mean(c(1.215254, 1.264348)), tolerance = .00001)

    expect_equal(predict(forest, data.frame(a=10, b=1, c=1)), mean(c(1.470968, 0.9857143)), tolerance = .00001)
    expect_equal(predict(forest, data.frame(a=10, b=10, c=1)), mean(c(1.470968, 1.264348)), tolerance = .00001)
    
    expect_equal(predict(forest, data.frame(a=10, b=1, c=10)), mean(c(1.3, 0.9857143)), tolerance = .00001)
    expect_equal(predict(forest, data.frame(a=10, b=10, c=10)), mean(c(1.3, 1.264348)), tolerance = .00001)

    expect_equal(predict(forest, data.frame(a=1, b=1, c=10)), mean(c(0.8947368, 0.9857143)), tolerance = .00001)
    expect_equal(predict(forest, data.frame(a=1, b=10, c=10)), mean(c(0.8947368, 1.264348)), tolerance = .00001)
})


context("gbm testing")

data = boot::amis
response = data$speed
data = data[,-1]

gbm = aml_gbm(data, response)
