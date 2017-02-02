################################################################################
context("neural network input testing")
################################################################################

test_that("Erroneous neural network input returns error", {
    expect_error(aml_neural_network(1, 1))
    expect_error(aml_neural_network(c("hello", "there"), 1))
    expect_error(aml_neural_network(c(1.5, 2.6, 2.1), 1))
})

################################################################################
context("neural network internal algorithm testing")
################################################################################

test_that("Sigmoid function works properly", {
    expect_true(abs(.calculate_sigmoid(5) - .9933071) < .0000001)
    expect_true(abs(.calculate_sigmoid(-10) - 4.539787e-05) < .0000001)
    expect_error(.calculate_sigmoid("abc"))
})

test_that("Random network initialization works properly", {
    random_net = .initialize_random_network(c(1, 2, 3))
    expect_is(random_net, "list")
    expect_is(random_net, "aml_neural_network")
    expect_identical(names(random_net), c("sizes", 
                                          "layers", 
                                          "biases", 
                                          "weights"))
    expect_true(length(random_net) == 4)
    expect_identical(lapply(random_net, length), list(sizes = as.integer(3),
                                                      layers = as.integer(1),
                                                      biases = as.integer(2),
                                                      weights = as.integer(2)))
    expect_identical(lapply(random_net$biases, length), list(as.integer(2), 
                                                             as.integer(3)))
    expect_identical(lapply(random_net$weights, length), list(as.integer(1), 
                                                             as.integer(2)))
    expect_identical(lapply(random_net$weights[[1]], length), 
                     list(as.integer(2)))
    expect_identical(lapply(random_net$weights[[2]], length), 
                     list(as.integer(3), as.integer(3)))
})

################################################################################
context("neural network output testing")
################################################################################



