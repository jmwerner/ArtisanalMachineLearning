################################################################################
context("neural network input testing")
################################################################################

test_that("Erroneous neural network input returns error", {
    expect_error(aml_neural_network(1, 1), "Argument sizes must have more than 1 layer. Did you forget to include the input or output layers?")
    expect_error(aml_neural_network(c("hello", "there"), 1), "Argument sizes must be numeric vector of integers")
    expect_error(aml_neural_network(c(1.5, 2.6, 2.1), 1), "Argument sizes must be vector of integers")

    expect_error(aml_neural_network(sizes=c(1, 1), learning_rate="hello"), "Argument learning_rate must be numeric")
    expect_error(aml_neural_network(sizes=c(1, 1), learning_rate=-10), "Argument learning_rate must be greater than 0")
    
    expect_error(aml_neural_network(sizes=c(1, 1), learning_rate=1, data = 1L), "Argument data must be a data.frame")
    expect_error(aml_neural_network(sizes=c(1, 1), learning_rate=1, data = data.frame(c("a", "b"), rnorm(2))), "Argument data must have all numeric columns")
    expect_error(aml_neural_network(sizes=c(1, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=letters[1:2])), "Argument data must have all numeric columns")

    expect_error(aml_neural_network(sizes=c(2, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=rnorm(2)),
                                    response=c("a", "b", "c")), "Argument response must be a numeric vector")

    expect_error(aml_neural_network(sizes=c(2, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=rnorm(2)),
                                    response=c(1, 2), epochs = -5), "Argument epochs must be a postive integer")
    expect_error(aml_neural_network(sizes=c(2, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=rnorm(2)),
                                    response=c(1, 2), epochs = 5.5), "Argument epochs must be a postive integer")

    expect_error(aml_neural_network(sizes=c(2, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=rnorm(2)),
                                    response=c(1, 2, 3), epochs = 100), "Length mismatch: response vector must be equal to number of training data rows")
    expect_error(aml_neural_network(sizes=c(3, 1), learning_rate=1, data = data.frame(x=rnorm(3), y=rnorm(3), z=rnorm(3)),
                                    response=c(1, 2), epochs = 100), "Length mismatch: response vector must be equal to number of training data rows")
    expect_error(aml_neural_network(sizes=c(1, 1), learning_rate=1, data = data.frame(x=rnorm(3), y=rnorm(3), z=rnorm(3)),
                                    response=c(1, 2, 3), epochs = 100), "First layer must be equal to the number of data columns")
    expect_error(aml_neural_network(sizes=c(3, 4), learning_rate=1, data = data.frame(x=rnorm(3), y=rnorm(3), z=rnorm(3)),
                                    response=c(1, 2, 3), epochs = 100), "Output layer must be equal to 1, multi-dimensional output currently not supported")
    expect_error(aml_neural_network(sizes=c(2, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=rnorm(2)), response=c(1,2), epochs=5, verbose=1L), "Argument verbose must be TRUE or FALSE")
    expect_error(aml_neural_network(sizes=c(2, 1), learning_rate=1, data = data.frame(x=rnorm(2), y=rnorm(2)), response=c(1,2), epochs=5, verbose="PIZZA"), "Argument verbose must be TRUE or FALSE")
})

################################################################################
context("neural network internal algorithm testing")
################################################################################

test_that("Sigmoid functions work properly", {
    expect_true(abs(.calculate_transformation(5) - 0.9999092) < .0000001)
    expect_true(abs(.calculate_transformation(-10) + 1) < .0000001)
    expect_true(all(abs(.calculate_transformation(c(-10, -10)) - rep(-1, 2)) < rep(.0000001, 2)))
    expect_error(.calculate_transformation("abc"))
    expect_true(abs(.calculate_transformation_prime(5) - 0.0001815832) < .0000001)
    expect_true(abs(.calculate_transformation_prime(-10) - 8.244615e-09) < .0000001)
    expect_error(.calculate_transformation_prime("abc"))
})

test_that("Random network initialization works properly", {
    random_net = .initialize_random_network(c(1, 2, 3))
    expect_is(random_net, "list")
    expect_is(random_net, "aml_neural_network")
    expect_identical(names(random_net), c("sizes", "layers", "weights"))
    expect_true(length(random_net) == 3)
    expect_identical(lapply(random_net, length), structure(list(sizes = 3L, layers = 1L, weights = 2L), .Names = c("sizes", "layers", "weights")))
    expect_identical(lapply(random_net$weights, length), list(as.integer(4), as.integer(9)))
    expect_identical(lapply(random_net$weights, dim), list(c(2L, 2L), c(3L, 3L)))
})

test_that("Backpropogation book example works properly", {
    sizes = c(1,2,1,1)
    epochs = 1
    learning_rate = .1
    initial_network = .initialize_random_network(sizes)
    initial_network$weights[[1]] = matrix(c(.1, .2, .3, .4), 2, 2, byrow = TRUE)
    initial_network$weights[[2]] = matrix(c(.2, 1, -3))
    initial_network$weights[[3]] = matrix(1:2)
    data = data.frame(c(2))
    data_obs = as.matrix(data)
    response = 1
    activations = .feed_forward(initial_network, as.matrix(data_obs))
    deltas = .compute_deltas(initial_network, activations, response)
    partial_derivatives = .calculate_partial_derivatives(activations, deltas, data_obs)
    network = .update_network(initial_network, partial_derivatives, learning_rate)
    network_2 = .back_propogation(initial_network, data, response, epochs, learning_rate)

    expected_activations = list(structure(list(output = structure(c(0.604367777117163, 0.761594155955765
        ), .Dim = c(2L, 1L)), s = structure(c(0.7, 1), .Dim = c(2L, 1L
        ))), .Names = c("output", "s")), structure(list(output = structure(-0.901545653372868, .Dim = c(1L,
        1L)), s = structure(-1.48041469075013, .Dim = c(1L, 1L))), .Names = c("output",
        "s")), structure(list(output = structure(-0.665761435493746, .Dim = c(1L,
        1L)), s = structure(-0.803091306745735, .Dim = c(1L, 1L))), .Names = c("output",
        "s")))
    expected_deltas = list(structure(c(-0.440838375756939, 0.875039825175393), .Dim = c(2L,
        1L)), structure(-0.694518480829472, .Dim = c(1L, 1L)), structure(-1.85486437391763, .Dim = c(1L,
        1L)))
    expected_partial_derivatives = list(structure(c(-0.440838375756939, -0.881676751513878, 0.875039825175393,
        1.75007965035079), .Dim = c(2L, 2L)), structure(c(-0.694518480829472,
        -0.419744590425697, -0.528941216203002), .Dim = c(3L, 1L)), structure(c(-1.85486437391763,
        1.67224491390163), .Dim = c(2L, 1L)))
    expected_network = structure(list(sizes = c(1, 2, 1, 1), layers = 4L, weights = list(
    structure(c(0.144083837575694, 0.388167675151388, 0.112496017482461,
    0.224992034964921), .Dim = c(2L, 2L)), structure(c(0.269451848082947,
    1.04197445904257, -2.9471058783797), .Dim = c(3L, 1L)), structure(c(1.18548643739176,
    1.83277550860984), .Dim = c(2L, 1L)))), .Names = c("sizes",
    "layers", "weights"), class = c("aml_neural_network", "list"))

    expect_equal(activations, expected_activations)
    expect_equal(deltas, expected_deltas)
    expect_equal(partial_derivatives, expected_partial_derivatives)
    expect_equal(network, expected_network)
    expect_equal(network, network_2)
})
