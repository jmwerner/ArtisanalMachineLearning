
# PUT ROXYGEN HERE
#' @param sizes Vector of integer values corresponding to layer sizes 
#' @param training_data Data for training network, 
#' dimension nxp where p is sizes[1]
#' @export
aml_neural_network <- function(sizes, learning_rate, data = NULL, epochs = NULL){
    .test_neural_network_input(sizes)


    # TEST RUNNING - following book example

    sizes = c(1,2,1,1)

    data = data.frame(x = rnorm(15), y = runif(15))

    epochs = 10

    eta = 3.0

    initial_network = .initialize_random_network(sizes)

    initial_network$weights[[1]][[1]] = c(.1, .2)
    initial_network$weights[[1]][[2]] = c(.3, .4)

    initial_network$weights[[2]][[1]] = .2
    initial_network$weights[[2]][[2]] = 1
    initial_network$weights[[2]][[3]] = -3

    initial_network$weights[[3]][[1]] = 1
    initial_network$weights[[3]][[2]] = 2


    data_obs = c(2)

    # Check matrix dims here (will need transpose when slicing)
    activations = .feed_forward(initial_network, as.matrix(data_obs))

    deltas = .compute_deltas(initial_network, activations)

    # "Mini batch" is entire set right now 
    # http://neuralnetworksanddeeplearning.com/chap1.html
}

.test_neural_network_input <- function(sizes){
    if(length(sizes) < 2){
        stop(paste("Argument sizes must have more than 1 layer. Did you forget",
                   "to include the input or output layers?"))
    }
    if(!is.numeric(sizes)){
        stop("Argument sizes must be numeric vector of integers")
    }
    if(!all(as.integer(sizes) == sizes)){
        stop("Argument sizes must be vector of integers")
    }
}

.calculate_transformation <- function(z){
    tanh(z)
}

.calculate_transformation_prime <- function(z){
    1 - tanh(z)^2
}

.initialize_random_network <- function(sizes){
    layers = length(sizes)
    weights = lapply(2:layers, function(index){
        # Add one for bias term
        number_of_nodes_in_previous_layer = sizes[index - 1] + 1
        number_of_nodes_in_current_layer = sizes[index]
        lapply(1:number_of_nodes_in_previous_layer, function(x){
            rnorm(number_of_nodes_in_current_layer)
        })
    })

    output = list(sizes = sizes, 
                  layers = layers, 
                  weights = weights)
    output = .prepend_class(output, "aml_neural_network")
    output
}

.feed_forward <- function(network, observation){
    network_output = lapply(1:length(network$weights), function(x){
        .calculate_activations(network, observation, x)
    })

    network_output
}

.calculate_activations <- function(network, observation, layer){
    if(layer == 1){
        # Cat observation with 1 for the bias term
        z = do.call(cbind, network$weights[[layer]]) %*% as.matrix(c(1, observation))
        output = .calculate_transformation(z)
    }else{
        z = do.call(cbind, network$weights[[layer]]) %*% 
                as.matrix(c(1, .calculate_activations(network, observation, layer - 1)))
        output = .calculate_transformation(z)
    }
    output
}

.back_propogation <- function(network, data, epochs, learning_rate){
    for(epoch_number in 1:epochs){
        print(paste("Epoch: ", epoch_number))

        activations = .feed_forward()

        network = .update_network(network, data, learning_rate)
    }
    network
}

.update_network <- function(network, data, learning_rate){
    .compute_deltas()
    .
}

# .compute_deltas <- function(){

# }

.compute_cost_derivative <- function(output_activations, y){
    output_activations - y
}


