#' AML neural network
#'
#' Trains a simple back-propagation neural network.
#'
#' @param sizes Vector of layer sizes
#' @param learning_rate Learning rate for gradient descent
#' @param data Input data.frame for training network, 
#' dimension nxp where p is sizes[1]
#' @param response Response vector of size nx1 corresponding to the training
#' data
#' @param epochs Number of epochs to use in network training
#' @param verbose Logical option for status printing
#' @return Trained neural network object of class aml_neural_network
#' @export
aml_neural_network <- function(sizes, learning_rate, data, response, epochs, verbose=FALSE){
    .test_neural_network_input(sizes, learning_rate, data, response, epochs, verbose)

    initial_network = .initialize_random_network(sizes)

    processed_network = .back_propogation(initial_network, data, response, epochs, learning_rate, verbose)

    processed_network
}

#' AML neural network predict method
#'
#' Returns predictions for a neural network when given a data set.
#'
#' @param network Results network object returned by aml_neural_network function 
#' @param data Data of correct size to be fed into the neural network
#' @return Vector of predictions corresponding to the data rows
#' @export
predict.aml_neural_network <- function(network, data){
    prediction_vector = sapply(1:nrow(data), function(i){
        data_observation = as.matrix(data[i,])
        forward = .feed_forward(network, data_observation)
        forward[[length(forward)]]$output
    })
    prediction_vector
}

################################################################################

.test_neural_network_input <- function(sizes, learning_rate, data, response, epochs, verbose){
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
    if(!is.numeric(learning_rate)){
        stop("Argument learning_rate must be numeric")
    }
    if(learning_rate <= 0){
        stop("Argument learning_rate must be greater than 0")
    }
    if(!is.data.frame(data)){
        stop("Argument data must be a data.frame")
    }
    if(!all(sapply(data, is.numeric))){
        stop("Argument data must have all numeric columns")
    }
    if(!is.numeric(response)){
        stop("Argument response must be a numeric vector")
    }
    if(!(as.integer(epochs) == epochs) | (epochs <= 0)){
        stop("Argument epochs must be a postive integer")
    }
    if(!(length(response) == nrow(data))){
        stop("Length mismatch: response vector must be equal to number of training data rows")
    }
    if(sizes[1] != ncol(data)){
        stop("First layer must be equal to the number of data columns")
    }
    if(sizes[length(sizes)] != 1){
        stop("Output layer must be equal to 1, multi-dimensional output currently not supported")
    }
    if(!is.logical(verbose)){
        stop("Argument verbose must be TRUE or FALSE")
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
    # Calculated from a heuristic 
    random_sd = .5
    weights = lapply(2:layers, function(index){
        # Add one for bias term
        number_of_nodes_in_previous_layer = sizes[index - 1] + 1
        number_of_nodes_in_current_layer = sizes[index]
        weight_list = lapply(1:number_of_nodes_in_previous_layer, function(x){
            rnorm(number_of_nodes_in_current_layer, mean=0, sd=random_sd)
        })
        do.call(rbind, weight_list)
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
        s = t(network$weights[[layer]]) %*% as.matrix(c(1, observation))
        output = .calculate_transformation(s)
    }else{
        s = t(network$weights[[layer]]) %*% 
            as.matrix(c(1, .calculate_activations(network, observation, layer - 1)$output))
        output = .calculate_transformation(s)
    }
    list(output = output, s = s)
}

.back_propogation <- function(network, data, response, epochs, learning_rate, verbose=FALSE){
    for(epoch_number in 1:epochs){
        if(verbose){
            print(paste(date(), "|", "Epoch:", epoch_number, "starting..."))
        }
        # This should be parallelized in batches, but it is not.
        for(row_number in 1:nrow(data)){
            data_observation = as.matrix(data[row_number,])
            response_observation = response[row_number]
            activations = .feed_forward(network, data_observation)
            deltas = .compute_deltas(network, activations, response_observation)
            partial_derivatives = .calculate_partial_derivatives(activations, deltas, data_observation)
            network = .update_network(network, partial_derivatives, learning_rate)
        }
        if(verbose){
            print(paste(date(), "|", "Epoch:", epoch_number, "complete!"))
        }
    }
    network
}

.update_network <- function(network, partial_derivatives, learning_rate){
    for(i in 1:length(network$weights)){
        network$weights[[i]] = network$weights[[i]] - learning_rate * partial_derivatives[[i]]
    }
    network
}

.compute_deltas <- function(network, activations, response){
    deltas = list()
    for(i in (network$layers - 1):1){
        if(i == (network$layers - 1)){
            deltas[[i]] = 2 * (activations[[i]]$output - response) * 
                .calculate_transformation_prime(activations[[i]]$s)
        }else{
            deltas[[i]] = (1 - activations[[i]]$output ^ 2) *
                as.matrix(network$weights[[i + 1]][-1,]) %*% deltas[[i + 1]] 
        }
    }
    deltas
}

.calculate_partial_derivatives <- function(activations, deltas, data_obs){
    partial_derivatives = list()
    for(i in 1:length(activations)){
        if(i == 1){
            partial_derivatives[[i]] = matrix(c(1,data_obs))%*% t(deltas[[i]])
        }else{
            partial_derivatives[[i]] = matrix(c(1, activations[[i - 1]]$output)) %*% t(deltas[[i]])  
        }
    }
    partial_derivatives
}
