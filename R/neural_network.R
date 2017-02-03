
# PUT ROXYGEN HERE
#' @param sizes Vector of integer values corresponding to layer sizes 
#' @param training_data Data for training network, 
#' dimension nxp where p is sizes[1]
#' @export
aml_neural_network <- function(sizes, learning_rate, data){
    .test_neural_network_input(sizes)


    # TEST RUNNING

    sizes = c(2,3,1)

    initial_network = .initialize_random_network(sizes)

    .feed_forward(initial_network, c(10, 20))

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

.calculate_sigmoid <- function(z){
    1 / (1 + exp(-z))
}

.calculate_sigmoid_prime <- function(z){
    sigmoid = .calculate_sigmoid(z) 
    sigmoid * (1 - sigmoid)
}

.initialize_random_network <- function(sizes){
    layers = length(sizes)
    biases = lapply(sizes[-1], rnorm)
    weights = lapply(2:layers, function(index){
        number_of_nodes_in_current_layer = sizes[index]
        number_of_nodes_in_previous_layer = sizes[index-1]
        lapply(1:number_of_nodes_in_previous_layer, function(x){
            rnorm(number_of_nodes_in_current_layer)
        })
    })

    output = list(sizes = sizes, 
                  layers = layers, 
                  biases = biases, 
                  weights = weights)
    output = .prepend_class(output, "aml_neural_network")
    output
}

.feed_forward <- function(network, observation){
    network_output = .calculate_output_a(network, 
                                         observation, 
                                         length(network$weights))

    network_output
}


.calculate_output_a <- function(network, observation, layer){
    if(layer == 1){
        z = do.call(cbind, network$weights[[layer]]) %*% observation + 
                network$biases[[layer]]
        output = .calculate_sigmoid(z)
    }else{
        z = do.call(cbind, network$weights[[layer]]) %*% 
                .calculate_a(network, observation, layer - 1) + 
                network$biases[[layer]]
        output = .calculate_sigmoid(z)
    }
    output
}








