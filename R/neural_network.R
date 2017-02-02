
# PUT ROXYGEN HERE
#' @export
aml_neural_network <- function(sizes, learning_rate){
    .test_neural_network_input(sizes)
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



