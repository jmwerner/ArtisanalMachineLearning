# This script contains all functions for the k means algorithm

aml_k_means = function(data, k){
    # Description:
    #     Calculates k means predictions for given data. It is assumed that all
    #         columns will be used in calculation. 
    # Args:
    #     n: Number of 
    #     k: Maximum label (integers 1,...,k)
    # Return:
    #     Vector Int[nrow(data)] of assigned labels from the algorithm

    # Error checking here since this is the only outward-facing function
    .test_input(data, k)


}

.test_input = function(data, k){
    tryCatch(k > 0, 
             finally = stop("Number of groups k must be positive!"))
    tryCatch(class(data) == "data.frame", 
             finally = stop("Data must be a data.frame"))
    tryCatch(nrow(data) > 0, 
             finally = stop("Data must have a positive number of rows!"))
    tryCatch(all(sapply(data, is.numeric)), 
             finally = stop("All columns of data must be numeric!"))
}

.find_initial_assignments = function(n, k){
    # Description:
    #     Randomly assign starting labels to n points
    # Args:
    #     n: Number of 
    #     k: Maximum label (integers 1,...,k)
    # Return:
    #     Vector Int[N] of labels 
    sample(1:k, n, replace = TRUE)
}

.calculate_centroids = function(data){
    5
}


.calculate_within_cluster_variance = function(data, variance_function){
    5
}
