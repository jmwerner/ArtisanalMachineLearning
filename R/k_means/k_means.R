# This script contains all functions for the k means algorithm

aml_k_means = function(data, k){
    # Description:
    #     Calculates k means predictions for given data. It is assumed that all
    #         given columns will be used in calculation. 
    # Args:
    #     n: Number of 
    #     k: Maximum label number (labels are integers 1,...,k)
    # Return:
    #     Vector Int[nrow(data)] of assigned labels from the algorithm

    # Error checking here since this is the only outward-facing function
    .test_input(data, k)



}

.test_input = function(data, k){
    # Description:
    #     This function will error-out early if any inputs are unsatisfactory
    # Args:
    #     data: data.frame input of size n x p
    #     k: Maximum label number (labels are integers 1,...,k)
    # Return:
    #     NULL

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
    #     n: Number of labels to assign (equal to nrow(data))
    #     k: Maximum label (integers 1,...,k)
    # Return:
    #     Vector Int[N] of labels 
    sample(1:k, n, replace = TRUE)
}

.calculate_centroids = function(data, labels){
    # Description:
    #     Calculates centroid values of data given labels
    # Args:
    #     data: numeric data.frame of size n x p
    #     labels: vector of labels of length n
    # Return:
    #     data.frame that returns each label group's mean by column,
    #         which is a k x p data.frame
    centroids = sapply(data, function(column){
        tapply(column, as.factor(labels), mean)
    })
    data.frame(centroids)
}


.calculate_within_cluster_variance = function(data, variance_function){
    5
}
