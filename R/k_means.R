# This script contains all functions for the k means algorithm

#' AML k-Means
#'
#' Calculates k means predictions for given data. It is assumed that all
#' given columns will be used in calculation. 
#'
#' @param data Input n x p sized data.frame of data
#' @param k Number of clusters to fit
#' @return Vector of assigned cluster values
#' @export
aml_k_means = function(data, k){
    .test_input(data, k)


    5
}

#' Test input 
#'
#' This function will error-out early if any inputs to the aml_k_means function
#' are unsatisfactory
#'
#' @param data data.frame input of size n x p
#' @param k Maximum label number (labels are integers 1,...,k)
#' @return NULL, function stops execution if error occurs
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

#' Find initial assignments
#' 
#' This function randomly assign starting labels to n points
#' 
#' @param n Number of labels to assign (equal to nrow(data))
#' @param k Maximum label (integers 1,...,k)
#' @param seed Random seed to be set for number generator
#' @return Vector Int[N] of labels 
.find_initial_assignments = function(n, k, seed = 1337){
    set.seed(seed)
    sample(1:k, n, replace = TRUE)
}

#' Calculate centroids
#' 
#' This function calculates the data centroids for the given labels
#' 
#' @param data numeric data.frame of size n x p
#' @param labels vector of labels of length n
#' @return data.frame that returns each label group's mean by column,
#' which is a k x p data.frame
.calculate_centroids = function(data, labels){
    centroids = sapply(data, function(column){
        tapply(column, as.factor(labels), mean)
    })
    data.frame(centroids)
}

#' Calculate distances from centroids
#' 
#' This function calculates the pointwise distances from the given data points
#' to the given centroids
#' 
#' @param data numeric data.frame of size n x p
#' @param centroids numeric data.frame of k x p
#' @return list of length k with distances to centroids in order
.calculate_distances_from_centroids = function(data, centroids){
    distances = list()
    n = nrow(data)
    for(roid in 1:nrow(centroids)){
        distances_for_roid = numeric(n)
        for(i in 1:n){
            distances_for_roid[i] = dist(rbind(centroids[roid,], data[i,]))
        }
        distances[[roid]] = distances_for_roid
    }
    distances
}

