# This script contains all functions for the k means algorithm

#' AML k-Means
#'
#' Calculates k means predictions for given data. It is assumed that all
#' given columns will be used in calculation
#'
#' @param data Input n x p sized data.frame of data
#' @param k Number of clusters to fit
#' @return Vector of assigned cluster values
#' @export
aml_k_means = function(data, k){
    .test_input(data, k)

    new_labels = .find_initial_assignments(nrow(data), k)
    labels = rep(0, nrow(data))
    iter = 0

    while(!all(labels == new_labels) & iter < 1000){
        labels = new_labels
        roids = .calculate_centroids(data, new_labels)
        distances = .calculate_distances_from_centroids(data, roids)
        new_labels = .calculate_new_labels(distances)
        iter = iter + 1
    }

    new_labels
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
    if(k <= 0){
        stop("Number of groups k must be positive!")
    }
    if(class(data) != "data.frame"){
        stop("Data must be a data.frame")
    }
    if(nrow(data) <= 0){
        stop("Data must have a positive number of rows!")
    }
    if(!all(sapply(data, is.numeric))){
        stop("All columns of data must be numeric!")
    }
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

#' Calculate new labels
#' 
#' This function calculates new integer labels based on the smallest distance
#' where repeated min values go to the first occurrence of the min
#' 
#' @param distances list of length k with distances to centroids in order
#' @return vector of length n with new integer labels
.calculate_new_labels = function(distances){
    matrix_of_distances = do.call(cbind, distances)
    apply(matrix_of_distances, 1, which.min)
}





