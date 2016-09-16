# This script contains all functions for the k means algorithm

#' AML k-Means
#'
#' Calculates k means predictions for given data. It is assumed that all
#' given columns will be used in calculation.
#'
#' @param data Input n x p sized data.frame of numeric data
#' @param k Number of clusters to fit
#' @param maximum_iterations Maximum number of iterations to allow while trying
#' to converge
#' @return Results object of class aml_k_means
#' @export
aml_k_means = function(data, k, maximum_iterations = 1e6){
    .test_input(data, k)

    results = .run_algorithm_until_convergence(data, k, maximum_iterations)

    .create_output_object(results)
}


.run_algorithm_until_convergence = function(data, k, maximum_iterations){
    new_labels = .find_initial_assignments(nrow(data), k)
    labels = rep(0, nrow(data))
    iter = 0

    while(!all(labels == new_labels) & iter < maximum_iterations){
        labels = new_labels
        roids = .calculate_centroids(data, new_labels)
        distances = .calculate_distances_from_centroids(data, roids)
        new_labels = .calculate_new_labels(distances)
        iter = iter + 1
    }

    if(iter == maximum_iterations){
        warning(paste("Convergence was not met, stopped at iteration", 
                      maximum_iterations))
    }

    list(new_labels = new_labels, 
         data = data, 
         iter = iter, 
         roids = roids, 
         k = k)
}

.test_input = function(data, k){
    if(k <= 0){
        stop("Number of groups k must be positive!")
    }
    if(as.integer(k) != k){
        stop("Number of groups k must be an integer!")
    }
    if(!is.data.frame(data)){
        stop("Data must be a data.frame!")
    }
    if(nrow(data) <= 0){
        stop("Data must have a positive number of rows!")
    }
    if(!all(sapply(data, is.numeric))){
        stop("All columns of data must be numeric!")
    }
}

.find_initial_assignments = function(n, k, seed = 1337){
    set.seed(seed)
    sample(1:k, n, replace = TRUE)
}

.calculate_centroids = function(data, labels){
    centroids = sapply(data, function(column){
        tapply(column, as.factor(labels), mean)
    })
    data.frame(centroids)
}

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

.calculate_new_labels = function(distances){
    matrix_of_distances = do.call(cbind, distances)
    apply(matrix_of_distances, 1, which.min)
}

.create_output_object = function(results){
    output = list(labels = results$new_labels, 
                  iterations = results$iter,
                  centroids = results$roids, 
                  k = results$k,
                  data = results$data)
    class(output) = c("aml_k_means", class(output))
    output
}


#' AML k-means default plot function
#'
#' Plots a basic k-means scatterplot graph based on given results. Will plot a 
#' scatterplot of the first two principal components if data has more than 2 
#' columns.
#'
#' @param results_object 
#' @return NULL
#' @export
plot.aml_k_means = function(results_object, plot_it = TRUE){
    if(ncol(results_object$data) == 2){
        print(names(results_object$data))
        column_names = names(results_object$data)
        ggplot_object = ggplot(results_object$data, 
                               aes(x = .convert_to_ns(column_names[1]), 
                                   y = .convert_to_ns(column_names[2]))) + 
                            geom_point()
    }
    ggplot_object
}

.convert_to_ns = function(string){
    eval(parse(text = string))
}

