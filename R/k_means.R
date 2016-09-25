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
    roids = .calculate_centroids(data, labels)
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
    output = .prepend_class(output, "aml_k_means")
    output
}

.prepend_class = function(input, class_to_add){
    class(input) = c(class_to_add, class(input))
    input
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
plot.aml_k_means = function(results_object, plot_centroids = FALSE){
    ggplot_object = .make_ggplot_object(results_object, plot_centroids)
    plot(ggplot_object)
}

.make_ggplot_object = function(object, plot_centroids){
    UseMethod(".make_ggplot_object", object)
}

.make_ggplot_object.aml_k_means = function(results_object, plot_centroids = FALSE){
    if(ncol(results_object$data) == 2){
        column_names = names(results_object$data)
        plotting_data = data.frame(results_object$data, 
                                   Labels = factor(results_object$labels))
    }else{
        column_names = c("Principal Component I",
                         "Principal Component II")
        prcomp_object = prcomp(results_object$data, center = TRUE, scale = TRUE)
        plotting_data = data.frame(prcomp_object$x[, 1:2], 
                                   factor(results_object$labels))
        names(plotting_data) = c(column_names, "Labels")
    }
    
    ggplot_object = ggplot(plotting_data, 
                        aes_string(x = .convert_to_ggname(column_names[1]), 
                                   y = .convert_to_ggname(column_names[2]),
                                   col = "Labels",
                                   shape = "Labels")) + 
                        geom_point(aes(shape = Labels), 
                                   size = 2.1, 
                                   col = "grey") +
                        geom_point() +
                        scale_color_viridis(discrete = TRUE) 

    if(plot_centroids){
        centroids = aggregate(as.matrix(plotting_data[, column_names]) ~ Labels, 
                              plotting_data, 
                              mean)
        ggplot_object = ggplot_object + 
                            geom_point(data = centroids, 
                                       size = 6, 
                                       col = "grey") + 
                            geom_point(data = centroids, size = 5)

    }

    ggplot_object
}

.convert_to_ggname = function(x){
    if(class(x) != "character") {
        return(x)
    }
    y = sapply(x, function(s){
        if (!grepl("^`", s)){
            s = paste("`", s, sep="", collapse="")
        }
        if (!grepl("`$", s)){
            s = paste(s, "`", sep="", collapse="")
        }
    })
    y 
}

