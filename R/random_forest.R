#' AML Random Forest
#'
#' Calculates predictions via random forest algorithm
#'
#' @param data Input data.frame of dimension n x p for training the random forest
#' @param response Response vector of size nx1 corresponding to the training
#' data
#' @param b Number of bootstrap iterations to perform (trees to build)
#' @param m Number of columns to randomly use at each splitting iteration
#' @param evaluation_criterion Function that calculates error criterion for
#' fitting, defaults to sum of squares
#' @param min_obs Minimum observations allowed to end up in a single node,
#' defaults to 5
#' @param max_depth Maximum number of successive splits allowed to happen
#' in the tree, defaults to 8
#' @return Results trained list of class aml_random_forest filled with random forest trees
#' @export
aml_random_forest <- function(data, response, b, m, evaluation_criterion = sum_of_squares, min_obs = 5, max_depth = 8){    
    bootstrap_trees = lapply(1:b, function(i){
        bootstrapped_data = .create_single_bootstrapped_data_frame(data)
        sampled_columns = sample(names(data), m)
        create_tree(bootstrapped_data[,sampled_columns], response, evaluation_criterion, min_obs, max_depth)
    })
}

#' AML CART
#'
#' Produces a single decision tree.
#'
#' @param data Input data.frame dimension n x p for training decision tree
#' @param response Response vector of size nx1 corresponding to the training
#' data
#' @param evaluation_criterion Function that calculates error criterion for
#' fitting, defaults to sum of squares
#' @param min_obs Minimum observations allowed to end up in a single node,
#' defaults to 5
#' @param max_depth Maximum number of successive splits allowed to happen
#' in the tree, defaults to 8
#' @return Single decision tree in list format of class aml_tree
#' @export
create_tree = function(data, response, evaluation_criterion = sum_of_squares, min_obs = 5, max_depth = 8){
    first_split = .find_one_split(data, response, evaluation_criterion)

    tree = .build_tree(data = data, 
                       response = response, 
                       split = first_split, 
                       max_depth = max_depth,
                       evaluation_criterion = evaluation_criterion, 
                       min_obs = min_obs)
    tree = .prepend_class(tree, "aml_tree")
}

#' AML single tree predict method
#'
#' Returns prediction for a single tree when given a row of data.
#'
#' @param tree Single fitted tree returned by the `create_tree` function
#' @param data Data.frame of size `1 x p` for prediction
#' @return Prediction value
#' @export
predict.aml_tree <- function(tree, data){
    if(!is.data.frame(data)){
        stop("ERROR: data must be a data.frame")
    }
    if(nrow(data) != 1){
        stop("ERROR: data must be a data.frame of dimension 1 x p")
    }
    while(!"prediction" %in% names(tree)){
        if(!tree$split_column %in% names(data)){
            stop("ERROR: Split column not provided in prediction data row")
        }
        if(data[[tree$split_column]] < tree$split_value){
            tree = tree$left
        }else{
            tree = tree$right
        }
    }
    return(tree$prediction)
}

sum_of_squares <- function(response_vector, prediction){
    sum((response_vector - prediction)^2)
}

.create_single_bootstrapped_data_frame <- function(data){
    row_indicators = sample(1:nrow(data), nrow(data), replace = TRUE)
    return(data[row_indicators,])
}

.find_one_column_split <- function(data, split_column_name, response, evaluation_criterion){
    # Get the midpoints between each unique value
    split_values = sort(unique(data[[split_column_name]]))
    distance_to_midpoint = diff(split_values) / 2
    split_values = split_values[-length(split_values)]
    split_values = split_values + distance_to_midpoint

    error_values = sapply(split_values, function(split_value){
        indicators = data[[split_column_name]] < split_value
        response_left = response[indicators]
        response_right = response[!indicators]
        evaluation_criterion(response_left, mean(response_left)) + evaluation_criterion(response_right, mean(response_right))
    })

    data.frame("split_column_name" = split_column_name,
               "criterion_value" = min(error_values),
               "split_value" = split_values[which.min(error_values)],
               stringsAsFactors = FALSE)
}

.find_one_split <- function(data, response, evaluation_criterion){
    all_splits = lapply(names(data), function(split_column_name){
        .find_one_column_split(data, split_column_name, response, evaluation_criterion)
    }) 
    all_splits = do.call(rbind, all_splits)
    all_splits$split_column_name = as.character(all_splits$split_column_name)
    all_splits[which.min(all_splits$criterion_value),]    
}

.build_tree <- function(data, response, split, max_depth, evaluation_criterion, min_obs, count = 1){
    indicators = data[[split$split_column_name]] < split$split_value
    if(count == max_depth | (sum(indicators) <= min_obs) | (sum(!indicators) <= min_obs)){
        return(data.frame(split,
                          prediction = mean(response), 
                          count = length(response),
                          stringsAsFactors = FALSE))
    }else{
        left_split = .find_one_split(data[indicators,], response[indicators], evaluation_criterion)
        right_split = .find_one_split(data[!indicators,], response[!indicators], evaluation_criterion)
        return(list(split_column = split$split_column_name, 
                    criterion_value = split$criterion_value,
                    split_value = split$split_value, 
                    left = .build_tree(data = data[indicators,], 
                                       response = response[indicators], 
                                       split = left_split, 
                                       max_depth = max_depth,
                                       evaluation_criterion = evaluation_criterion,
                                       min_obs = min_obs, 
                                       count = count + 1), 
                    right = .build_tree(data = data[!indicators,], 
                                        response = response[!indicators], 
                                        split = right_split, 
                                        max_depth = max_depth,
                                        evaluation_criterion = evaluation_criterion,
                                        min_obs = min_obs,
                                        count = count + 1)))
    }
}

