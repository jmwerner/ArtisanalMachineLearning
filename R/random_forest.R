#' AML Random Forest
#'
#' Calculates predictions via random forest algorithm
#'
#' @return Results object of class aml_random_forest
#' @export
aml_random_forest = function(){
    5
}

sum_of_squares = function(response_vector, prediction){
    sum((response_vector - prediction)^2)
}

.find_one_column_split = function(data, split_column_name, response, evaluation_criterion){
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

.find_one_split = function(data, response, evaluation_criterion){
    all_splits = lapply(names(data), function(split_column_name){
        .find_one_column_split(data, split_column_name, response, evaluation_criterion)
    }) 
    all_splits = do.call(rbind, all_splits)
    all_splits$split_column_name = as.character(all_splits$split_column_name)
    all_splits[which.min(all_splits$criterion_value),]    
}
