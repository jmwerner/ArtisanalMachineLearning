## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
library(rprojroot)
abalone_data_file = system.file("external_data", "abalone_data.RDS", package="ArtisanalMachineLearning", mustWork=TRUE)
abalone_data = readRDS(abalone_data_file)
set.seed(1337)

## ------------------------------------------------------------------------
library(ArtisanalMachineLearning)

## ------------------------------------------------------------------------
dim(abalone_data$data)
summary(abalone_data$data)

## ---- echo=FALSE, fig.width = 7, fig.height = 5--------------------------
ggplot(data=data.frame(response=abalone_data$response), aes(response)) + 
    geom_histogram(breaks=seq(0, 30, by = 1), 
                   col="grey", 
                   fill="blue") + 
    labs(x="", y="Count", title="Histogram of Response") + 
    theme_bw()

## ---- eval=FALSE---------------------------------------------------------
#  random_forest = aml_random_forest(data=abalone_data$data, response=abalone_data$response, b=200, m=6, evaluation_criterion=sum_of_squares, min_obs=5, max_depth=16, verbose=FALSE)

## ----echo = FALSE--------------------------------------------------------
# Do a cooking show trick and bring out an already baked rf
random_forest = readRDS(file.path(find_root('DESCRIPTION'), 'data/random_forest.RDS'))

## ------------------------------------------------------------------------
random_forest_predictions = predict_all(random_forest, abalone_data$data, n_trees=200)

## ------------------------------------------------------------------------
sum((abalone_data$response - random_forest_predictions)^2) / length(abalone_data$response)

## ---- echo=FALSE, fig.width=7, fig.height=5------------------------------
plotting_data_rf = data.frame(predicted=random_forest_predictions, 
                               actual=abalone_data$response, 
                               Difference=abs(random_forest_predictions - abalone_data$response))

ggplot(plotting_data_rf, aes(x=actual, y=predicted, color=Difference)) +
    geom_point() + 
    scale_y_continuous(limits = c(-1, 29)) + 
    geom_jitter() + 
    scale_color_viridis() + 
    geom_abline(intercept = 0, slope = 1, color="black", size=1.5) + 
    labs(x="Actual", y="Predicted", title="Actual vs Predicted") + 
    theme_bw()

## ---- eval=FALSE---------------------------------------------------------
#  gbm = aml_gbm(abalone_data$data, abalone_data$response, learning_rate=.1, n_trees=50, evaluation_criterion=sum_of_squares, min_obs=10, max_depth=4, verbose=FALSE)

## ----echo = FALSE--------------------------------------------------------
# Do a cooking show trick and bring out an already baked rf
gbm = readRDS(file.path(find_root('DESCRIPTION'), 'data/gbm.RDS'))

## ------------------------------------------------------------------------
gbm_predictions = predict_all(gbm, abalone_data$data, n_trees=50)

## ------------------------------------------------------------------------
sum((abalone_data$response - gbm_predictions)^2) / length(abalone_data$response)

## ---- echo=FALSE, fig.width=7, fig.height=5------------------------------

plotting_data_gbm = data.frame(predicted=gbm_predictions, 
                               actual=abalone_data$response, 
                               Difference=abs(gbm_predictions - abalone_data$response))

ggplot(plotting_data_gbm, aes(x=actual, y=predicted, color=Difference)) +
    geom_point() + 
    scale_y_continuous(limits = c(-1, 29)) + 
    geom_jitter() + 
    scale_color_viridis() + 
    geom_abline(intercept = 0, slope = 1, color="black", size=1.5) + 
    labs(x="Actual", y="Predicted", title="Actual vs Predicted") + 
    theme_bw()

