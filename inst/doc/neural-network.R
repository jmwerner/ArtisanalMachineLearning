## ----echo=FALSE----------------------------------------------------------
set.seed(1337)
library(rprojroot)

## ------------------------------------------------------------------------
library(ArtisanalMachineLearning)
data(iris)

## ------------------------------------------------------------------------
iris = iris[1:100,]
iris$Species = as.numeric(iris$Species) - 2
iris$Species[iris$Species == 0] = 1

## ------------------------------------------------------------------------
validation_set = rbind(iris[1:10,], iris[91:100,])
validation_set$Species = NULL
validation_response = c(iris$Species[1:10], iris$Species[91:100])

data = iris[11:90,]
data$Species = NULL
response = iris$Species[11:90]

## ------------------------------------------------------------------------
network = aml_neural_network(sizes=c(4, 15, 15, 4, 1),
                             learning_rate=.01, 
                             data=data, 
                             response=response, 
                             epochs=100, 
                             verbose=FALSE)

## ------------------------------------------------------------------------
preds = predict(network, data)
training_accuracy = sum((preds < 0) == (response < 0)) / length(preds)
print(paste("Training accuracy:", training_accuracy))

validation = predict(network, validation_set)
validation_accuracy = sum((validation < 0) == (validation_response < 0)) / length(validation)
print(paste("Validation accuracy:", validation_accuracy))

## ---- results="hide", warning=FALSE--------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

process_digit_data <- function(data_row){
    data_row %>%
        gather(pixel, value) %>%
        tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
        mutate(pixel = pixel - 2,
               x = pixel %% 28,
               y = 28 - pixel %/% 28)
}

plot_digit <- function(data_row){
    image_df = process_digit_data(data_row)
    image = ggplot(image_df, aes(x = x, y = y, fill = value)) + 
                geom_raster() + 
                coord_equal() +
                scale_fill_viridis()
    image
}

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", 
                      col_names = FALSE,
                      col_types = cols())

response = unlist(mnist_raw[,1])

data = mnist_raw
data[,1] = NULL

## ------------------------------------------------------------------------
digits_to_keep = c(0,7)
data_sub = data[response %in% digits_to_keep,]
response_sub = response[response %in% digits_to_keep]
response_transformed = sapply(response_sub, function(x){ifelse(x == 0, -1, 1)})

training_indicators = sample(1:length(response_transformed), 10000)
test_indicator = !(1:nrow(data_sub) %in% training_indicators)

training_data = data_sub[training_indicators,]
training_response = response_transformed[training_indicators]

test_data = data_sub[test_indicator,]

## ----echo=FALSE----------------------------------------------------------
set.seed(999999)

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_digit(training_data[sample(1:nrow(training_data), 1),])
plot_digit(training_data[sample(1:nrow(training_data), 1),])
plot_digit(training_data[sample(1:nrow(training_data), 1),])
plot_digit(training_data[sample(1:nrow(training_data), 1),])
plot_digit(training_data[sample(1:nrow(training_data), 1),])

## ---- fig.width = 7, fig.height = 5--------------------------------------
# Plot average 0 image
averages_frame = as.data.frame(t(apply((training_data[training_response == -1,]),MARGIN=2,FUN=mean)))
plot_digit(averages_frame)

# Plot average 7 image
averages_frame = as.data.frame(t(apply((training_data[training_response == 1,]),MARGIN=2,FUN=mean)))
plot_digit(averages_frame)

## ----eval = FALSE--------------------------------------------------------
#  small_network = aml_neural_network(c(784, 50, 25, 1), learning_rate = .01, data=training_data, response=training_response, epochs=10, verbose = FALSE)

## ----echo = FALSE--------------------------------------------------------
# Do a cooking show trick and bring out an already baked net
small_network = readRDS(file.path(find_root('DESCRIPTION'), 'data/small_net.rds'))

## ------------------------------------------------------------------------
# Training error
preds = predict(small_network, training_data)
training_error = sum((training_response < 0) == (preds < 0)) / length(preds)
print(paste("Training error:", training_error))

# Test error 
test_preds = predict(small_network, test_data)
testing_error = sum((response_transformed[test_indicator] < 0) == (test_preds < 0)) / length(test_preds)
print(paste("Testing error:", testing_error))

## ----eval = FALSE--------------------------------------------------------
#  big_network = aml_neural_network(c(784, 100, 100, 50, 25, 1), learning_rate = .01, data=training_data, response=training_response, epochs=100, verbose = FALSE)

## ----echo = FALSE--------------------------------------------------------
# Do a cooking show trick and bring out an already baked net
big_network = readRDS(file.path(find_root('DESCRIPTION'), 'data/big_net.rds'))

## ------------------------------------------------------------------------
# Training error
preds = predict(big_network, training_data)
training_error = sum((training_response < 0) == (preds < 0)) / length(preds)
print(paste("Training error:", training_error))

# Test error 
test_preds = predict(big_network, test_data)
testing_error = sum((response_transformed[test_indicator] < 0) == (test_preds < 0)) / length(test_preds)
print(paste("Testing error:", testing_error))

## ---- fig.width = 7, fig.height = 5--------------------------------------
median_obs = which(preds == median(preds)) # Many observations are equal to the median
plot_digit(training_data[sample(median_obs, 1),])
plot_digit(training_data[sample(median_obs, 1),])
plot_digit(training_data[sample(median_obs, 1),])
plot_digit(training_data[sample(median_obs, 1),])

## ---- fig.width = 7, fig.height = 5--------------------------------------
median_obs = which(test_preds == median(test_preds)) # Many observations are equal to the median
plot_digit(test_data[sample(median_obs, 1),])
plot_digit(test_data[sample(median_obs, 1),])
plot_digit(test_data[sample(median_obs, 1),])
plot_digit(test_data[sample(median_obs, 1),])

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_digit(training_data[which.min(preds),])

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_digit(training_data[which.max(preds),])

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_digit(test_data[which.min(test_preds),])

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_digit(test_data[which.max(test_preds),])

