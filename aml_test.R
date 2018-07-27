# LOAD ALL AML FIRST


data(iris)

# Limit iris to a 2 response data set (so we can have a binary response)
iris = iris[1:100,]
# Because of the nature of the tanh function, values should be in the [-1, 1] interval
iris$Species = as.numeric(iris$Species) - 2
iris$Species[iris$Species == 0] = 1

validation_set = rbind(iris[1:10,], iris[91:100,])
validation_set$Species = NULL
validation_response = c(iris$Species[1:10], iris$Species[91:100])


data = iris[11:90,]
data$Species = NULL
response = iris$Species[11:90]

# Train a network
network = aml_neural_network(c(4, 15, 15, 4, 1), learning_rate = .01, data=data, response=response, epochs=100, verbose = TRUE)

preds = .calculate_prediction(network, data)

(preds < 0) == (response < 0)


validation = .calculate_prediction(network, validation_set)
validation

(validation < 0) == (validation_response < 0)


# MNIST

# Downloaded from "https://pjreddie.com/media/files/mnist_train.csv"

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

mnist_raw <- read_csv("~/Downloads/mnist_train.csv", col_names = FALSE)

response = unlist(mnist_raw[,1])

data = mnist_raw
data[,1] = NULL



# From an r bloggers tutorial: https://www.r-bloggers.com/exploring-handwritten-digit-classification-a-tidy-analysis-of-the-mnist-dataset/
pixels_gathered <- mnist_raw %>%
  head(10000) %>%
  rename(label = X1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)

pixels_gathered


pixels_gathered %>%
  filter(instance <= 12) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ instance + label)





# Differentiate between 0 and 7
digits_to_keep = c(0,7)
data_sub = data[response %in% digits_to_keep,]
response_sub = response[response %in% digits_to_keep]

response_transformed = sapply(response_sub, function(x){ifelse(x == 0, -1, 1)})

network = aml_neural_network(c(784, 50, 10, 1), learning_rate = .01, data=data_sub, response=response_transformed, epochs=10, verbose = TRUE)

preds = .calculate_prediction(network, data_sub)


sum((response_transformed < 0) == (preds < 0)) / length(preds)
# .888 training set accuracy with small neural net (!!!!)


