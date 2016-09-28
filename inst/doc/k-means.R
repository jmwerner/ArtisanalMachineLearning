## ------------------------------------------------------------------------
library(ArtisanalMachineLearning)
data(iris)

## ------------------------------------------------------------------------
data = iris[,2:3]
head(data)

## ---- fig.width = 7, fig.height = 5--------------------------------------
k_means = aml_k_means(data, k = 2, maximum_iterations = 0)
plot(k_means, plot_centroids = TRUE)

## ---- fig.width = 7, fig.height = 5--------------------------------------
k_means = aml_k_means(data, k = 2, maximum_iterations = 1)
plot(k_means, plot_centroids = TRUE)

## ---- fig.width = 7, fig.height = 5--------------------------------------
k_means = aml_k_means(data, k = 2)
plot(k_means, plot_centroids = TRUE)

## ------------------------------------------------------------------------
k_means$iterations

## ------------------------------------------------------------------------
data = iris[,1:4]
head(data)

## ---- fig.width = 7, fig.height = 5--------------------------------------
k_means = aml_k_means(data, k = 5)
plot(k_means, plot_centroids = TRUE)

## ---- fig.width = 7, fig.height = 5--------------------------------------
k_means = aml_k_means(data, k = 5)
plot(k_means, plot_centroids = TRUE, tsne_reduction = TRUE)

