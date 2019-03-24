## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
abalone_data_file = system.file("external_data", "abalone_data.RDS", package="ArtisanalMachineLearning", mustWork=TRUE)
abalone_data = readRDS(abalone_data_file)

## ------------------------------------------------------------------------
library(ArtisanalMachineLearning)

## ------------------------------------------------------------------------
dim(abalone_data$data)
summary(abalone_data$data)

## ---- echo=FALSE---------------------------------------------------------
ggplot(data=data.frame(response=abalone_data$response), aes(response)) + 
    geom_histogram(breaks=seq(0, 30, by = 1), 
                   col="grey", 
                   fill="blue") + 
    labs(x="", y="Count", title="Histogram of Response") + 
    theme_bw()

