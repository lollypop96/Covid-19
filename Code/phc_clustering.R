library(ggplot2)
library(plyr)
library(dplyr)
library(tibble)
library(nvmix)
library(readxl)
library(pracma)
library(fit.models)
library(sqldf)
library(writexl)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readr)
library(data.table)
library(dygraphs)
library(xts)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(forecast)
library(bnlearn)
library(R.utils)
library(RCurl)
library(latticeExtra)
library(ggeasy)
require(reshape2)
library(TSstudio)
library(ggeasy)
library(TTR)

#############################################################################
          #### CLUSTER ANALYSIS on italian regions ####
regions <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Area Regions full sum up", col_names = TRUE)

## First clustering
# We remove the State column
regions <- regions[,-c(2)]

# We start by scaling/standardizing the data
regions<- data.frame(regions)
rownames(regions) <- regions$Area.Region
regions<- regions[,-c(1)]

# We cannot consider in our analysis neither Rio grande do sul nor Eastern Cape
# because of lack of data on these two regions
# Remove na-containing rows
regions1 <- na.omit(regions)
regions1 <- scale(regions1)

distance <- get_dist(regions1)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

### Hierarchical clustering ###
## Ward Hierarchical Clustering
d <- dist(regions1, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")

## Ward Hierarchical Clustering with Bootstrapped p values
# Clusters that are highly supported by the data
# will have large p values.
library(pvclust)
fit <- pvclust(regions1, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

### K-means algorithm ###
## It is one of the mosto commonly used unsupervised
# machine learning algorithms for partitioning a
# given data set into a set of k clusters.
kmeans <- kmeans(regions1, centers = 4)
str(kmeans)
fviz_cluster(kmeans, data = regions1)