library(dplyr)
library(cluster)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)
library(data.table)
library(ggbiplot)
library(tidyr)
library(caret)
library(tidyverse)

dataset <- read.csv("CC GENERAL.CSV")
data <- dataset
#Data Pre-Processing

# 1. Drop the customer ID column
data <- data[c(-1)]

# 2. Dealing with null values
sum(is.na(data)) # 314 null values
summary(data) # Credit limit = 1, Minimum Payments = 313 

data$MINIMUM_PAYMENTS[is.na(data$MINIMUM_PAYMENTS)]<- 0
data = na.omit(data)


# 3. Scaling the data
data <- scale(data)
data <- as.data.frame(data)


# 4. Dealing with dependent variables using correlation
data_cr = cor(data)
data <- data[c(-4)]


# 5. Dealing with Outliers

library(reshape)
meltData <- melt(data)
outlier_plot <- ggplot(meltData, aes(factor(variable), value, col = factor(variable))) 
outlier_plot + geom_boxplot() + facet_wrap(~variable, scale="free")

final_data <- data

capping_outliers <- function(x){
  outliers <- quantile(x, c(.10, .90))
  x[x<outliers[1] ] <- outliers[1]
  x[x>outliers[2] ] <- outliers[2]
  return(x)
}
for(i in 1:ncol(data)){
  final_data[,i] <- capping_outliers(final_data[,i])
}

melt_finData <- melt(final_data)
outlier_plot_fin <- ggplot(melt_finData, aes(factor(variable), value,col = factor(variable)))
dev.off()
outlier_plot_fin + geom_boxplot() + facet_wrap(~variable, scale="free")



boxplot(final_data)$out
boxplot(final_data$BALANCE,
        col="#ff0066",
        main="Boxplot for Descriptive Analysis of Balance")

#KMEANS CLUSTERING [ELBOW, GAP STATISTICS, SILHOUTTE - Optimal Clusters]

data <- final_data
library(factoextra)
library(cluster)

# Elbow method   - Method 1
fviz_nbclust(x = data,
             FUNcluster = kmeans,
             method = "wss", 
             k.max = 10, 
             diss = get_dist(data, method = "euclidean"), # Optimal = 5
             nstart = 50)

#Gap statistics   - Method 2
gap_stat <- clusGap(data,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 15,
                    B = 20)

fviz_gap_stat(gap_stat)   # Cannot decide using gap statistics Optimal = 7, 10, 15
#plot number of clusters vs. gap statistic


# Silhouette method   - Method 3
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = data, k = k)
  model$silinfo$avg.width
})


sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)             # Generate a data frame containing both k and sil_width

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2:10)         # Optimal clusters = 2, 4, 5


# PCA

pca = prcomp(data,
             center = TRUE,scale=TRUE)

ggbiplot(pca, scale = 0, labels=rownames(pca$x), color = TRUE) + geom_point( size = 0.5) + ggtitle("PCA Plot") + theme(plot.title = element_text(hjust = 0.5))

pr.var <- pca$sdev^2
pve <- pr.var / sum(pr.var)
Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

pr_out <- pca
pca_transform = as.data.frame(-pr_out$x[,1:2])

# MODEL IMPLEMENTATION - OPTIMAL CLUSTERS = 5
set.seed(123)
km <- kmeans(pca_transform, centers = 5, nstart = 50)
km
fviz_cluster(km, data = pca_transform)  # plotting
aggregate(data, by=list(cluster=km$cluster), mean) # Mean value of each cluster

final_data <- cbind(pca_transform, cluster 
                    = km$cluster)  # Final k means model dataframe


#HIERARCHICAL CLUSTERING

dist_matrix_scale <- dist(pca_transform, method = "euclidean")
hclust_df_scale <- hclust(dist_matrix_scale, method = "average")
hclust_df_scale

plot(hclust_df_scale,
     main = "Dendrogram",
     xlab = "States",
     ylab = "Euclidean Distances")

#CUT THE TREE WITH CLUSTERS = 5
groups_scale <- cutree(hclust_df_scale, k=5)
rect.hclust(hclust_df_scale, k = 5, border = 2:8)

#FIND THE NUMBER OF OBSERVATIONS IN EACH CLUSTER
c_scale <- data.frame(table(groups_scale))
c_scale


#TO FIND WHICH STATES BELONGS TO WHICH GROUP - VISUALIZE
fviz_cluster(list(data = pca_transform, cluster = groups_scale))



#DBSCAN

#install.packages("fpc")
#install.packages("dbscan")


# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(data, eps = 0.15, MinPts = 5)
plot(db, pca_transform, main = "DBScan")

library("factoextra")
fviz_cluster(db, data = data, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

print(db)

#SPECTRAL CLUSTERING