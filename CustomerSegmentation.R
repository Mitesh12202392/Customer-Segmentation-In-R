install.packages("ggplot2")
install.packages("dplyr")
install.packages("cluster")
install.packages("factoextra")

library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

customer_data <- read.csv("C:/Users/mites/Downloads/Mall_Customers.csv")

View(customer_data)

head(customer_data)

sum(is.na(customer_data))

customer_data <- na.omit(customer_data)

customer_data <- customer_data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

customer_data <- scale(customer_data)

# Finding the optimal number of clusters using Elbow Method
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(customer_data, centers = i, nstart = 25)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot Elbow Method
plot(1:10, wcss, type="b", pch=19, col="blue", xlab="Number of Clusters", ylab="WCSS", 
     main="Elbow Method for Optimal K")

# Applying K-Means clustering with optimal K (assume K=5)
set.seed(123)
kmeans_result <- kmeans(customer_data, centers = 5, nstart = 25)

# Add cluster labels to original dataset
customer_data <- as.data.frame(customer_data)  # Convert matrix back to dataframe
customer_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualizing the Clusters
ggplot(customer_data, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Customer Segmentation using K-Means Clustering")

# Cluster Centroids Visualization
fviz_cluster(kmeans_result, data = customer_data[, c("Annual.Income..k..", "Spending.Score..1.100.")],
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal())


# Print final clustered dataset
print(customer_data)

