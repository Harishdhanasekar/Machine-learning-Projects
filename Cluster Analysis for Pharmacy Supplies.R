# CIA 1 - APPLICATIONS OF CLUSTERING

################ K-MEANS #################################################################################

library(cluster)
library(factoextra)
library(dplyr)

data <- CIA.1
data_num <- data[sapply(data, is.numeric)]

print(sum(is.na(data_num)))  # Total number of NAs
print(colSums(is.na(data_num)))  # Missing values by column

# Remove rows with missing values (NA)
data_num_clean <- na.omit(data_num)  # This removes all rows with any NA

# Scale the numerical data
data_scaled <- scale(data_num_clean)

### K-Means Clustering
set.seed(123)

# Perform K-Means clustering
kmeans_result <- kmeans(data_scaled, centers = 3)

# Print cluster assignments
print(kmeans_result$cluster)

# Visualize K-Means clusters
fviz_cluster(kmeans_result, data = data_scaled, geom = "point", stand = FALSE, ellipse.type = "norm")

library(psych)

# Perform PCA on the scaled numerical data
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Visualize the variance explained by each principal component
fviz_eig(pca_result)

# Display the loadings (importance of each variable)
print(pca_result$rotation)

# Visualize the contributions of variables to the first two principal components
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

target_variable <- data_num_clean$Tot_Suplrs # Replace 'Income' with your specific target variable

# After clustering, check how clusters differ based on the target variable
cluster_vs_target <- data.frame(Cluster = kmeans_result$cluster, Target = target_variable)

# Summarize the relationship between clusters and the target variable
aggregate(Target ~ Cluster, data = cluster_vs_target, FUN = mean)


############### HIERARCHIAL CLUSTERING ###################################

library(ggplot2)
library(cluster)
library(factoextra)

# Remove non-numeric columns
data_numeric <- data[sapply(data, is.numeric)]  # Keep only numeric columns

# Remove rows with missing values (NA)
data_numeric_clean <- na.omit(data_numeric)  # This removes all rows with any NA

# Step 1: Scale the data
data_scaled <- scale(data_numeric_clean)

# Step 2: Calculate distance matrix using the 'stats' package
dist_matrix <- stats::dist(data_scaled, method = "euclidean")  # Specify the stats package

# Step 3: Perform hierarchical clustering using different methods

# Single linkage using stats package
hc_single <- stats::hclust(dist_matrix, method = "single")

# Complete linkage using stats package
hc_complete <- stats::hclust(dist_matrix, method = "complete")

# Average linkage using stats package
hc_average <- stats::hclust(dist_matrix, method = "average")

# Centroid linkage using stats package
hc_centroid <- stats::hclust(dist_matrix, method = "centroid")

# Ward's linkage using stats package
hc_ward <- stats::hclust(dist_matrix, method = "ward.D2")

# Plot Single linkage
plot(hc_single, main = "Single Linkage", xlab = "", sub = "", hang = -1)

# Plot Complete linkage
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", hang = -1)

# Plot Average linkage
plot(hc_average, main = "Average Linkage", xlab = "", sub = "", hang = -1)

# Plot Centroid linkage
plot(hc_centroid, main = "Centroid Linkage", xlab = "", sub = "", hang = -1)

# Plot Ward's linkage
plot(hc_ward, main = "Ward's Linkage", xlab = "", sub = "", hang = -1)

# Reset plot layout
par(mfrow=c(1, 1))

# Step 5: Cut the dendrogram into clusters (e.g., 3 clusters)
clusters_single <- cutree(hc_single, k = 3)
clusters_complete <- cutree(hc_complete, k = 3)
clusters_average <- cutree(hc_average, k = 3)
clusters_centroid <- cutree(hc_centroid, k = 3)
clusters_ward <- cutree(hc_ward, k = 3)

# Step 6: Visualize clusters for Ward's method
# Create a data frame for plotting
data_clusters <- data.frame(data_numeric_clean, cluster = clusters_ward)

# Scatter plot of clusters (using first two dimensions)
ggplot(data_clusters, aes(x = data_clusters[,1], y = data_clusters[,2], color = as.factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Ward's Hierarchical Clustering", x = "Dimension 1", y = "Dimension 2", color = "Cluster") +
  theme_minimal()


############### DBSCAN ################################################################

library(dbscan)
library(ggplot2)

# Select the relevant numerical columns for DBSCAN clustering
numerical_data <- data[, c( 
  "Avg_Suplr_Sbmtd_Chrg", "Avg_Suplr_Mdcr_Alowd_Amt",
  "Avg_Suplr_Mdcr_Pymt_Amt", "Avg_Suplr_Mdcr_Stdzd_Amt")]

# Standardize the data
numerical_data_scaled <- scale(numerical_data)

# Perform k-NN distance plot to determine appropriate epsilon
kNNdistplot(numerical_data_scaled, k = 3)
abline(h = 0.5, col = "red")  # Adjust this based on where the elbow is

# Set parameters for DBSCAN: epsilon and minPts
eps <- 0.5   # Adjust this based on the elbow point in the k-NN plot
minPts <- 4 # Adjust based on domain knowledge

# Run DBSCAN clustering
set.seed(1234)
db <- dbscan(numerical_data_scaled, eps = eps, minPts = minPts)

# View clustering results
print(db)

# Plot the clustering results (optional)
fviz_cluster(db, numerical_data_scaled, stand = FALSE, ellipse = TRUE, geom = "point", show.clust.cent = FALSE) +
  ggtitle("DBSCAN Clustering")






################### TSNE ####################################################################

library(Rtsne)
library(ggplot2)

sp <- CIA.1

# Remove non-numeric columns for t-SNE (assuming first column is non-numeric, adjust if needed)
sp_mat <- as.matrix(sp[ , sapply(sp, is.numeric)]) # Keep only numeric columns

# Remove rows with missing values
sp_clean <- na.omit(sp_mat)

# Remove duplicate rows
sp_clean <- sp_clean[!duplicated(sp_clean), ]

# Fit t-SNE after removing duplicates
sptsne <- Rtsne(sp_clean, perplexity = 30, max_iter = 2000)

# Convert matrix back to dataframe
sptsne_df <- data.frame(X1 = sptsne$Y[,1], X2 = sptsne$Y[,2])


# Perform k-means clustering on the t-SNE results
set.seed(42)  # for reproducibility
kmeans_result <- kmeans(sptsne_df, centers = 2)  # Choose number of clusters 

# Add cluster assignments to the t-SNE dataframe
sptsne_df$cluster <- factor(kmeans_result$cluster)

# Visualize t-SNE results with clusters
ggplot(sptsne_df) + 
  geom_point(aes(x = X1, y = X2, color = cluster)) + 
  ggtitle("t-SNE Clustering Visualization") +
  theme_minimal()



#### LEADERBOARD #########################################

library(cluster)
library(factoextra)
library(dplyr)

# K-Means clustering
kmeans_result <- kmeans(data_scaled, centers = 3)  # Perform K-Means clustering again if needed

# Hierarchical clustering
hc_single <- hclust(dist(data_scaled), method = "single")
hc_complete <- hclust(dist(data_scaled), method = "complete")
hc_average <- hclust(dist(data_scaled), method = "average")
hc_centroid <- hclust(dist(data_scaled), method = "centroid")
hc_ward <- hclust(dist(data_scaled), method = "ward.D2")

# Create a function to compute silhouette scores
compute_silhouette_score <- function(clustering_result, dist_matrix) {
  # Ensure the clustering result is a factor or numeric vector
  silhouette_score <- silhouette(as.numeric(clustering_result), dist_matrix)
  mean(silhouette_score[, 3])  # Return the average silhouette width
}

# Create a distance matrix for the scaled data
dist_matrix <- dist(data_scaled)

# Calculate silhouette scores for each model
silhouette_scores <- data.frame(
  S.No. = 1:7,  # Number of models, adjusted to 7 to include DBSCAN
  Model = c("K-Means", 
            "Hierarchical Single", 
            "Hierarchical Complete", 
            "Hierarchical Average", 
            "Hierarchical Centroid", 
            "Hierarchical Ward", 
            "DBSCAN"),
  Silhouette_Score = c(
    compute_silhouette_score(kmeans_result$cluster, dist_matrix),  # K-Means
    compute_silhouette_score(cutree(hc_single, k = 3), dist_matrix),  # Single Linkage
    compute_silhouette_score(cutree(hc_complete, k = 3), dist_matrix),  # Complete Linkage
    compute_silhouette_score(cutree(hc_average, k = 3), dist_matrix),  # Average Linkage
    compute_silhouette_score(cutree(hc_centroid, k = 3), dist_matrix),  # Centroid Linkage
    compute_silhouette_score(cutree(hc_ward, k = 3), dist_matrix),  # Ward's Linkage
    # Calculate for DBSCAN, excluding noise points (cluster 0)
    mean(silhouette(as.numeric(db$cluster[db$cluster != 0]), dist(data_scaled[db$cluster != 0, ]))[, 3])
  )
)

# Print the leaderboard
print(silhouette_scores)

