# Q1
# Set the file path
file_path <- "random_strings.txt"

# Generate 1000 random strings and save them to the file
random_strings <- replicate(1000, paste0(sample(c(letters, LETTERS, 0:9), 10, replace = TRUE), collapse = ""))
writeLines(random_strings, file_path)



# Q2
# Set the seed for reproducibility
set.seed(42)

# Generate a random dataset of 100 rows and 30 columns
dataset <- matrix(runif(100*30, min = 1, max = 200), nrow = 100)

# (i) Replace values with NA in the dataset between rows 10 and 60
dataset[10:60, ] <- NA

# Count the number of rows with missing values
rows_with_missing <- sum(rowSums(is.na(dataset)) > 0)
cat("Number of rows with missing values:", rows_with_missing, "\n")

# (ii) Replace NA values with column averages
dataset <- apply(dataset, 2, function(x) {
  na_index <- is.na(x)
  x[na_index] <- mean(x, na.rm = TRUE)
  x
})

# (iii) Calculate Pearson correlation and plot a heat map
correlation <- cor(dataset)
heatmap(correlation)

# Select columns with correlation <= 0.7
cor_threshold <- 0.7
low_correlation_columns <- colnames(dataset)[apply(correlation, 2, function(x) any(x <= cor_threshold))]
cat("Columns with correlation <= 0.7:", paste(low_correlation_columns, collapse = ", "), "\n")

# (iv) Normalize values between 0 and 10
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 10
}
dataset <- apply(dataset, 2, normalize)

# (v) Replace values with 1 if <= 0.5, else with 0
dataset <- ifelse(dataset <= 0.5, 1, 0)



# Q3
# Set the seed for reproducibility
set.seed(42)

# Generate a random dataset of 500 rows and 10 columns
dataset <- matrix(NA, nrow = 500, ncol = 10)

# Column 1 to 4: [-10, 10]
dataset[, 1:4] <- matrix(runif(500*4, min = -10, max = 10), nrow = 500)

# Column 5 to 8: [10, 20]
dataset[, 5:8] <- matrix(runif(500*4, min = 10, max = 20), nrow = 500)

# Column 9 to 10: [-100, 100]
dataset[, 9:10] <- matrix(runif(500*2, min = -100, max = 100), nrow = 500)

# (i) K-Means Clustering

# Determine the optimal number of clusters using the elbow method
wss <- numeric(10)
for (k in 1:10) {
  kmeans_model <- kmeans(dataset, centers = k, nstart = 10)
  wss[k] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")

# Choose the optimal number of clusters based on the plot
optimal_k <- 3  # You can modify this based on the elbow point in the plot

# Perform K-Means clustering with the optimal number of clusters
kmeans_model <- kmeans(dataset, centers = optimal_k, nstart = 10)

# Plot distance metric graph
plot(kmeans_model$centers, type = "p", pch = 19, col = 1:optimal_k, main = "K-Means Clustering")

# (ii) Hierarchical Clustering

# Calculate the distance matrix
distance_matrix <- dist(dataset)

# Perform Hierarchical clustering
hierarchical_model <- hclust(distance_matrix, method = "ward.D2")

# Determine the optimal number of clusters using the dendrogram
plot(hierarchical_model, hang = -1, main = "Hierarchical Clustering")

# Choose the optimal number of clusters based on the dendrogram
optimal_clusters <- 4  # You can modify this based on the dendrogram structure

# Cut the dendrogram to obtain clusters
hierarchical_clusters <- cutree(hierarchical_model, k = optimal_clusters)

# Plot distance metric graph
plot(distance_matrix, main = "Hierarchical Clustering Distance Metric")



# Q4
# Set the seed for reproducibility
set.seed(42)

# Generate a random dataset of 600 rows and 15 columns
dataset <- matrix(runif(600*15, min = -100, max = 100), nrow = 600)

# (i) Plot scatter graph between Column 5 and Column 6
column5 <- dataset[, 5]
column6 <- dataset[, 6]
plot(column5, column6, xlab = "Column 5", ylab = "Column 6", main = "Scatter Plot")

# (ii) Plot histogram of each column in a single graph
par(mfrow = c(3, 5))  # Set the layout to 3 rows and 5 columns
par(mar = c(3, 3, 1, 1))
for (i in 1:15) {
  hist(dataset[, i], main = paste("Column", i), xlab = "Value", col = "lightblue", border = "white")
}

# (iii) Plot the Box plot of each column in a single graph
par(mfrow = c(3, 5))  # Set the layout to 3 rows and 5 columns
par(mar = c(3, 3, 1, 1))
for (i in 1:15) {
  boxplot(dataset[, i], main = paste("Column", i), ylab = "Value", col = "lightblue", border = "black")
}



# Q5
# Set the seed for reproducibility
set.seed(42)

# Generate a random dataset of 500 rows and 5 columns
dataset <- matrix(runif(500*5, min = 5, max = 10), nrow = 500)

# (i) Perform t-Test on each column
t_test_results <- lapply(1:5, function(i) {
  t.test(dataset[, i])
})

# (ii) Perform Wilcoxon Signed Rank Test on each column
wilcoxon_test_results <- lapply(1:5, function(i) {
  wilcox.test(dataset[, i])
})

# (iii) Perform Two Sample t-Test and Wilcoxon Rank Sum Test on Column 3 and Column 4
column3 <- dataset[, 3]
column4 <- dataset[, 4]

# Two Sample t-Test
ttest_2sample <- t.test(column3, column4)

# Wilcoxon Rank Sum Test
wilcox_2sample <- wilcox.test(column3, column4)

# Print the results
cat("T-Test Results:\n")
for (i in 1:5) {
  cat("Column", i, ": p-value =", t_test_results[[i]]$p.value, "\n")
}

cat("\nWilcoxon Signed Rank Test Results:\n")
for (i in 1:5) {
  cat("Column", i, ": p-value =", wilcoxon_test_results[[i]]$p.value, "\n")
}

cat("\nTwo Sample t-Test Results for Column 3 and Column 4:\n")
cat("p-value =", ttest_2sample$p.value, "\n")

cat("\nWilcoxon Rank Sum Test Results for Column 3 and Column 4:\n")
cat("p-value =", wilcox_2sample$p.value, "\n")

