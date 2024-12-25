# Load necessary libraries
library(readxl)
library(caret)
library(glmnet)
library(MASS)

# Load the data
data <- read_excel("C:/Users/haris/Downloads/regression_cancer.xlsx")

# Drop rows with NA values
data <- na.omit(df)
View(df)

plot_missing(df)
 # Define dependent and independent variables
X <- df[, c('avgAnnCount', 'TARGET_deathRate', 'incidenceRate', 'popEst2015', 'MedianAgeFemale')]
y <- df$avgDeathsPerYear

lambda <- 10^seq(10, -2, length = 100)
print(lambda)

# Split the dataset into training (70%) and testing (30%) sets
set.seed(567)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
Y_train <- y[trainIndex]
Y_test <- y[-trainIndex]

X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)

# Perform Ridge regression
ridge_reg <- glmnet(X_train_matrix, Y_train, alpha = 0, lambda = lambda)
summary(ridge_reg)

# Find the best lambda via cross-validation
ridge_reg1 <- cv.glmnet(X_train_matrix, Y_train, alpha = 0)
bestlam_ridge <- ridge_reg1$lambda.min
print(bestlam_ridge)

# Predict on the validation set
ridge_pred <- predict(ridge_reg, s = bestlam_ridge, newx = X_test_matrix)

# Calculate mean squared error for Ridge regression
mse_ridge <- mean((Y_test - ridge_pred)^2)
print(paste("Ridge Regression - Mean Squared Error:", mse_ridge))

# Calculate R² value for Ridge regression
sst <- sum((Y_test - mean(Y_test))^2)
sse_ridge <- sum((Y_test - ridge_pred)^2)
r2_ridge <- 1 - (sse_ridge / sst)
print(paste("Ridge Regression - R²:", r2_ridge))

# Get the Ridge regression coefficients
ridge_coef <- predict(ridge_reg, type = "coefficients", s = bestlam_ridge)
print("Ridge Coefficients:")
print(ridge_coef)

# Perform Lasso regression
lasso_reg <- glmnet(X_train_matrix, Y_train, alpha = 1, lambda = lambda)

# Find the best lambda via cross-validation
lasso_reg1 <- cv.glmnet(X_train_matrix, Y_train, alpha = 1)
bestlam_lasso <- lasso_reg1$lambda.min
print(bestlam_lasso)

# Predict on the validation set
lasso_pred <- predict(lasso_reg, s = bestlam_lasso, newx = X_test_matrix)

# Calculate mean squared error for Lasso regression
mse_lasso <- mean((Y_test - lasso_pred)^2)
print(paste("Lasso Regression - Mean Squared Error:", mse_lasso))

# Calculate R² value for Lasso regression
sse_lasso <- sum((Y_test - lasso_pred)^2)
r2_lasso <- 1 - (sse_lasso / sst)
print(paste("Lasso Regression - R²:", r2_lasso))

# Get the Lasso regression coefficients
lasso_coef <- predict(lasso_reg, type = "coefficients", s = bestlam_lasso)
print("Lasso Coefficients:")
print(lasso_coef)
