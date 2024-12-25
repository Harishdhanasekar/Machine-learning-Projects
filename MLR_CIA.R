# Load necessary libraries
library(readxl)
library(psych)
library(ggplot2)
library(DataExplorer)
library(car) # scatterplot, vif
library(lmtest) # autocorrelation
library(Metrics) # loss/cost functions
library(MASS) # stepAIC

# Load the data
df <- read_excel("C:/Users/haris/Downloads/regression_cancer.xlsx")

# Define dependent and independent variables
X <- df[, c('avgAnnCount', 'TARGET_deathRate', 'incidenceRate', 'popEst2015', 'MedianAgeFemale')]
y <- df$avgDeathsPerYear

# Ensure all independent variables are numeric
X <- as.data.frame(lapply(X, as.numeric))

# Ensure the dependent variable is numeric
y <- as.numeric(y)

# Combine the independent and dependent variables into a single dataframe
data <- cbind(X, avgDeathsPerYear = y)

# Perform EDA
str(data)
summary(data)
plot_missing(data)

# Visualizations
pairs.panels(data)
plot_histogram(data)
plot_density(data)
plot_correlation(data)

# Split the dataset into training (70%) and testing (30%) sets
set.seed(1234)
trainIndex <- createDataPartition(data$avgDeathsPerYear, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Build a full model
fullmodel <- lm(avgDeathsPerYear ~ ., data = trainData)
fullmodel_summary <- summary(fullmodel)
print(fullmodel_summary)

# Build a simple linear regression model with only popest2015
slm_popEst2015  <- lm(avgDeathsPerYear ~ popEst2015, data = trainData)
slm_popEst2015_summary <- summary(slm_popEst2015)
print(slm_popEst2015_summary)

# Predict and evaluate on test data
fullmodel_pred <- predict(fullmodel, newdata = testData)
slm_popEst2015_pred <- predict(slm_popEst2015 , newdata = testData)

# Calculate R-squared values for training and testing data
fullmodel_train_r2 <- fullmodel_summary$r.squared
fullmodel_test_r2 <- cor(testData$avgDeathsPerYear, fullmodel_pred)^2

slm_popEst2015_train_r2 <- slm_popEst2015_summary$r.squared
slm_popEst2015_test_r2 <- cor(testData$avgDeathsPerYear, slm_popEst2015_pred)^2

# Calculate Mean Squared Error (MSE)
fullmodel_mse <- mse(testData$avgDeathsPerYear, fullmodel_pred)
slm_popEst2015_mse <- mse(testData$avgDeathsPerYear, slm_popEst2015_pred)

# Compare R-squared values and MSE
cat("Full Model - Train R2:", fullmodel_train_r2, "Test R2:", fullmodel_test_r2, "MSE:", fullmodel_mse, "\n")
cat("Simple Model - Train R2:", slm_popEst2015_train_r2, "Test R2:", slm_popEst2015_test_r2, "MSE:", slm_popEst2015_mse, "\n")
