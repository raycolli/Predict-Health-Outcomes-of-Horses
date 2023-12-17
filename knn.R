install.packages("caret")
install.packages("e1071")
install.packages("class")
install.packages("reshape2")
install.packages("ggplot2")

# Read the data
original_data = train # train is imported using built-in RStudio's "Import Dataset" functionality
original_data$id = NULL
original_data$hospital_number=NULL
dim(train)
dim(test)

# Data Preprocessing
# Handling missing values, encoding categorical variables, and normalization

# Check for missing values - 0
sum(is.na(original_data))

# Column-wise Summary of Missing Values - 0 for all columns
sapply(original_data, function(x) sum(is.na(x)))

# Check for categorical predictors
summary(original_data)




# Correlation matrix for continuous predictors
cor_matrix <- cor(original_data[, sapply(original_data, is.numeric)])

# Visualize the matrix
cor_matrix_melted <- melt(cor_matrix)
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes(label = round(value, 2)), vjust = 1) 
scale_fill_gradient2() 




# Function to identify categorical columns
getCategoricalColumns <- function(data) {
  sapply(data, function(x) is.factor(x) || is.character(x))
}

# Encoding categorical variables
encodeCategoricalVariables <- function(data) {
  cat_cols <- getCategoricalColumns(data)
  for (col_name in names(data)[cat_cols]) {
    # Converting factors to characters to ensure consistent encoding
    data[[col_name]] <- as.character(data[[col_name]])
    # Then converting characters to numeric factors
    data[[col_name]] <- as.numeric(as.factor(data[[col_name]]))
  }
  return(data)
}

# Identifying and encoding categorical variables
data <- encodeCategoricalVariables(original_data)


# Verify changes
str(data)


categorical_vars = c("surgery", "temp_of_extremities", "peripheral_pulse", "mucous_membrane", "mucous_membrane", "capillary_refill_time", "pain", "peristalsis", "abdominal_distention", "nasogastric_tube", "rectal_exam_feces", "abdomen", "abdomo_appearance", "surgical_lesion", "cp_data", "lesion_3", "lesion_2", "outcome")


# Convert to factor only if column exists
for (var in categorical_vars) {
  if (var %in% colnames(data)) {
    data[[var]] <- factor(data[[var]])
  } else {
    warning(paste("Column", var, "not found in the dataset."))
  }
}

# Normalize continuous variables only
# Convert categorical variable names to column indices
cat_var_indices <- match(categorical_vars, names(data))

# Exclude categorical variables and scale the rest
continuous_vars <- setdiff(1:ncol(data), cat_var_indices)
data[, continuous_vars] <- scale(data[, continuous_vars])



# Check the structure of the normalized data
str(data)


# Drop columns
data$lesion_3 = NULL
data$hospital_number = NULL

# Set seed for reproducibility
set.seed(123)

# Calculate the size of the training set (80% of the dataset)
train_size <- floor(0.80 * nrow(data))

# Randomly sample indices for the training set
trainIdx = sample(nrow(data), size=train_size)

# Create training and testing sets
train_set <- data[trainIdx, ]
test_set <- data[-trainIdx, ]
print(dim(test_set))
print(dim(train_set))
evaluate_knn <- function(train_data, test_data, k_value) {
  set.seed(123)


train_x <- train_data[, -ncol(train_data)]
train_y <- train_data[, -ncol(train_data)]
test_x <- test_data[, -ncol(test_data)]
test_y <- test_data[, -ncol(test_data)]

# Train the KNN model
knn_model <- knn(train = train_x,
                 test = test_x,
                 cl = train_y,
                 k = k_value)

# Predictions and performance on training set
prediction_train <- knn(train = train_x,
                        test = test_x,
                        cl = train_y,
                        k = k_value)
print(length(predictions_train))
print(length(train_y))

#ConfusionMatrix_train <- confusionMatrix(predictions_train, train_y)

# Predictions and performance on test set
predictions_test <- knn(train = train_x,
                        test = test_x,
                        cl = train_y, # Use the trained model here
                        k = k_value)
confusionMatrix_test <- confusionMatrix(predictions_test, test_y)

# Print results
print(paste("KNN with k =", k_value))
# Print(confusionMatrix_train)
print(confusoinMatrix_test)
}

# Performance evaluation for different values of k
k_values <- c(1, 5, 10)
for (k_val in k_values) {
  evaluate_knn(train_set, test_set, k_val)
}

