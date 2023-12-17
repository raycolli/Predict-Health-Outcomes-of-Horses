install.packages("e1071")
install.packages("caret")
library(caret)
library(e1071)
library(reshape2)

# Read the data
original_data = train # train is imported using built-in RStudio's "Import Dataset" functionality
original_data$id = NULL
original_data$hospital_number=NULL


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
sample(nrow(data), size=train_size)

# Create training and testing sets
train_set <- data[trainIdx, ]
test_set <- data[-trainIdx, ]


# SVM Model Training with hyperparameter tuning

# Define a function to perform SVM tuning and evaluation
evaluate_svm <- function(kernel_type, train_data, test_data, cost_values, gamma_values=NULL, degree_values=NULL) {
  set.seed(123)
  control <- tune.control(cross = 10)
  # Tune the model
  if(!is.null(gamma_values)){
    tune_result <- tune(svm, outcome ~ ., data = train_data, kernel = kernel_type, ranges = list(cost = cost_values, gamma = gamma_values), tunecontrol = control)
  }
  else if(!is.null(degree_values)){
    tune_result <- tune(svm, outcome ~ ., data = train_data, kernel = kernel_type, ranges = list(cost = cost_values, degree = degree_values), tunecontrol = control)
  }
  else {
    tune_result <- tune(svm, outcome ~ ., data = train_data, kernel = kernel_type, ranges = list(cost = cost_values), tunecontrol = control)
  }
  
  
  # Train the model with the best parameters
  best_model <- tune_result$best.model
  
  # Predictions and performance on training set
  predictions_train <- predict(best_model, newdata = train_data)
  confusionMatrix_train <- confusionMatrix(predictions_train, train_data$outcome)
  
  # Predictions and performance on test set
  predictions_test <- predict(best_model, newdata = test_data)
  confusionMatrix_test <- confusionMatrix(predictions_test, test_data$outcome)
  
  # Print results
  print(kernel_type)
  print(summary(tune_result))
  print(confusionMatrix_train)
  print(confusionMatrix_test)
}

# Perform evaluation for each kernel type
evaluate_svm("linear", train_set, test_set, cost_values = c(0.1,1,10,100))
# evaluate_svm("radial", train_set, test_set, cost_values = c(), gamma_values = seq(from=0.1, to=0.3, by=0.01))
evaluate_svm("radial", train_set, test_set, cost_values = c(0.01,0.1,1,10,100,1000), gamma_values = c(0.01,0.1,1,10))
evaluate_svm("polynomial", train_set, test_set, cost_values = seq(1, 10, 1), degree_values = c(1, 2))
