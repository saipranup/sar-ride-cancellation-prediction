# Load necessary libraries ----------
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(visdat)
library(writexl)
library(corrplot)
library(stats)
library(randomForest)
library(lubridate)
library(caret)
library(pROC)
library(ROSE)

# Check the current working directory
getwd()
# Set the working directory to the folder containing the csv file
setwd("/Users/pranup/Downloads/Analytics_Practicum/Project1")
# Load the dataset ----------
# Set the file path
file_path <- "/Users/pranup/Downloads/Analytics_Practicum/Project1/SAR_Rental.csv"
data <- read.csv(file_path)

# 3. Data Preprocessing --------------------
# Check column names and fix column names if they have dots
colnames(data) <- gsub("\\.", "_", colnames(data))
# Convert datetime columns
data <- data %>%
  mutate(
    from_date = mdy_hm(from_date),
    to_date = mdy_hm(to_date),
    booking_created = mdy_hm(booking_created)
  )
# Check the first few rows of the dataset
head(data)
colnames(data)
str(data)

# Calculate the proportion of missing values in the entire dataset
missing_proportion <- function(data) {
  total_values <- prod(dim(data)) # Total number of values in the dataset
  missing_values <- sum(is.na(data)) # Total missing values
  proportion_missing <- missing_values / total_values # Calculate proportion
  return(proportion_missing)
}
# usage
missing_ratio <- missing_proportion(data)
print(paste0("Total Missingness Proportion: ", round(missing_ratio * 100, 2), "%"))
# Calculate missing proportion
missing_ratio <- missing_proportion(data)
present_ratio <- 1 - missing_ratio

# Create a data frame for plotting
missing_data <- data.frame(
  Category = c("Missing Data", "Present Data"),
  Proportion = c(missing_ratio, present_ratio)
)
# Load ggplot2 for visualization
library(ggplot2)
# Create pie chart
ggplot(missing_data, aes(x = "", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("orange", "lightgreen")) +
  labs(title = "Proportion of Missing vs Present Data")

# Check for missing values column-wise
missing_summary <- sapply(data, function(x) sum(is.na(x)))

# Display columns with missing values
missing_summary[missing_summary > 0]
# Visualizing missing values before handling
vis_miss(data) + ggtitle("Missing Values Heatmap")

# Proportion of missing values in each column
missing_proportion <- missing_summary / nrow(data)
missing_proportion

#handling
# Replace missing values in `package_id` with "Unknown"
data$package_id <- ifelse(is.na(data$package_id), "Unknown", data$package_id)

# Print column names
colnames(data)

library(dplyr)
# Drop 'row_' as it is irrelevant identifier, the model cannot find meaningful patterns from it, resulting in errors or noise during training.
data <- data %>% dplyr::select(-`row_`)


# Drop `from_city_id` and `to_city_id` due to high missingness and redundancy
data <- data %>%
  dplyr::select(-from_city_id, -to_city_id)


# Check remaining missing values
colSums(is.na(data))

#handling remaining missing values
# Replace missing `from_area_id` and `to_area_id` with "Unknown"
data$from_area_id <- ifelse(is.na(data$from_area_id), "Unknown", data$from_area_id)
data$to_area_id <- ifelse(is.na(data$to_area_id), "Unknown", data$to_area_id)

#The reason for creating a binary flag for missing to_date is to capture the information hidden in missingness itself.
#In this dataset, missing to_date might indicate that the trip was never completed, which is strongly associated with cancellations.
#By creating a binary flag (missing_to_date), we explicitly tell the model whether a to_date value was missing or not.
#The missing_to_date flag will allow the model to learn that rows with missing to_date often have Car_Cancellation = 1.
# Create a binary flag for missing `to_date`
data$missing_to_date <- ifelse(is.na(data$to_date), 1, 0)

# Stacked Bar-chart
ggplot(data, aes(x = as.factor(missing_to_date), fill = as.factor(Car_Cancellation))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Not Canceled", "Canceled")) +
  labs(title = "Proportion of Cancellations by Missing to_date",
       x = "Missing to_date (0 = No, 1 = Yes)",
       y = "Proportion of Trips",
       fill = "Cancellation Status") +
  theme_minimal()

library(dplyr)
# Dropping to_date as Imputing it shows extreme trip durations.
data <- data %>% dplyr::select(-to_date)


# Impute missing 'from_long' and 'from_lat' with the median
data$from_long[is.na(data$from_long)] <- median(data$from_long, na.rm = TRUE)
data$from_lat[is.na(data$from_lat)] <- median(data$from_lat, na.rm = TRUE)


# Impute missing 'to_lat' and 'to_long' with the median
data$to_lat[is.na(data$to_lat)] <- median(data$to_lat, na.rm = TRUE)
data$to_long[is.na(data$to_long)] <- median(data$to_long, na.rm = TRUE)

# Verify no missing values remain
colSums(is.na(data))

# check NA counts
print(colSums(is.na(data)))

# check NaN counts
print(sapply(data, function(x) sum(is.nan(x))))

# 4. Predictor Analysis and Relevancy ---------------------
# Cancellation Rates for Categorical Variables
#Group smaller categories (e.g., package_id 5 with a single record) into a single "Other" category to avoid noise.
# Group smaller `package_id` categories (count < 10) into "Other"
data$package_id <- ifelse(data$package_id %in% c("5"), "Other", data$package_id)
# Analyze categorical variables: package_id and travel_type_id
cat_vars <- c("package_id", "travel_type_id", "from_area_id", "to_area_id")

for (var in cat_vars) {
  # Calculate cancellation rates
  cancellation_analysis <- data %>%
    group_by_at(var) %>%
    summarize(
      cancellation_rate = mean(Car_Cancellation, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(desc(cancellation_rate))
  
  print(paste("Analysis for:", var))
  print(cancellation_analysis)
}

# Chi-square tests
# Chi-Square test for independence between categorical predictors and Car_Cancellation
#for (var in cat_vars) {
  # Create a contingency table
 # contingency_table <- table(data[[var]], data$Car_Cancellation)
  
  # Perform Chi-Square test
  #chi_sq_test <- chisq.test(contingency_table)
  
 # print(paste("Chi-Square Test for:", var))
 # print(chi_sq_test)
#}

# Fisher exact test
# Variables to test
#categorical_vars <- c("package_id", "travel_type_id", "from_area_id", "to_area_id")

# Loop through variables
#for (var in categorical_vars) {
  #cat("\nAnalysis for:", var, "\n")
  #contingency_table <- table(data[[var]], data$Car_Cancellation)
  
  # Perform Fisher's Exact Test
 # fisher_test_result <- fisher.test(contingency_table)
 # print(fisher_test_result)
#}

# Convert target variable to a factor (Binary Classification)
data$Car_Cancellation <- as.factor(data$Car_Cancellation)

# Feature Importance
# Train Random Forest
rf_model <- randomForest(Car_Cancellation ~ ., data = data, importance = TRUE)

# Extract Feature Importance
importance_scores <- importance(rf_model)
print(importance_scores)

# Visualize Importance
varImpPlot(rf_model)

colnames(data)


# Data sparsity
# Count occurrences of from_area_id
from_area_counts <- as.data.frame(table(data$from_area_id))
colnames(from_area_counts) <- c("Area_ID", "Count")

# Plot histogram for from_area_id
ggplot(from_area_counts, aes(x = Count)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Rides per From Area ID",
       x = "Number of Rides",
       y = "Count of Areas",
       caption = "Shows how some areas have very few rides, indicating sparsity.") +
  theme_minimal()

# 5. Data Engineering and Transformation ---------------------
library(lubridate)
library(dplyr)

# Extract day_of_week, hour_of_day, and month_of_year from from_date
data <- data %>%
  mutate(day_of_week = wday(from_date, label = TRUE, abbr = FALSE),
         hour_of_day = hour(from_date),
         month_of_year = month(from_date, label = TRUE, abbr = FALSE))

# Calculate trip_length using Haversine formula
GPSDist <- function(lat1, lon1, lat2, lon2) {
  R <- 6371.0 # Earth's radius in kilometers
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

library(dplyr)
# Compute trip_length and remove geospatial variables immediately
data <- data %>%
  mutate(trip_length = GPSDist(from_lat, from_long, to_lat, to_long)) %>%
  dplyr::select(-from_lat, -from_long, -to_lat, -to_long)

# VISUALIZATION: Trip Length Boxplot
ggplot(data, aes(y = trip_length)) +
  geom_boxplot(fill = "blue", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Trip Length", y = "Trip Length (km)") +
  theme_minimal()

# VISUALIZATION: Trip Length Histogram
ggplot(data, aes(x = trip_length)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Trip Length", x = "Trip Length (km)", y = "Frequency") +
  theme_minimal()

# VISUALIZATION: Trip Length vs. Cancellations
ggplot(data, aes(x = trip_length, fill = factor(Car_Cancellation))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Not Canceled", "Canceled")) +
  labs(title = "Trip Length Distribution by Ride Cancellation", x = "Trip Length (km)", y = "Density") +
  theme_minimal()

# VISUALIZATION:Create a bar plot of cancellations by hour_of_day
ggplot(data, aes(x = factor(hour_of_day), fill = Car_Cancellation)) +
  geom_bar(position = "dodge") +
  labs(title = "Ride Cancellations by Hour of Day",
       x = "Hour of Day",
       y = "Number of Bookings",
       fill = "Cancellation Status") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed


# Categorize booking_created into time buckets
data <- data %>%
  mutate(booking_time_bucket = case_when(
    hour(booking_created) >= 0 & hour(booking_created) < 6 ~ "Early Morning",
    hour(booking_created) >= 6 & hour(booking_created) < 12 ~ "Morning",
    hour(booking_created) >= 12 & hour(booking_created) < 18 ~ "Afternoon",
    hour(booking_created) >= 18 & hour(booking_created) < 24 ~ "Evening"
  ))

# VISUALIZATION: Booking Time Bucket vs. Cancellations (Stacked Bar Chart)
ggplot(data, aes(x = booking_time_bucket, fill = factor(Car_Cancellation))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Not Canceled", "Canceled")) +
  labs(title = "Booking Time Bucket vs. Cancellations", x = "Time Bucket", y = "Proportion of Trips") +
  theme_minimal()

# Identify traditional bookings (not online or mobile)
data$traditional_booking <- ifelse(data$online_booking == 0 & data$mobile_site_booking == 0, 1, 0)

# Convert Car_Cancellation to numeric
data$Car_Cancellation <- as.numeric(as.character(data$Car_Cancellation))

# Calculate cancellation rates for traditional vs. online bookings
cancellation_rates <- data %>%
  group_by(traditional_booking) %>%
  summarize(
    total_trips = n(),
    canceled_trips = sum(Car_Cancellation),
    cancellation_rate = mean(Car_Cancellation, na.rm = TRUE)
  )

# Print the summary table
print(cancellation_rates)

# Create a stacked bar chart
ggplot(data, aes(x = factor(traditional_booking), fill = factor(Car_Cancellation))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Not Canceled", "Canceled")) +
  labs(
    title = "Cancellation Rate by Booking Type",
    x = "Booking Type (0 = Online/Mobile, 1 = Traditional)",
    y = "Proportion of Trips",
    fill = "Cancellation Status"
  ) +
  theme_minimal()


# Convert datetime columns
data <- data %>%
  mutate(from_date = format(as.Date(from_date), "%m/%d/%Y"), 
         booking_created = format(as.Date(booking_created), "%m/%d/%Y"))

# Aggregate user-level features
user_features <- data %>%
  group_by(user_id) %>%
  summarize(
    user_total_bookings = n(),
    user_cancellation_rate = mean(Car_Cancellation, na.rm = TRUE),
    user_avg_trip_length = mean(trip_length, na.rm = TRUE)
  )

# Merge user features and drop user_id
data <- data %>%
  left_join(user_features, by = "user_id") %>%
  dplyr::select(-user_id)

# VISUALIZATION: User Cancellation Rate Distribution
ggplot(data, aes(x = user_cancellation_rate)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of User Cancellation Rate", x = "User Cancellation Rate", y = "Frequency") +
  theme_minimal()


# Binning Low-Frequency Categories (AFTER Feature Extraction)
new_threshold <- 50  # Define threshold

# Create a frequency table for from_area_id
from_area_freq <- data %>%
  group_by(from_area_id) %>%
  summarize(count = n())

# Now, perform binning based on frequency threshold
data$from_area_binned <- ifelse(data$from_area_id %in% from_area_freq$from_area_id[from_area_freq$count >= new_threshold], 
                                as.character(data$from_area_id), "Other")
# Create frequency table for to_area_id
to_area_freq <- data %>%
  group_by(to_area_id) %>%
  summarize(count = n())

# Perform binning
data$to_area_binned <- ifelse(data$to_area_id %in% to_area_freq$to_area_id[to_area_freq$count >= new_threshold], 
                              as.character(data$to_area_id), "Other")

# Convert to categorical
data$from_area_binned <- as.factor(data$from_area_binned)
data$to_area_binned <- as.factor(data$to_area_binned)

# Drop original `from_area_id` and `to_area_id` (NOW removing them)
data <- data %>% dplyr::select(-from_area_id, -to_area_id)

# Create a frequency count for vehicle_model_id
vehicle_counts <- data %>%
  group_by(vehicle_model_id) %>%
  summarise(total_rides = n(), cancel_rate = mean(Car_Cancellation))


# Binning vehicle models: Identify low-frequency models
low_count_models <- vehicle_counts$vehicle_model_id[vehicle_counts$total_rides < 20]
# Replace low-frequency vehicle models with "Other"
data$vehicle_model_id <- ifelse(data$vehicle_model_id %in% low_count_models, "Other", as.character(data$vehicle_model_id))
# Convert to factor
data$vehicle_model_id <- as.factor(data$vehicle_model_id)

# Validate dataset
summary(data)
glimpse(data)


colnames(data)

# Check for NAs in each column
na_summary <- colSums(is.na(data))
print("NA Summary:")
print(na_summary)

# Check for zeros in each column
zero_summary <- sapply(data, function(col) sum(col == 0, na.rm = TRUE))
print("Zero Summary:")
print(zero_summary)

# Impute `trip_length` where it is 0 AND the trip is not canceled
data$trip_length[data$trip_length == 0 & data$Car_Cancellation == 0] <- 
  median(data$trip_length[data$trip_length > 0 & data$Car_Cancellation == 0], na.rm = TRUE)

# Verify if there are any remaining 0 values in `trip_length`
sum(data$trip_length == 0)

# 6. Dimension Reduction (Feature Selection) ---------------
# 1. Fisher's Exact Test for Categorical Variables
cat_vars <- c("from_area_binned", "to_area_binned", "vehicle_model_id", 
              "package_id", "travel_type_id", "day_of_week", "month_of_year")

# Create an empty list to store results
fisher_results <- list()

for (var in cat_vars) {
  cat("\nFisher's Exact Test for:", var, "\n")
  
  # Create contingency table
  contingency_table <- table(data[[var]], data$Car_Cancellation)
  
  # Perform Fisher's Exact Test
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
  
  # Store p-value
  fisher_results[[var]] <- fisher_test$p.value
}

# Convert Fisher's test results into a data frame
fisher_df <- data.frame(Variable = names(fisher_results), 
                        P_Value = unlist(fisher_results))

# Filter significant variables (p-value < 0.05)
significant_vars <- fisher_df %>% filter(P_Value < 0.05)

# Print significant categorical variables
print("Significant Categorical Variables (p < 0.05):")
print(significant_vars)

# 2. Correlation Analysis for Numerical Variables
num_vars <- data %>% dplyr::select(trip_length, hour_of_day, user_total_bookings, user_cancellation_rate)

# Compute correlation matrix
cor_matrix <- cor(num_vars, use = "complete.obs")

# Visualize correlation matrix
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numerical Variables", mar = c(0, 0, 1, 0))

# Identify highly correlated variables (> 0.85)
high_corr <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, arr.ind = TRUE)

# Print highly correlated variable pairs
if (length(high_corr) > 0) {
  print("Highly Correlated Variable Pairs (r > 0.85):")
  print(high_corr)
} else {
  print("No highly correlated numerical variables detected.")
}

# 3. Feature Importance using Random Forest

data$Car_Cancellation <- as.factor(data$Car_Cancellation)

rf_model <- randomForest(Car_Cancellation ~ ., data = data, importance = TRUE)

set.seed(42)

# Train a Random Forest model
rf_model <- randomForest(Car_Cancellation ~ ., data = data, importance = TRUE)

# Extract feature importance
importance_scores <- importance(rf_model)

# Convert to data frame for visualization
importance_df <- data.frame(Variable = rownames(importance_scores), 
                            Importance = importance_scores[, 1])

# Sort by importance
importance_df <- importance_df %>% arrange(desc(Importance))

# Plot Feature Importance
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "Features", y = "Importance") +
  theme_minimal()

# 4. Handling Zero Values in Numerical Variables
# Impute trip_length zero values for non-canceled trips
data$trip_length[data$trip_length == 0 & data$Car_Cancellation == 0] <- 
  median(data$trip_length[data$trip_length > 0 & data$Car_Cancellation == 0], na.rm = TRUE)

# Verify if there are any remaining zero values in `trip_length`
sum(data$trip_length == 0)

# 5. Final Feature Selection
# Load necessary libraries
library(dplyr)

# Define all features to drop
features_to_drop <- c("month_of_year", "day_of_week", "missing_to_date", 
                      "package_id", "travel_type_id", "user_avg_trip_length", 
                      "mobile_site_booking", "booking_time_bucket", "vehicle_model_id")

# Remove unnecessary columns BEFORE partitioning
data <- data %>% dplyr::select(-all_of(features_to_drop))

# Print final dataset summary
summary(data)
str(data)
# Ensure data types are in correct format before modeling 
data <- data %>%
  mutate(from_date = as.Date(from_date, format = "%m/%d/%Y"),
         booking_created = as.Date(booking_created, format = "%m/%d/%Y"))
data$traditional_booking <- as.factor(data$traditional_booking)
data$online_booking <- as.factor(data$online_booking)

str(data)
# Load required library
#library(writexl)

# Save updated dataset as an Excel file
#write_xlsx(data, "/Users/pranup/Downloads/Analytics_Practicum/Project1/Updated_SAR_Dataset.xlsx")

# Confirm file saved
#cat("Updated dataset saved")

# 7. Data Partitioning ---------------
# Set seed for reproducibility
set.seed(42)

# Perform an 80-20 stratified split to maintain class balance
trainIndex <- createDataPartition(data$Car_Cancellation, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Verify class distribution remains balanced
cat("Training Set Class Distribution:\n")
print(prop.table(table(train_data$Car_Cancellation)))

cat("\nTest Set Class Distribution:\n")
print(prop.table(table(test_data$Car_Cancellation)))

# Validate dataset after partitioning
str(train_data)
str(test_data)

# 9. Model Fitting, Validation Accuracy, and Test Accuracy ------------
# ---- 9.1 Baseline Random Forest ----

# Set seed for reproducibility
set.seed(42)
# Train Baseline Random Forest Model
rf_baseline <- randomForest(Car_Cancellation ~ ., 
                            data = train_data, 
                            ntree = 100, 
                            importance = TRUE)

# Predictions on Training Set
train_predictions <- predict(rf_baseline, train_data, type = "response")
train_conf_matrix <- confusionMatrix(train_predictions, train_data$Car_Cancellation, positive = "1")

# Predictions on Test Set
test_predictions <- predict(rf_baseline, test_data, type = "response")
test_conf_matrix <- confusionMatrix(test_predictions, test_data$Car_Cancellation, positive = "1")

# Compute Confusion Matrix
conf_matrix_baseline <- confusionMatrix(test_predictions, test_data$Car_Cancellation, positive = "1")
print(conf_matrix_baseline)


# Extract Training and Testing Accuracy
train_accuracy <- train_conf_matrix$overall["Accuracy"]
test_accuracy <- test_conf_matrix$overall["Accuracy"]

# Extract Other Performance Metrics
sensitivity <- test_conf_matrix$byClass["Sensitivity"]   # True Positive Rate
specificity <- test_conf_matrix$byClass["Specificity"]   # True Negative Rate
precision <- test_conf_matrix$byClass["Precision"]       # Positive Predictive Value
npv <- test_conf_matrix$byClass["Neg Pred Value"]        # Negative Predictive Value
f1_score <- test_conf_matrix$byClass["F1"]               # F1 Score

# Print Performance Metrics
cat("\nBaseline Model Performance Metrics:\n")
cat("Training Accuracy:", train_accuracy, "\n")
cat("Testing Accuracy:", test_accuracy, "\n")
cat("Sensitivity (Recall):", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision (PPV):", precision, "\n")
cat("Negative Predictive Value (NPV):", npv, "\n")
cat("F1 Score:", f1_score, "\n")

# Compute probability predictions instead of class labels
predictions_prob <- predict(rf_baseline, test_data, type = "prob")[, 2]  # Probability of class 1

# Compute ROC and AUC using probability scores
roc_baseline <- roc(test_data$Car_Cancellation, predictions_prob, levels = c(0,1), direction = "<")
auc_baseline <- auc(roc_baseline)

# Print AUC Score
cat("AUC Score (Baseline Model):", auc_baseline, "\n")

# Plot ROC Curve for Baseline Model
plot(roc_baseline, col = "blue", lwd = 2, main = "ROC Curve - Baseline Model", legacy.axes = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal reference line
hist(predictions_prob, breaks = 20, main = "Distribution of Predicted Probabilities")


# Feature Importance Plot
varImpPlot(rf_baseline)

# ---- 9.2 Hybrid RF (Class Weighting + Oversampling) ----
# Load required libraries
library(randomForest)
library(caret)
library(pROC)

# Set seed for reproducibility
set.seed(123)

# Convert target variable to factor
train_data$Car_Cancellation <- as.factor(train_data$Car_Cancellation)

# Manual Oversampling: Duplicate minority class instances
minority_class <- train_data[train_data$Car_Cancellation == "1", ]
train_data_oversampled <- rbind(train_data, minority_class, minority_class)  # Duplicating twice

# Define class weights (higher weight for cancellations)
class_weights <- c("0" = 1, "1" = sum(train_data$Car_Cancellation == 0) / sum(train_data$Car_Cancellation == 1))

# Train Random Forest with class weighting
rf_weighted <- randomForest(
  Car_Cancellation ~ ., 
  data = train_data_oversampled, 
  ntree = 500, 
  mtry = sqrt(ncol(train_data_oversampled) - 1), 
  classwt = class_weights, 
  importance = TRUE
)

# Predictions on test set
test_predictions_rf_weighted <- predict(rf_weighted, test_data, type = "class")

# Confusion Matrix
conf_matrix_rf_weighted <- confusionMatrix(test_predictions_rf_weighted, as.factor(test_data$Car_Cancellation))
print(conf_matrix_rf_weighted)

# Predictions on Training Set
train_predictions_rf_weighted <- predict(rf_weighted, train_data_oversampled, type = "class")
train_conf_matrix_rf_weighted <- confusionMatrix(train_predictions_rf_weighted, train_data_oversampled$Car_Cancellation, positive = "1")

# Predictions on Test Set
test_predictions_rf_weighted <- predict(rf_weighted, test_data, type = "class")
test_conf_matrix_rf_weighted <- confusionMatrix(test_predictions_rf_weighted, as.factor(test_data$Car_Cancellation), positive = "1")

# Extract Training and Testing Accuracy
train_accuracy_rf_weighted <- train_conf_matrix_rf_weighted$overall["Accuracy"]
test_accuracy_rf_weighted <- test_conf_matrix_rf_weighted$overall["Accuracy"]

# Extract Other Performance Metrics
sensitivity_rf_weighted <- test_conf_matrix_rf_weighted$byClass["Sensitivity"]   # True Positive Rate
specificity_rf_weighted <- test_conf_matrix_rf_weighted$byClass["Specificity"]   # True Negative Rate
precision_rf_weighted <- test_conf_matrix_rf_weighted$byClass["Precision"]       # Positive Predictive Value
npv_rf_weighted <- test_conf_matrix_rf_weighted$byClass["Neg Pred Value"]        # Negative Predictive Value
f1_score_rf_weighted <- test_conf_matrix_rf_weighted$byClass["F1"]               # F1 Score

# Print Performance Metrics
cat("\nHybrid RF (Class Weighting + Oversampling) Model Performance Metrics:\n")
cat("Training Accuracy:", train_accuracy_rf_weighted, "\n")
cat("Testing Accuracy:", test_accuracy_rf_weighted, "\n")
cat("Sensitivity (Recall):", sensitivity_rf_weighted, "\n")
cat("Specificity:", specificity_rf_weighted, "\n")
cat("Precision (PPV):", precision_rf_weighted, "\n")
cat("Negative Predictive Value (NPV):", npv_rf_weighted, "\n")
cat("F1 Score:", f1_score_rf_weighted, "\n")

# Compute probability predictions for ROC & AUC
predictions_prob_rf_weighted <- predict(rf_weighted, test_data, type = "prob")[, 2]  # Probability of class 1
# Compute ROC & AUC
roc_rf_weighted <- roc(test_data$Car_Cancellation, as.numeric(test_predictions_rf_weighted), levels = c(0,1), direction = "<")
auc_rf_weighted <- auc(roc_rf_weighted)
cat("AUC Score (Class Weighted + Manual Oversampling RF Model):", auc_rf_weighted, "\n")

# Plot ROC Curve
plot(roc_rf_weighted, col = "blue", main = "ROC Curve - Class Weighted + Manual Oversampling RF Model")

# Feature Importance Plot
varImpPlot(rf_weighted, main = "Feature Importance - Class Weighted + Manual Oversampling RF Model")

library(dplyr)
# Removing from_date from training and test datasets as we have already extracted features from that variable and may be this is causing redundancy in executing logistic regression, so, I am removing this.
train_data <- train_data %>% dplyr::select(-from_date)
test_data <- test_data %>% dplyr::select(-from_date)

# ---- 9.3 Oversampled Logistic Regression ----
# Apply oversampling to balance the dataset
oversampled_data <- ovun.sample(Car_Cancellation ~ ., data = train_data, method = "over", seed = 1)$data

# Check class distribution after oversampling
table(oversampled_data$Car_Cancellation)

# Train logistic regression model on the oversampled dataset
log_model_oversampled <- glm(Car_Cancellation ~ ., data = oversampled_data, family = binomial)

# Predict probabilities on test set
test_probabilities_oversampled <- predict(log_model_oversampled, test_data, type = "response")

# Convert probabilities to class labels (Threshold = 0.5)
test_predictions_oversampled <- ifelse(test_probabilities_oversampled > 0.5, 1, 0)


# Compute Training Accuracy
train_predictions_oversampled <- ifelse(predict(log_model_oversampled, oversampled_data, type = "response") > 0.5, 1, 0)
train_accuracy_oversampled <- mean(train_predictions_oversampled == oversampled_data$Car_Cancellation)

# Compute Test Accuracy
test_predictions_oversampled <- ifelse(predict(log_model_oversampled, test_data, type = "response") > 0.5, 1, 0)
test_accuracy_oversampled <- mean(test_predictions_oversampled == test_data$Car_Cancellation)

# Compute Confusion Matrix
conf_matrix_oversampled <- confusionMatrix(as.factor(test_predictions_oversampled), as.factor(test_data$Car_Cancellation))
print(conf_matrix_oversampled)


# Extract key evaluation metrics
sensitivity_oversampled <- conf_matrix_oversampled$byClass["Sensitivity"]
specificity_oversampled <- conf_matrix_oversampled$byClass["Specificity"]
precision_oversampled <- conf_matrix_oversampled$byClass["Pos Pred Value"]
npv_oversampled <- conf_matrix_oversampled$byClass["Neg Pred Value"]
f1_score_oversampled <- 2 * ((precision_oversampled * sensitivity_oversampled) / (precision_oversampled + sensitivity_oversampled))

# Print Metrics
cat("\nOversampled Logistic Regression Performance Metrics:\n")
cat("Training Accuracy:", train_accuracy_oversampled, "\n")
cat("Testing Accuracy:", test_accuracy_oversampled, "\n")
cat("Sensitivity (Recall):", sensitivity_oversampled, "\n")
cat("Specificity:", specificity_oversampled, "\n")
cat("Precision (PPV):", precision_oversampled, "\n")
cat("Negative Predictive Value (NPV):", npv_oversampled, "\n")
cat("F1 Score:", f1_score_oversampled, "\n")


# Compute ROC Curve & AUC Score
roc_oversampled <- roc(test_data$Car_Cancellation, test_probabilities_oversampled, levels = c(0,1), direction = "<")
auc_oversampled <- auc(roc_oversampled)

# Print AUC Score
cat("AUC Score (Oversampled Logistic Regression):", auc_oversampled, "\n")

# Plot ROC Curve
plot(roc_oversampled, col = "red", main = "ROC Curve - Oversampled Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Reference line




