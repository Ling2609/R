# LOW WEI LING, TP080089
# Hypothesis: Customer demographic and geographic factors,alongside specific customer segment product brand preferences, 
# significantly influence customer rating behavior and satisfaction.

# Objective: To comprehensively analyze how demographic factors (age, income, gender), geographic variations, and 
# customer segment product preferences influence customer rating behavior, leveraging descriptive, diagnostic, predictive, and 
# prescriptive analytics to derive actionable insights for enhancing satisfaction.

library(dplyr)
library(ggplot2)
library(vcd)
library(dunn.test)
library(ggridges)
library(gt)
library(FactoMineR)
library(effsize) 
library(RColorBrewer) 
library(effectsize) 
library(rcompanion) 
library(treemap)
library(factoextra)
library(tidyr)
library(cluster)
library(smotefamily)
library(caret)
library(circlize)
library(scales)
library(leaflet)       
library(sf)            
library(rnaturalearth) 
library(rnaturalearthdata) 
library(tigris)        
library(randomForest)
library(DescTools)

check_na <- function(data, step) {
  if(any(is.na(data))) {
    stop(paste("NA values detected at step:", step))
  } else {
    print(paste("No NA values at step:", step))
  }
}
# Import dataset
complete_data <- read.csv("C:\\Users\\Ling\\Desktop\\Y2S1\\data analysis r code\\R assignment\\complete_data.csv")

# Check for NAs after loading the dataset
check_na(complete_data, "Dataset Import")

complete_data$Ratings <- factor(complete_data$Ratings, levels = c("Low", "High"), labels = c("Low", "High"))
complete_data$Income <- factor(complete_data$Income, levels = c("Low", "Medium", "High"))
complete_data$Gender <- factor(complete_data$Gender) # Ensure Gender is a factor

# --- Analysis 1: Income vs. Ratings ---
# How does income level impact customer rating behaviour, and what statistical evidence supports this relationship?
summary_table <- complete_data %>%
  group_by(Income) %>%
  summarise(
    Total_Count = n(),
    High_Rating_Count = sum(as.numeric(Ratings) == 2), 
    Low_Rating_Count = sum(as.numeric(Ratings) == 1),
    Proportion_High = mean(as.numeric(Ratings) == 2),
    Proportion_Low = mean(as.numeric(Ratings) == 1)
  ) %>%
  arrange(desc(Proportion_High))

check_na(summary_table, "Summary Table (Income)")

print(summary_table %>% gt() %>% tab_header(title = "Summary of Ratings by Income Group"))

income_ratings_table <- table(complete_data$Income, complete_data$Ratings)
print(income_ratings_table)

plot_data_for_labels <- complete_data %>%
  group_by(Income, Ratings) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(Income) %>% 
  mutate(
    proportion = count / sum(count), 
  ) %>%
  ungroup()

# Stacked Bar Chart
ggplot(complete_data, aes(x = Income, fill = Ratings)) +
  geom_bar(position = "fill") +  
  scale_fill_manual(
    values = c("Low" = "steelblue", "High" = "tomato"),
    labels = c("Low", "High")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  
  labs(
    title = "Proportion of Customer Ratings by Income Level",
    x = "Income Level",
    y = "Proportion (%)",
    fill = "Ratings"
  ) +
  theme_minimal() +
  geom_text(
    data = plot_data_for_labels,
    aes(x = Income, y = proportion, label = scales::percent(proportion)), 
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

income_ratings_df <- as.data.frame(income_ratings_table)
names(income_ratings_df) <- c("Income", "Ratings", "Freq")

# Mosaic Plot
mosaic(~ Income + Ratings,
       data = income_ratings_df, 
       gp = shading_Friendly,
       direction = c("v", "h"),
       spacing = spacing_equal(),
       main = "Mosaic Plot: Customer Ratings by Income Level",
       set_varnames = c(Income = "Income Level", Ratings = "Ratings"),
       labeling = labeling_border(rot_labels = c(left = 90, top = 0))
)

# Extra Feature 1
# Chord Diagram
chord_data_refined <- complete_data %>%
  count(Income, Ratings) %>%
  rename(Value = n) %>%
  mutate(
    Income_Sector = paste0(Income, " Income"),
    Ratings_Sector = paste0(Ratings, " Rating")
  )

chord_data_refined$Income_Sector <- factor(chord_data_refined$Income_Sector, levels = c("Low Income", "Medium Income", "High Income"))
chord_data_refined$Ratings_Sector <- factor(chord_data_refined$Ratings_Sector, levels = c("Low Rating", "High Rating"))

income_sector_colors <- c(
  "Low Income" = brewer.pal(3, "Set2")[1],
  "Medium Income" = brewer.pal(3, "Set2")[2],
  "High Income" = brewer.pal(3, "Set2")[3]
)
rating_sector_colors <- c(
  "Low Rating" = "royalblue",
  "High Rating" = "gold"
)

all_sector_grid_colors <- c(income_sector_colors, rating_sector_colors)

circos.clear()
par(mar = c(5.1, 4.1, 4.1, 2.1))

chordDiagram(
  x = chord_data_refined[, c("Income_Sector", "Ratings_Sector", "Value")],
  grid.col = all_sector_grid_colors,
  col = rating_sector_colors[chord_data_refined$Ratings_Sector], 
  directional = 1,
  direction.type = c("diffHeight", "arrows"),
  link.arr.length = 0.05,
  link.sort = TRUE,
  link.largest.ontop = TRUE,
  transparency = 0.25,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chord_data_refined[, c("Income_Sector", "Ratings_Sector")])))))
)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + 0.1, sector.name,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.7)
}, bg.border = NA)

title("Customer Ratings Flow by Income Level", cex.main = 1.2, line = -3)

legend(
  x = "right",
  pch = 15,
  col = all_sector_grid_colors,
  legend = names(all_sector_grid_colors),
  title = "Category Colors",
  bty = "n",
  cex = 0.8
)

# Perform Chi-Square Test
chi_square_income_ratings <- chisq.test(income_ratings_table)

# Optional: Check for low expected frequencies and consider Fisher's Exact Test if needed
if (any(chi_square_income_ratings$expected < 5)) {
  print("Warning: Some cells have low expected frequencies, Chi-Square results may be less reliable.")
  print("Consider Fisher's Exact Test if applicable.")
  # fisher_test_income_ratings <- fisher.test(income_ratings_table)
  # print(fisher_test_income_ratings)
}

print(chi_square_income_ratings)

cramers_v_income_ratings <- sqrt(chi_square_income_ratings$statistic / (sum(income_ratings_table) * (min(dim(income_ratings_table)) - 1)))
print(cramers_v_income_ratings)

# --- Analysis 2: Age vs. Ratings ---
# How does age influence customer rating behaviour, and what statistical evidence supports this relationship?
# Create Age Group
complete_data$Age_Group <- cut(complete_data$Age,
                               breaks = c(18, 25, 35, 45, 55, 70),
                               labels = c("18-25", "26-35", "36-45", "46-55", "56-70"),
                               include.lowest = TRUE)

complete_data$Age_Binary <- ifelse(!is.na(complete_data$Age) & complete_data$Age <= 35, "Young", "Old")
complete_data$Age_Binary <- as.factor(complete_data$Age_Binary)
print(levels(complete_data$Age_Binary)) 
sum(is.na(complete_data$Age_Binary))    
sum(is.na(complete_data$Age))
sum(is.na(complete_data$Ratings))

# **Comparison Table: Ratings Breakdown by Age Group**
comparison_table_age <- complete_data %>%
  group_by(Age_Group) %>%
  summarise(
    Total_Count = n(),
    High_Rating_Count = sum(Ratings == "High"),
    Low_Rating_Count = sum(Ratings == "Low"),
    Proportion_High = mean(Ratings == "High"),
    Proportion_Low = mean(Ratings == "Low"),
    .groups = 'drop' # Ensure output is not grouped
  ) %>%
  arrange(desc(Proportion_High))

print(comparison_table_age)

plot_data_age <- complete_data %>%
  group_by(Age_Group) %>%
  summarise(
    High_Rating_Proportion = mean(Ratings == "High"),
    Low_Rating_Proportion = mean(Ratings == "Low"),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_longer(cols = ends_with("_Proportion"),
                      names_to = "Rating_Type",
                      values_to = "Proportion") %>%
  mutate(Rating_Type = factor(Rating_Type, levels = c("Low_Rating_Proportion", "High_Rating_Proportion"),
                              labels = c("Low Rating", "High Rating")))

age_ratings_table <- table(complete_data$Age_Group, complete_data$Ratings)

# Stacked Bar Chart
trend_data <- plot_data_age %>%
  filter(Rating_Type == "High Rating")

ggplot(plot_data_age, aes(x = Age_Group, y = Proportion, fill = Rating_Type)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("Low Rating" = "orange", "High Rating" = "darkgreen")) +
  
  # Add trend line for high ratings
  geom_line(data = trend_data, aes(x = Age_Group, y = Proportion, group = 1), 
            color = "red", linewidth = 1.2, linetype = "dashed") +  # Use linewidth instead of size
  geom_point(data = trend_data, aes(x = Age_Group, y = Proportion), 
             color = "red", size = 3) +
  
  labs(title = "Ratings Distribution by Age Group with Trend Line",
       x = "Age Group",
       y = "Proportion",
       fill = "Rating") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  geom_text(aes(label = scales::percent(Proportion)), 
            position = position_fill(vjust = 0.5),    
            color = "white", size = 3.5,
            fontface = "bold")  # Make text bold

# Mosaic Plot
mosaic(~ Age_Group + Ratings, data = complete_data, shade = TRUE,
       main = "Mosaic Plot: Customer Ratings by Age Group",
       set_varnames = c(Age_Group = "Age Group", Ratings = "Ratings"))

# Violin Plot + Box Plot
ggplot(complete_data, aes(x = Ratings, y = Age, fill = Ratings)) +
  geom_violin(alpha = 0.6) + # Soften the fill color
  geom_boxplot(width = 0.1, alpha = 0.4, color = "black") + 
  scale_fill_manual(values = c("Low" = "purple", "High" = "orange")) +
  labs(title = "Age Distribution Across Ratings", x = "Ratings", y = "Age") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

# **Chi-Square Test: Age Group vs Ratings**
chisq_test_result_age <- chisq.test(age_ratings_table)
if (any(chisq_test_result_age$expected < 5)) {
  fisher_test_age <- fisher.test(age_ratings_table)
  print("Fisher's Exact Test Results (due to low expected cell counts for Age Group vs Ratings):")
  print(fisher_test_age)
} else {
  print("Chi-Square Test Results (Age Group vs Ratings):")
  print(chisq_test_result_age)
}

if(any(age_ratings_table < 5)) {
  print("Warning: Some cells in Age Group vs Ratings table have low frequencies, which may impact Chi-Square results.")
}

cramers_v_age_ratings <- sqrt(chisq_test_result_age$statistic / (sum(age_ratings_table) * (min(dim(age_ratings_table)) - 1)))
print(cramers_v_age_ratings)

sum(is.na(complete_data$Age))  # Check for missing Age values
sum(is.na(complete_data$Ratings))  # Check for missing Ratings values

# Extra feature 2
# Wilcoxon rank-sum test with Ratings converted to numeric
wilcox_test_age_binary <- wilcox.test(as.numeric(Ratings) ~ Age_Binary, data = complete_data)
print(wilcox_test_age_binary)

phi_coefficient <- Phi(table(complete_data$Age_Binary, complete_data$Ratings))
print(phi_coefficient)


# Load and select required columns
ml_data <- complete_data %>%
  select(Age, Gender, Income, Country, State, Ratings, Customer_Segment)

# Convert categorical columns to factors
ml_data <- ml_data %>%
  mutate(
    Gender = as.factor(Gender),
    Income = as.factor(Income),
    Country = as.factor(Country),
    State = as.factor(State),
    Ratings = factor(Ratings, levels = c("Low", "High")),  # Target variable
    Customer_Segment = as.factor(Customer_Segment)
  )

# Split into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(ml_data$Ratings, p = 0.8, list = FALSE)
train_data <- ml_data[trainIndex, ]
test_data <- ml_data[-trainIndex, ]

# Separate features and target
X_train <- train_data[, -which(names(train_data) == "Ratings")]
y_train <- train_data$Ratings

# Ensure all variables are numeric (strictly for smotefamily::SMOTE)
X_train$Gender <- as.numeric(as.factor(X_train$Gender))
X_train$Income <- as.numeric(as.factor(X_train$Income))
X_train$Country <- as.numeric(as.factor(X_train$Country))
X_train$State <- as.numeric(as.factor(X_train$State))
X_train$Customer_Segment <- as.numeric(as.factor(X_train$Customer_Segment))
X_train$Age <- as.numeric(X_train$Age)  # just to be explicit

# Ensure target is 0/1
y_train <- ifelse(y_train == "High", 1, 0)

# Check structure to confirm all numeric
str(X_train)

# Now apply SMOTE from smotefamily
set.seed(123)

# Extra Feature 3
# Class Imbalance Handling (SMOTE)
smote_result <- SMOTE(X_train, y_train, K = 5, dup_size = 1)

# Extract the balanced data
balanced_data <- smote_result$data
balanced_data$Ratings <- as.factor(balanced_data$class)
balanced_data$class <- NULL

saveRDS(balanced_data, "balanced_data.rds")
balanced_data <- readRDS("balanced_data.rds")

# Optional: confirm balance
table(balanced_data$Ratings)

library(randomForest)
# Step 1: Train/Test Split on Balanced Data
set.seed(123)
trainIndex_bal <- createDataPartition(balanced_data$Ratings, p = 0.8, list = FALSE)
train_bal <- balanced_data[trainIndex_bal, ]
test_bal <- balanced_data[-trainIndex_bal, ]

# Step 2: Train Random Forest Model
set.seed(123)
rf_model <- randomForest(Ratings ~ ., data = train_bal, ntree = 100)
# Save Random Forest model
saveRDS(rf_model, file = "rf_model.rds")

rf_model <- readRDS("rf_model.rds")
rf_pred <- predict(rf_model, test_bal)
rf_prob <- predict(rf_model, test_bal, type = "prob")[, "1"]  # probability for class 1 (High)

library(pROC)
# ROC for Random Forest
rf_roc <- roc(response = test_bal$Ratings, predictor = rf_prob, levels = c("0", "1"))

# Save ROC for Random Forest
saveRDS(rf_roc, file = "rf_roc.rds")

# Step 3: Train Logistic Regression Model
log_model <- glm(Ratings ~ ., data = train_bal, family = binomial)
# Save Logistic Regression model
saveRDS(log_model, file = "log_model.rds")
log_model <- readRDS("log_model.rds")
log_prob <- predict(log_model, test_bal, type = "response")
log_pred <- ifelse(log_prob > 0.5, "1", "0")
log_pred <- as.factor(log_pred)

# ROC for Logistic Regression
log_roc <- roc(response = test_bal$Ratings, predictor = log_prob, levels = c("0", "1"))

# Save ROC for Logistic Regression
saveRDS(log_roc, file = "log_roc.rds")

train_label <- ifelse(train_bal$Ratings == "1", 1, 0)
test_label <- ifelse(test_bal$Ratings == "1", 1, 0)

# Extra Feature 4
# XGBoost (Extreme Gradient Boosting)
library(xgboost)

train_matrix <- as.matrix(train_bal[, -which(names(train_bal) == "Ratings")])
test_matrix <- as.matrix(test_bal[, -which(names(test_bal) == "Ratings")])

set.seed(123)
xgb_model <- xgboost(
  data = train_matrix,
  label = train_label,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "auc",
  verbose = 0
)
saveRDS(xgb_model, file = "xgb_model.rds")
xgb_model <- readRDS("xgb_model.rds")
xgb_pred_prob <- predict(xgb_model, newdata = test_matrix, iteration_range = c(1, 100))
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)
xgb_pred_factor <- factor(xgb_pred_class, levels = c(0, 1))

library(pROC)
xgb_roc <- roc(test_bal$Ratings, xgb_pred_prob)

results <- data.frame(
  ID = rownames(test_bal),
  Actual = test_bal$Ratings,
  Predicted = xgb_pred_factor,
  Probability = xgb_pred_prob
)

write.csv(results, "xgb_predictions.csv", row.names = FALSE)
saveRDS(xgb_model, file = "xgb_model.rds")
saveRDS(xgb_roc, file = "xgb_roc.rds")

# Save Random Forest predictions
rf_results <- data.frame(
  ID = rownames(test_bal),
  Actual = test_bal$Ratings,
  Predicted = rf_pred,
  Probability = rf_prob
)
write.csv(rf_results, "rf_predictions.csv", row.names = FALSE)

# Save Logistic Regression predictions
log_results <- data.frame(
  ID = rownames(test_bal),
  Actual = test_bal$Ratings,
  Predicted = log_pred,
  Probability = log_prob
)
write.csv(log_results, "log_predictions.csv", row.names = FALSE)

# XGBoost
confusionMatrix(factor(results$Predicted), factor(results$Actual))

# Random Forest
confusionMatrix(factor(rf_results$Predicted), factor(rf_results$Actual))

# Logistic Regression
confusionMatrix(factor(log_results$Predicted), factor(log_results$Actual))

# Tuned Random Forest
balanced_data <- readRDS("balanced_data.rds")

set.seed(123)
library(ranger)

trainIndex_bal <- createDataPartition(balanced_data$Ratings, p = 0.8, list = FALSE)

# Restore Train and Test sets
train_bal <- balanced_data[trainIndex_bal, ]
test_bal  <- balanced_data[-trainIndex_bal, ]

train_bal$Ratings <- factor(train_bal$Ratings, levels = c("0", "1"), labels = c("Low", "High"))
test_bal$Ratings  <- factor(test_bal$Ratings, levels = c("0", "1"), labels = c("Low", "High"))

# Check to ensure NAs are gone 
print("Check Test Data:")
print(table(test_bal$Ratings, useNA = "ifany"))

ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     classProbs = TRUE,  # REQUIRED for type = "prob"
                     summaryFunction = twoClassSummary)

rf_tuned <- train(Ratings ~ ., 
                  data = train_bal, 
                  method = "ranger",
                  metric = "ROC",
                  tuneGrid = expand.grid(mtry = c(2, 3, 5),
                                         splitrule = c("gini", "extratrees"),
                                         min.node.size = c(1, 5, 10)),
                  trControl = ctrl)

saveRDS(rf_tuned, file = "rf_tuned_model.rds")
rf_tuned <- readRDS("rf_tuned_model.rds")

# Generate Predictions (For Confusion Matrix)
rf_tuned_pred <- predict(rf_tuned, test_bal)

rf_tuned_conf_matrix <- confusionMatrix(rf_tuned_pred, test_bal$Ratings, positive = "High")  
print(rf_tuned_conf_matrix)

# Probability Prediction (For ROC Plot)
rf_tuned_prob <- predict(rf_tuned, test_bal, type = "prob")[, "High"]

rf_tuned_roc <- roc(response = test_bal$Ratings, predictor = rf_tuned_prob, levels = c("Low", "High"))

grid <- expand.grid(
  eta = c(0.1, 0.2, 0.3),  
  max_depth = c(4, 5, 6),   
  subsample = c(0.8, 1),   
  colsample_bytree = c(0.8, 1),   
  nrounds = c(100, 200),  
  gamma = c(0, 0.1, 0.2),  # Add gamma (controls loss reduction)
  min_child_weight = c(1, 3, 5)  # Add min_child_weight (controls minimum loss reduction)
)
train_label <- factor(train_label, levels = c(0, 1), labels = c("Low", "High"))
set.seed(123)

# Extra Feature 5
# Tuned XGBoost
xgb_tuned <- train(
  x = train_matrix, y = train_label,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
  tuneGrid = grid
)

saveRDS(xgb_tuned, "xgb_tuned_model.rds")
xgb_tuned_model <- readRDS("xgb_tuned_model.rds")
xgb_booster <- xgb_tuned_model$finalModel  # Retrieves the actual XGBoost model
class(xgb_booster)  # Should now be "xgb.Booster"
xgb_booster <- xgb.Booster.complete(xgb_booster)

# Ensure labels are properly set
test_bal$Ratings <- factor(test_bal$Ratings, levels = c("Low", "High"))

# Predict class labels
xgb_tuned_pred <- predict(xgb_tuned_model, newdata = test_matrix)

# Predict probabilities
xgb_tuned_prob <- predict(xgb_tuned_model, newdata = test_matrix, type = "prob")[, "High"]

xgb_tuned_conf_matrix <- confusionMatrix(xgb_tuned_pred, test_bal$Ratings)
print(xgb_tuned_conf_matrix)

xgb_tuned_roc <- roc(test_bal$Ratings, xgb_tuned_prob)

# Extra Feature 6
# AUC (Area Under the Curve)
library(pROC)
# Plot the first ROC curve
plot.roc(xgb_roc, col = "blue", main = "ROC Curves for All Models")

# Add more ROC curves using add=TRUE
plot.roc(rf_roc, col = "red", add = TRUE)
plot.roc(log_roc, col = "green", add = TRUE)
plot.roc(rf_tuned_roc, col = "purple", add = TRUE)
plot.roc(xgb_tuned_roc, col = "orange", add = TRUE)

# Add a legend
legend("bottomright",
       legend = c(paste("XGBoost (AUC:", round(xgb_roc$auc, 3), ")"),
                  paste("Random Forest (AUC:", round(rf_roc$auc, 3), ")"),
                  paste("Logistic Regression (AUC:", round(log_roc$auc, 3), ")"),
                  paste("Tuned RF (AUC:", round(rf_tuned_roc$auc, 3), ")"),
                  paste("Tuned XGBoost (AUC:", round(xgb_tuned_roc$auc, 3), ")")),
       col = c("blue", "red", "green", "purple", "orange"), lwd = 2, cex = 0.7)

# Store the actual levels and numeric ranges from your complete_data
original_gender_levels <- levels(factor(complete_data$Gender))
original_income_levels <- levels(factor(complete_data$Income))
original_country_levels <- levels(factor(complete_data$Country))
original_state_levels <- levels(factor(complete_data$State))
original_customer_segment_levels <- levels(factor(complete_data$Customer_Segment))

# Store the actual min and max for Age
original_age_min <- min(complete_data$Age)
original_age_max <- max(complete_data$Age)

# Final Decision: Tuned XGBoost Model
xgb_tuned_model <- readRDS("xgb_tuned_model.rds")
cat("Tuned XGBoost model loaded for prediction.")

set.seed(456) # for reproducibility of dummy data
num_new_records <- 1000

future_data <- data.frame(
  Age = sample(original_age_min:original_age_max, num_new_records, replace = TRUE),
  Gender = sample(na.omit(as.character(original_gender_levels)), num_new_records, replace = TRUE),
  Income = sample(na.omit(as.character(original_income_levels)), num_new_records, replace = TRUE),
  Country = sample(na.omit(as.character(original_country_levels)), num_new_records, replace = TRUE),
  State = sample(na.omit(as.character(original_state_levels)), num_new_records, replace = TRUE),
  Customer_Segment = sample(na.omit(as.character(original_customer_segment_levels)), num_new_records, replace = TRUE)
)

future_data <- future_data %>%
  mutate(
    Age = ifelse(is.na(Age), sample(original_age_min:original_age_max, 1), Age),
    Gender = ifelse(is.na(Gender), sample(original_gender_levels, 1), Gender),
    Income = ifelse(is.na(Income), sample(original_income_levels, 1), Income),
    Country = ifelse(is.na(Country), sample(original_country_levels, 1), Country),
    State = ifelse(is.na(State), sample(original_state_levels, 1), State),
    Customer_Segment = ifelse(is.na(Customer_Segment), sample(original_customer_segment_levels, 1), Customer_Segment)
  )

future_data$Gender <- factor(future_data$Gender, levels = original_gender_levels)
future_data$Income <- factor(future_data$Income, levels = original_income_levels)
future_data$Country <- factor(future_data$Country, levels = original_country_levels)
future_data$State <- factor(future_data$State, levels = original_state_levels)
future_data$Customer_Segment <- factor(future_data$Customer_Segment, levels = original_customer_segment_levels)

future_data_numeric <- future_data %>%
  mutate(
    Gender = as.numeric(Gender),
    Income = as.numeric(Income),
    Country = as.numeric(Country),
    State = as.numeric(State),
    Customer_Segment = as.numeric(Customer_Segment),
    Age = as.numeric(Age)
  )

future_matrix <- as.matrix(future_data_numeric)
xgb_booster <- xgb_tuned_model$finalModel
future_prob <- predict(xgb_booster, newdata = future_matrix)
future_class <- ifelse(future_prob > 0.5, "High", "Low")
future_class <- factor(future_class, levels = c("Low", "High"))

future_results <- data.frame(
  ID = 1:num_new_records,
  Age = future_data$Age,
  Gender = future_data$Gender,
  Income = future_data$Income,
  Country = future_data$Country,
  State = future_data$State,
  Customer_Segment = future_data$Customer_Segment,
  Predicted_Ratings = future_class,
  Probability = future_prob
)
importance_matrix <- xgb.importance(model = xgb_booster)
print(importance_matrix)

importance_df <- importance_matrix %>%
  top_n(10, wt = Gain) %>% # Select top 10 features based on Gain
  mutate(Feature = factor(Feature, levels = rev(Feature))) # Order features for plotting

# Extra Feature 7
# Feature Importances Plot 
ggplot(importance_df, aes(x = Gain, y = Feature)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") + # Add bars with fill and border
  geom_text(aes(label = sprintf("%.4f", Gain)), # Label with Gain value, formatted to 4 decimal places
            hjust = -0.1, # Position text slightly outside the bar
            color = "black",
            size = 3.5) +
  labs(
    title = "Top 10 Feature Importances (Gain)",
    x = "Gain (Contribution to Model Improvement)",
    y = "Feature"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines for cleaner look
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  ) +
  # Expand x-axis limits to make space for labels
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cor(future_data_numeric, future_results$Probability)

aggregate(Probability ~ Age, data = future_results, FUN = mean)
future_results$AgeGroup <- cut(future_results$Age, 
                               breaks = c(18, 25, 35, 45, 55, 65, 75, 85), 
                               labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-85"),
                               include.lowest = TRUE, right = FALSE)

aggregate(Probability ~ AgeGroup, data = future_results, FUN = function(x) mean(x, na.rm = TRUE))

mean_prob_by_agegroup <- future_results %>%
  group_by(AgeGroup) %>%
  summarise(
    Mean_Probability = mean(Probability, na.rm = TRUE), # Ensure NA values are removed
    .groups = 'drop'
  ) %>%
  mutate(AgeGroup = factor(AgeGroup, levels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-85")))

mean_prob_by_agegroup <- mean_prob_by_agegroup %>%
  mutate(Mean_Probability = ifelse(is.na(Mean_Probability), 0, Mean_Probability))

# Step 2: Create the plot
ggplot(mean_prob_by_agegroup, aes(x = AgeGroup, y = Mean_Probability, fill = AgeGroup)) +
  geom_bar(stat = "identity", color = "black") + # stat="identity" means use y-value as is, add border color
  
  # Add value labels on top of the bars
  geom_text(
    aes(label = scales::percent(Mean_Probability, accuracy = 0.1)), # Format as percentage, 1 decimal place
    vjust = -0.5, # Position text slightly above the bar
    color = "black",
    size = 3.5
  ) +
  
  labs(
    title = "Average Predicted Probability of High Ratings by Age Group",
    x = "Age Group",
    y = "Average Probability of High Rating"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) + # Format y-axis as percentage, expand limits to fit labels
  scale_fill_brewer(palette = "Set3") + # Use a color palette for Age Groups
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "none" # Hide the legend as AgeGroup is on x-axis and filled
  )

# Calculate medians and counts for labels
boxplot_labels <- future_results %>%
  group_by(AgeGroup) %>%
  summarise(
    median_prob = median(Probability, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

ggplot(future_results, aes(x = AgeGroup, y = Probability)) +
  geom_boxplot(aes(fill = AgeGroup), color = "black", alpha = 0.7) + # Add fill for boxes
  # Add median labels
  geom_text(data = boxplot_labels, aes(y = median_prob, label = scales::percent(median_prob, accuracy = 0.1)),
            vjust = -0.8, color = "darkred", size = 3.5, fontface = "bold") +
  # Add count labels (positioned at a fixed y-value for consistency)
  geom_text(data = boxplot_labels, aes(y = 1.05, label = paste0("n=", count)), # Adjust y=1.05 based on your data's max
            vjust = 0, color = "gray30", size = 3) +
  # Add Mean points (Optional - requires mean_prob_by_agegroup to be available)
  geom_point(data = mean_prob_by_agegroup, # Using the summary table from your bar chart
             aes(y = Mean_Probability), # Just map y to Mean_Probability
             color = "red", shape = 8, size = 3, stroke = 1.2) + # Star shape for mean
  labs(
    title = "Predicted Probability Distributions by Age Group",
    x = "Age Group",
    y = "Predicted Probability of High Rating"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.1)) + # Expand limits to fit labels
  scale_fill_brewer(palette = "Set3") + # Use a color palette consistent with your bar chart
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "none" # Hide the legend as AgeGroup is on x-axis and filled
  )
# --- Analysis 3: Gender+Customer_Segments vs. Ratings ---
# How does gender influence customer ratings across different segments, and is this relationship statistically significant?
combined_summary <- complete_data %>%
  group_by(Gender, Customer_Segment) %>%
  summarise(
    Total_Count = n(),
    High_Rating_Count = sum(Ratings == "High"),
    Low_Rating_Count = sum(Ratings == "Low"),
    Proportion_High = mean(Ratings == "High"),
    Proportion_Low = mean(Ratings == "Low"),
    .groups = 'drop'
  ) %>%
  arrange(desc(Proportion_High))
print(combined_summary)

# Convert Ratings to binary (1 for High, 0 for Low)
complete_data$Ratings_Binary <- ifelse(complete_data$Ratings == "High", 1, 0)

# Create a proper contingency table using `xtabs`
combined_ratings_table <- xtabs(Ratings_Binary ~ Gender + Customer_Segment, data = complete_data)

# Perform Chi-Square Test
chi_result_combined <- chisq.test(combined_ratings_table)
print(chi_result_combined)

cramers_v_combined <- sqrt(chi_result_combined$statistic / 
                             (sum(combined_ratings_table) * (min(dim(combined_ratings_table)) - 1)))
print(cramers_v_combined)

gender_ratings_table <- table(complete_data$Gender, complete_data$Ratings)
chi_result_gender <- chisq.test(gender_ratings_table)
print(chi_result_gender)

cramers_v_gender <- sqrt(chi_result_gender$statistic / 
                           (sum(gender_ratings_table) * (min(dim(gender_ratings_table)) - 1)))
print(cramers_v_gender)

segment_ratings_table <- table(complete_data$Customer_Segment, complete_data$Ratings)
chi_result_segment <- chisq.test(segment_ratings_table)
print(chi_result_segment)

cramers_v_segment <- sqrt(chi_result_segment$statistic / 
                            (sum(segment_ratings_table) * (min(dim(segment_ratings_table)) - 1)))
print(cramers_v_segment)

plot_data_segment_gender_labels <- complete_data %>%
  group_by(Gender, Customer_Segment, Ratings) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(Gender, Customer_Segment) %>% 
  mutate(
    proportion = count / sum(count) 
  ) %>%
  ungroup()

ggplot(complete_data, aes(x = Customer_Segment, fill = Ratings)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.2) +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("Low" = "salmon", "High" = "lightgreen")) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Proportion of Customer Ratings by Gender and Segment",
       x = "Customer Segment",
       y = "Proportion",
       fill = "Ratings") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1), # Make x-axis labels angled
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines"),
    legend.title = element_text(face = "bold")
  ) +
  geom_text(
    data = plot_data_segment_gender_labels,
    aes(x = Customer_Segment, y = proportion, label = scales::percent(proportion, accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  )

# Extra feature 8
# Multiple Correspondence Analysis (MCA)
mca_data_combined <- complete_data %>%
  select(Gender, Customer_Segment, Ratings) %>%
  mutate(across(everything(), as.factor))

mca_model_combined <- MCA(mca_data_combined, graph = FALSE)
fviz_mca_var(mca_model_combined,
             repel = TRUE,
             col.var = "red",
             col.ind = "red",
             labels = c("var", "ind"),
             title = "MCA: Association between Gender, Customer Segment, and Ratings",
             labelsize = 3)  # Set numeric size

# Find the top 3 most purchased product brands per segment
top_brands_by_segment <- complete_data %>%
  group_by(Customer_Segment, Product_Brand) %>%
  summarise(Purchase_Count = n(), .groups = "drop") %>%
  group_by(Customer_Segment) %>%
  mutate(Percentage = (Purchase_Count / sum(Purchase_Count)) * 100) %>%
  ungroup() %>%
  arrange(Customer_Segment, desc(Purchase_Count)) %>%
  group_by(Customer_Segment) %>%
  slice_head(n = 3) %>%
  mutate(Percentage = round(Percentage, 2))  # Round for readability
print(top_brands_by_segment)

# Create bar chart with percentage labels
ggplot(top_brands_by_segment, aes(x = reorder(Product_Brand, Percentage), 
                                  y = Percentage, fill = Customer_Segment)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2) +
  facet_wrap(~ Customer_Segment, scales = "free_x") +
  labs(title = "Top Product Brands Purchased by Customer Segment",
       x = "Product Brand",
       y = "Purchase Percentage (%)",
       fill = "Customer Segment") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  ) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 3)

# --- Analysis 4: Geographical Trends in Ratings ---
# How do customer ratings show geographic variation, and are these patterns statistically significant?
state_summary <- complete_data %>%
  group_by(State) %>%
  summarise(
    Total_Count = n(),
    High_Rating_Count = sum(Ratings == "High"),
    Low_Rating_Count = sum(Ratings == "Low"),
    Proportion_High = mean(Ratings == "High"),
    Proportion_Low = mean(Ratings == "Low"),
    Average_Rating_Numeric = mean(as.numeric(Ratings), na.rm = TRUE), # Numeric average (1=Low, 2=High)
    .groups = "drop"
  ) %>%
  arrange(desc(Proportion_High))

country_summary <- complete_data %>%
  group_by(Country) %>%
  summarise(
    Total_Count = n(),
    High_Rating_Count = sum(Ratings == "High"),
    Low_Rating_Count = sum(Ratings == "Low"),
    Proportion_High = mean(Ratings == "High"),
    Proportion_Low = mean(Ratings == "Low"),
    Average_Rating_Numeric = mean(as.numeric(Ratings), na.rm = TRUE), # Numeric average (1=Low, 2=High)
    .groups = "drop"
  ) %>%
  arrange(desc(Proportion_High))

# Print/Display Geographical Results
print(state_summary)
print(country_summary)

# State-Level Ratings Association
state_ratings_table <- table(complete_data$State, complete_data$Ratings)
chi_state <- chisq.test(state_ratings_table)

print("Chi-Square Test Results (State vs Ratings):")
print(chi_state)

cramers_v_state <- sqrt(chi_state$statistic / (sum(state_ratings_table)* (min(dim(state_ratings_table)) - 1)))
print("Cramér's V (Effect Size for State vs Ratings):")
print(cramers_v_state)

# Country-Level Ratings Association
country_ratings_table <- table(complete_data$Country, complete_data$Ratings)
chi_country <- chisq.test(country_ratings_table)

print("Chi-Square Test Results (Country vs Ratings):")
print(chi_country)

cramers_v_country <- sqrt(chi_country$statistic / (sum(country_ratings_table)* (min(dim(country_ratings_table)) - 1)))
print("Cramér's V (Effect Size for Country vs Ratings):")
print(cramers_v_country)

ggplot(country_summary, aes(x = reorder(Country, Proportion_High), y = Proportion_High, fill = Country)) +
  geom_col(show.legend = FALSE) +
  # Add percentage labels to the bars
  geom_text(aes(label = scales::percent(Proportion_High, accuracy = 0.1)), # Format as percentage with one decimal place
            hjust = -0.1, # Adjust horizontal justification to place labels outside the bars (since coord_flip is used)
            size = 3.5, # Adjust label size as needed
            color = "black") + # Set label color
  coord_flip() + # Flip for better readability
  labs(title = "Proportion of High Ratings by Country",
       x = "Country",
       y = "Proportion High Ratings") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + # Format y-axis labels as percentages
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
    axis.text.y = element_text(size = 9) # Adjust country name text size
  )

# Get World Map Spatial Data
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Clean and Join your country_summary data with world_sf
# Harmonize country names (Revised for precise matching with rnaturalearth data)
country_summary_harmonized <- country_summary %>%
  mutate(Country_Spatial = case_when(
    # Correct mapping for US: "Usa" from your data to "United States of America" in world_sf
    Country == "Usa" ~ "United States of America",
    
    # Correct mapping for UK: "Uk" from your data to "United Kingdom",
    Country == "Uk" ~ "United Kingdom",
    
    # Add other common variations if they exist in your data (e.g., all caps)
    Country == "USA" ~ "United States of America", # If you also have "USA" in your data
    Country == "UK" ~ "United Kingdom",           # If you also have "UK" in your data
    
    # Keep original if no specific mapping
    TRUE ~ as.character(Country)
  ))

# Join your summary data with the spatial data
world_data_for_map <- world_sf %>%
  left_join(country_summary_harmonized, by = c("name" = "Country_Spatial"))

# --- DIAGNOSTIC STEPS (reference) ---
cat("--- Diagnosing Country Map Data ---")
print("Unique Countries in your 'country_summary' data:")
print(unique(country_summary$Country))
print("Unique Countries in your 'country_summary_harmonized$Country_Spatial' after harmonization:")
print(unique(country_summary_harmonized$Country_Spatial))
print("Unique Countries in 'world_sf$name':")
print(unique(world_sf$name))
print("Countries from your data present in 'world_data_for_map' (with Proportion_High):")
print(world_data_for_map %>%
        st_drop_geometry() %>%
        select(name, Proportion_High, Total_Count) %>%
        filter(!is.na(Proportion_High)))
print("Countries from your data that FAILED to join (their data is not in world_data_for_map after filter):")
missing_from_map <- anti_join(country_summary_harmonized,
                              world_data_for_map %>% st_drop_geometry(),
                              by = c("Country_Spatial" = "name"))
print(missing_from_map)
cat("--- End Diagnosis ---")
# --- END DIAGNOSTIC STEPS ---

# Extra Feature 9
# Leaflet Map
# 1.3 Create the Leaflet Map for Countries
if (nrow(world_data_for_map) > 0) {
  
  data_proportion_high_domain <- range(country_summary_harmonized$Proportion_High, na.rm = TRUE)
  
  pal <- colorNumeric(
    palette = "YlOrRd", # Yellow-Green-Blue, good for sequential data
    domain = data_proportion_high_domain, # Use the actual data range as the domain
    na.color = "transparent" # --- KEY CHANGE: Countries with no data will be transparent ---
  )
  
  # Create labels for tooltips
  map_labels <- paste(
    "<b>Country:</b>", world_data_for_map$name, "<br/>",
    "<b>Proportion High Ratings:</b>", ifelse(is.na(world_data_for_map$Proportion_High),
                                              "No data", # Display "No data" for NA values
                                              scales::percent(world_data_for_map$Proportion_High, accuracy = 0.1)), "<br/>",
    "<b>Total Ratings:</b>", ifelse(is.na(world_data_for_map$Total_Count),
                                    "No data", # Display "No data" for NA values
                                    world_data_for_map$Total_Count)
  ) %>% lapply(htmltools::HTML)
  
  country_map <- leaflet(world_data_for_map) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(Proportion_High),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = map_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = data_proportion_high_domain, # Legend should also reflect only the data domain
      opacity = 0.7,
      title = "Proportion High Ratings",
      position = "bottomright",
      labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
    ) %>%
    addControl(
      html = "<h3>Proportion of High Ratings by Country</h3>",
      position = "topleft"
    )
  
  print(country_map)
} else {
  print("No country data available after joining with spatial data to create map.")
}

us_states_sf <- states(cb = TRUE, class = "sf", year = 2023)

us_states_sf <- us_states_sf %>%
  filter(!NAME %in% c("Puerto Rico", "U.S. Virgin Islands", "Guam",
                      "American Samoa", "Commonwealth of the Northern Mariana Islands")) %>%
  select(NAME, geometry) %>%
  rename(region = NAME) %>%
  # Ensure the CRS is WGS84 (EPSG:4326) for Leaflet compatibility
  st_transform(4326)

# --- Join your state_summary data with us_states_sf ---
# Assume state_summary has a column named 'State' with full state names like "Alabama", "California"
states_data_for_map <- us_states_sf %>%
  left_join(state_summary, by = c("region" = "State"))

states_data_for_map_filtered <- states_data_for_map %>%
  filter(!is.na(Proportion_High)) # Only include states you have data for

# --- 2.3 Create the Leaflet Map for States ---
if (nrow(states_data_for_map) > 0) {
  state_proportion_high_domain <- range(states_data_for_map$Proportion_High, na.rm = TRUE)
  
  state_pal <- colorNumeric(
    palette = "YlGnBu", # You can choose a different palette here if desired
    domain = state_proportion_high_domain,
    na.color = "#000000" # Light gray for states with no data
  )
  
  # Create labels for tooltips for states
  state_map_labels <- paste(
    "<b>State:</b>", states_data_for_map$region, "<br/>",
    "<b>Proportion High Ratings:</b>", ifelse(is.na(states_data_for_map$Proportion_High),
                                              "No data",
                                              scales::percent(states_data_for_map$Proportion_High, accuracy = 0.1)), "<br/>",
    "<b>Total Ratings:</b>", ifelse(is.na(states_data_for_map$Total_Count),
                                    "No data",
                                    states_data_for_map$Total_Count)
  ) %>% lapply(htmltools::HTML)
  
  state_map <- leaflet(states_data_for_map) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~state_pal(Proportion_High),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = state_map_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = state_pal,
      values = state_proportion_high_domain,
      opacity = 0.7,
      title = "Proportion High Ratings by State",
      position = "bottomright",
      labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
    ) %>%
    addControl(
      html = "<h3>Proportion of High Ratings by US State</h3>",
      position = "topleft"
    )
  
  print(state_map)
} else {
  print("No state data available after joining with spatial data to create map.")
}
