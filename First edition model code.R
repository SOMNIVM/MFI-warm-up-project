library(dplyr)       
library(ggplot2)      
library(corrplot)     
library(GGally)      
library(tidyr)        
library(caret)       
library(randomForest) 
library(car)

rm(list = ls())

data <- read.csv("~/Desktop/MFI /Prep Project/premium_data.csv", header = TRUE)

data_clean <- na.omit(data)

data_clean$Smoking.Status <- as.factor(trimws(data_clean$Smoking.Status))

data_clean$Gender <- as.factor(data_clean$Gender)
data_clean$Region <- as.factor(data_clean$Region)
data_clean$Educational.Level <- as.factor(data_clean$Educational.Level)
data_clean$Age_Groups <- as.factor(data_clean$Age_Groups)
data_clean$Income_Level <- as.factor(data_clean$Income_Level)
data_clean$Credit_Category <- as.factor(data_clean$Credit_Category)
data_clean$Pre.existing.Conditions <- as.factor(data_clean$Pre.existing.Conditions)
data_clean$Family.Medical.History <- as.factor(data_clean$Family.Medical.History)


cat_vars <- c("Smoking.Status", "Gender", "Region", "Educational.Level", 
              "Age_Groups", "Income_Level", "Credit_Category", 
              "Pre.existing.Conditions", "Family.Medical.History")

cat_data <- data_clean[, c("Premium.Amount", cat_vars)]

cat_long <- cat_data %>%
  pivot_longer(cols = all_of(cat_vars), 
               names_to = "Variable", 
               values_to = "Category")
ggplot(cat_long, aes(x = Category, y = Premium.Amount, fill = Category)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Premium Amount by Categorical Variables",
       x = "Category", y = "Premium Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

num_vars <- data_clean %>% select_if(is.numeric)

corrplot(cor(num_vars), method = "color", type = "lower", tl.cex = 0.8)

model_data <- data_clean %>%
  select(Premium.Amount, Age, BMI, Credit.Score, Sum.Insured, High_Risk,
         Smoking.Status, Pre.existing.Conditions, Family.Medical.History, Income_Level)

model_data$Smoking.Status <- as.factor(model_data$Smoking.Status)
model_data$Pre.existing.Conditions <- as.factor(model_data$Pre.existing.Conditions)
model_data$Family.Medical.History <- as.factor(model_data$Family.Medical.History)
model_data$Income_Level <- as.factor(model_data$Income_Level)

str(model_data)

set.seed(888)

train_index <- createDataPartition(model_data$Premium.Amount, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

model_lm <- lm(Premium.Amount ~ ., data = train_data)
summary(model_lm)

par(mfrow = c(2, 2))
plot(model_lm)

vif(model_lm)

model_rf <- randomForest(Premium.Amount ~ ., data = train_data, importance = TRUE)

varImpPlot(model_rf)

pred_lm <- predict(model_lm, newdata = test_data)

rmse_lm <- sqrt(mean((pred_lm - test_data$Premium.Amount)^2))
mae_lm <- mean(abs(pred_lm - test_data$Premium.Amount))

cat("Linear Model RMSE:", rmse_lm, "\nMAE:", mae_lm, "\n")

pred_rf <- predict(model_rf, newdata = test_data)

rmse_rf <- sqrt(mean((pred_rf - test_data$Premium.Amount)^2))
mae_rf <- mean(abs(pred_rf - test_data$Premium.Amount))

cat("Random Forest RMSE:", rmse_rf, "\nMAE:", mae_rf, "\n")

df_compare <- data.frame(Actual = test_data$Premium.Amount, Predicted = pred_rf)

ggplot(df_compare, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, col = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Premium Amount (Random Forest)",
       x = "Actual Premium", y = "Predicted Premium") +
  theme_minimal()
results <- data.frame(
  Model = c("Linear Model", "Random Forest"),
  RMSE = c(rmse_lm, rmse_rf),
  MAE = c(mae_lm, mae_rf)
)

print(results)

