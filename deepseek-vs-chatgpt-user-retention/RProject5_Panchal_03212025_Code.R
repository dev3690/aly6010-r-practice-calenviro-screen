# Module 5: AI Performance Analysis
library(tidyverse)
library(corrplot)
library(ggplot2)
library(knitr)
library(gridExtra)
library(lubridate)

# Data Import and Cleaning
ai_data <- read.csv("deepseek_vs_chatgpt.csv")

# Data Cleaning
ai_data <- ai_data %>%
  # Remove any duplicate entries
  distinct() %>%
  # Handle missing values
  drop_na() %>%
  # Ensure numeric variables are properly formatted
  mutate(
    Response_Accuracy = as.numeric(Response_Accuracy),
    User_Return_Frequency = as.numeric(User_Return_Frequency),
    Retention_Rate = as.numeric(Retention_Rate),
    Daily_Churn_Rate = as.numeric(Daily_Churn_Rate),
    Active_Users = as.numeric(Active_Users)
  ) %>%
  # Ensure categorical variables are factors
  mutate(
    AI_Platform = as.factor(AI_Platform),
    Query_Type = as.factor(Query_Type),
    Weekday = as.factor(Weekday)
  ) %>%
  # Remove outliers (values beyond 3 standard deviations)
  filter(
    abs(scale(Response_Accuracy)) <= 3,
    abs(scale(User_Return_Frequency)) <= 3,
    abs(scale(Retention_Rate)) <= 3,
    abs(scale(Daily_Churn_Rate)) <= 3,
    abs(scale(Active_Users)) <= 3
  )

# Print summary of cleaned data
print("Summary of cleaned dataset:")
summary(ai_data)

# Check for any remaining missing values
print("\nMissing values after cleaning:")
colSums(is.na(ai_data))

# Focused Correlation Analysis 
key_metrics_cor <- ai_data %>%
  select(Response_Accuracy, User_Return_Frequency, Retention_Rate, 
         Daily_Churn_Rate, Active_Users) %>%
  cor(use = "complete.obs")

# Print correlation matrix to console
print("Key Performance Metrics Correlation:")
print(key_metrics_cor)

# Enhanced Correlation Visualization
png("key_metrics_correlation.png", width = 800, height = 800, res = 120)
corrplot(key_metrics_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Key Performance Metrics Correlation",
         # Add these improvements
         diag = FALSE,  # Remove diagonal (self-correlations)
         col = colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                                 "#F4A582", "#FDDBC7", "#FFFFFF", 
                                 "#D1E5F0", "#92C5DE", "#4393C3", 
                                 "#2166AC", "#053061"))(200),  # Better color scheme
         mar = c(0,0,1,0),  # Adjust margins
         tl.cex = 0.8,      # Adjust text size
         cl.cex = 0.8)      # Adjust legend text size
dev.off()

# Regression Analysis
retention_model <- lm(Retention_Rate ~ Response_Accuracy + User_Return_Frequency + 
                     Daily_Churn_Rate + factor(AI_Platform), data = ai_data)

# Print regression results to console
print("Regression Analysis: Predicting Retention Rate")
print(summary(retention_model))

# Regression Visualization
ggplot(ai_data, aes(x = Response_Accuracy, y = Retention_Rate, color = AI_Platform)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Retention Rate vs Response Accuracy by Platform",
       x = "Response Accuracy",
       y = "Retention Rate")
ggsave("retention_regression_plot.png", width = 10, height = 6)

# Diagnostic Plots for Regression
png("regression_diagnostics.png", width = 1200, height = 800, res = 120)
par(mfrow = c(2,2))
plot(retention_model)
dev.off()

# Platform Comparison Analysis
platform_metrics <- ai_data %>%
  group_by(AI_Platform) %>%
  summarise(
    Avg_Response_Accuracy = mean(Response_Accuracy, na.rm = TRUE),
    Avg_User_Return = mean(User_Return_Frequency, na.rm = TRUE),
    Avg_Retention_Rate = mean(Retention_Rate, na.rm = TRUE)
  )

# Technical Query Analysis
technical_accuracy <- ai_data %>%
  filter(Query_Type == "Technical") %>%
  group_by(AI_Platform) %>%
  summarise(
    Tech_Response_Accuracy = mean(Response_Accuracy, na.rm = TRUE)
  )

# Print platform comparison results to console
print("\nPlatform Performance Metrics:")
print(platform_metrics)
print("\nTechnical Query Accuracy by Platform:")
print(technical_accuracy)
