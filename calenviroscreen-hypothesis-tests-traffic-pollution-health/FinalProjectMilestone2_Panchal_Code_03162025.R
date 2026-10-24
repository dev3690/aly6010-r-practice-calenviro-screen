# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stats)

# Read and clean the original data
data <- read.csv("calenviroscreen.csv", sep=";")
# Data cleaning steps
data_clean <- data %>%
  na.omit() %>% 
  mutate(
    CES.3.0.Percentile = as.numeric(gsub("[^0-9.]", "", CES.3.0.Percentile))
  )

# Clean numeric columns
numeric_cols <- c("Total.Population", "CES.3.0.Score", "Ozone", "PM2.5", "Diesel.PM", 
                  "Drinking.Water", "Pesticides", "Tox..Release", "Traffic", "Cleanup.Sites",
                  "Groundwater.Threats", "Haz..Waste", "Imp..Water.Bodies", "Solid.Waste",
                  "Pollution.Burden", "Pollution.Burden.Score", "Asthma", "Low.Birth.Weight",
                  "Cardiovascular.Disease", "Education", "Linguistic.Isolation", "Poverty",
                  "Unemployment", "Housing.Burden")

for(col in numeric_cols) {
  data_clean[[col]] <- as.numeric(gsub(",", ".", data_clean[[col]]))
}

# Remove outliers function
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE)
  H <- 1.5 * IQR(x, na.rm=TRUE)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)
}

# Apply outlier removal to key indicators
key_indicators <- c("Asthma", "Cardiovascular.Disease", "Low.Birth.Weight", 
                    "Education", "Poverty", "Pollution.Burden.Score")

for(col in key_indicators) {
  data_clean[[col]] <- remove_outliers(data_clean[[col]])
}

# Remove NA values and unreasonable values
data_clean <- data_clean %>%
  na.omit() %>%
  filter(
    Asthma >= 0 & Asthma <= 100,
    Cardiovascular.Disease >= 0 & Cardiovascular.Disease <= 100,
    Low.Birth.Weight >= 0 & Low.Birth.Weight <= 100,
    Education >= 0 & Education <= 100,
    Poverty >= 0 & Poverty <= 100,
    Pollution.Burden.Score >= 0 & Pollution.Burden.Score <= 100
  )

# Question 1: One-Sample Test
# Is the average Pollution Burden Score significantly different from 75?

# Perform one-sample t-test for Pollution Burden Score
pollution_test <- t.test(data_clean$Pollution.Burden.Score, mu = 75)

# Create visualization for Pollution Burden Score
pollution_hist <- ggplot(data_clean, aes(x = Pollution.Burden.Score)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  geom_vline(xintercept = 75, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(data_clean$Pollution.Burden.Score), 
             color = "darkgreen", size = 1) +
  geom_vline(xintercept = pollution_test$conf.int[1], 
             color = "darkgreen", linetype = "dotted") +
  geom_vline(xintercept = pollution_test$conf.int[2], 
             color = "darkgreen", linetype = "dotted") +
  annotate("text", x = 75, y = max(hist(data_clean$Pollution.Burden.Score, plot = FALSE)$counts), 
           label = "Threshold (75)", color = "red", vjust = -1) +
  annotate("text", x = mean(data_clean$Pollution.Burden.Score), 
           y = max(hist(data_clean$Pollution.Burden.Score, plot = FALSE)$counts), 
           label = sprintf("Mean: %.2f", mean(data_clean$Pollution.Burden.Score)), 
           color = "darkgreen", vjust = -1) +
  labs(title = "Distribution of Pollution Burden Scores",
       subtitle = "Comparing Community Scores with Regulatory Threshold",
       x = "Pollution Burden Score",
       y = "Number of Communities",
       caption = "Green line: Mean score | Red line: Regulatory threshold | Dotted lines: 95% CI") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Question 2: Two-Sample Test for Asthma Rates
# Compare Asthma rates between disadvantaged and non-disadvantaged communities

# Perform two-sample t-test for Asthma rates
asthma_test <- t.test(Asthma ~ SB.535.Disadvantaged.Community, data = data_clean)

# Create visualization for Asthma comparison
asthma_box <- ggplot(data_clean, aes(x = SB.535.Disadvantaged.Community, y = Asthma)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Asthma Rates by Community Status",
       x = "Disadvantaged Community Status",
       y = "Asthma Rate")

# Question 3: Two-Sample Test for Education and Poverty
# Compare poverty rates between high and low education communities

# Create education groups based on median
education_median <- median(data_clean$Education)
data_clean$Education.Group <- ifelse(data_clean$Education > education_median, 
                                   "High Education", "Low Education")

# Perform two-sample t-test for Poverty rates
poverty_test <- t.test(Poverty ~ Education.Group, data = data_clean)

# Create visualization for Education and Poverty relationship
education_poverty_box <- ggplot(data_clean, aes(x = Education.Group, y = Poverty)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Poverty Rates by Education Level",
       x = "Education Level",
       y = "Poverty Rate")

# Save test results
test_results <- list(
  pollution_burden = pollution_test,
  asthma_community = asthma_test,
  poverty_education = poverty_test
)

# Save visualizations
ggsave("plots/pollution_burden_hist.png", pollution_hist, width = 10, height = 6)
ggsave("plots/asthma_community_box.png", asthma_box, width = 10, height = 6)
ggsave("plots/education_poverty_box.png", education_poverty_box, width = 10, height = 6)

# Print test results
cat("\nPollution Burden Score Test Results:\n")
print(pollution_test)

cat("\nAsthma Rates by Community Status Test Results:\n")
print(asthma_test)

cat("\nPoverty Rates by Education Level Test Results:\n")
print(poverty_test)

# Additional summary statistics
summary_stats <- data_clean %>%
  group_by(SB.535.Disadvantaged.Community) %>%
  summarise(
    mean_asthma = mean(Asthma),
    sd_asthma = sd(Asthma),
    mean_pollution = mean(Pollution.Burden.Score),
    sd_pollution = sd(Pollution.Burden.Score)
  )

print("\nSummary Statistics by Community Status:")
print(summary_stats)

# Create correlation matrix for health and environmental indicators
correlation_vars <- data_clean %>%
  select(Pollution.Burden.Score, Asthma, Cardiovascular.Disease, 
         Low.Birth.Weight, Education, Poverty)
cor_matrix <- cor(correlation_vars)

# Create heatmap for correlations
correlation_heatmap <- ggplot(data = as.data.frame(as.table(cor_matrix)),
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Key Indicators",
       x = "", y = "", fill = "Correlation")

# Create heatmap for disadvantaged communities
community_heatmap <- data_clean %>%
  group_by(SB.535.Disadvantaged.Community) %>%
  summarise(
    Mean_Asthma = mean(Asthma),
    Mean_Cardiovascular = mean(Cardiovascular.Disease),
    Mean_LowBirthWeight = mean(Low.Birth.Weight),
    Mean_Pollution = mean(Pollution.Burden.Score)
  ) %>%
  pivot_longer(cols = starts_with("Mean_"), 
               names_to = "Indicator", 
               values_to = "Value") %>%
  ggplot(aes(x = SB.535.Disadvantaged.Community, y = Indicator, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Health Indicators by Community Status",
       x = "Disadvantaged Community Status",
       y = "Health Indicator",
       fill = "Mean Value")

# Save the new heatmaps
ggsave("plots/correlation_heatmap.png", correlation_heatmap, width = 10, height = 8)
ggsave("plots/community_heatmap.png", community_heatmap, width = 10, height = 6)
