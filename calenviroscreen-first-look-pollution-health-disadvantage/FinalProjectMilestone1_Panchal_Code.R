# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr) 
library(scales)
library(gridExtra)
library(grid)

# Read the data
data <- read.csv("calenviroscreen.csv", sep=";", header=TRUE)

# Enhanced Data Cleaning
# 1. Remove any rows with missing values
data_clean <- na.omit(data)

# 2. Convert percentage strings to numeric values
data_clean$CES.3.0.Percentile <- as.numeric(gsub("[^0-9.]", "", data_clean$CES.3.0.Percentile))

# 3. Clean numeric columns by removing commas and converting to numeric
numeric_cols <- c("Total.Population", "CES.3.0.Score", "Ozone", "PM2.5", "Diesel.PM", 
                  "Drinking.Water", "Pesticides", "Tox..Release", "Traffic", "Cleanup.Sites",
                  "Groundwater.Threats", "Haz..Waste", "Imp..Water.Bodies", "Solid.Waste",
                  "Pollution.Burden", "Pollution.Burden.Score", "Asthma", "Low.Birth.Weight",
                  "Cardiovascular.Disease", "Education", "Linguistic.Isolation", "Poverty",
                  "Unemployment", "Housing.Burden")

for(col in numeric_cols) {
  data_clean[[col]] <- as.numeric(gsub(",", ".", data_clean[[col]]))
}

# 4. Remove outliers using IQR method for key health and socioeconomic indicators
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE)
  H <- 1.5 * IQR(x, na.rm=TRUE)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)
}

key_indicators <- c("Asthma", "Cardiovascular.Disease", "Low.Birth.Weight", 
                    "Education", "Poverty", "Pollution.Burden.Score")

for(col in key_indicators) {
  data_clean[[col]] <- remove_outliers(data_clean[[col]])
}

# 5. Remove any new NA values created by outlier removal
data_clean <- na.omit(data_clean)

# 6. Check for unreasonable values
data_clean <- data_clean %>%
  filter(
    Asthma >= 0 & Asthma <= 100,
    Cardiovascular.Disease >= 0 & Cardiovascular.Disease <= 100,
    Low.Birth.Weight >= 0 & Low.Birth.Weight <= 100,
    Education >= 0 & Education <= 100,
    Poverty >= 0 & Poverty <= 100,
    Pollution.Burden.Score >= 0 & Pollution.Burden.Score <= 100
  )

# Print data quality summary
cat("Original number of rows:", nrow(data), "\n")
cat("Number of rows after cleaning:", nrow(data_clean), "\n")
cat("Percentage of data retained:", round(nrow(data_clean)/nrow(data)*100, 2), "%\n\n")

# Save cleaned data to new CSV file
write.csv(data_clean, "calenviroscreen_cleaned.csv", row.names = FALSE)
# Print confirmation
cat("Cleaned data saved to 'calenviroscreen_cleaned.csv'\n")

# Create summary table
summary_table <- data_clean %>%
  summarise(
    across(c(CES.3.0.Score, Pollution.Burden.Score, Asthma, 
             Cardiovascular.Disease, Low.Birth.Weight, Education, 
             Poverty, Unemployment),
           list(
             Mean = ~mean(., na.rm = TRUE),
             Median = ~median(., na.rm = TRUE),
             SD = ~sd(., na.rm = TRUE),
             Min = ~min(., na.rm = TRUE),
             Max = ~max(., na.rm = TRUE)
           )
    )
  ) %>%
  t() %>%
  as.data.frame()

# Create directory for plots
dir.create("plots", showWarnings = FALSE)

# 1. Histogram: Distribution of Health Indicators
hist_plot <- ggplot(data_clean) +
  geom_histogram(aes(x=Asthma, fill="Asthma"), alpha=0.5, bins=30) +
  geom_histogram(aes(x=Cardiovascular.Disease, fill="Cardiovascular Disease"), alpha=0.5, bins=30) +
  geom_histogram(aes(x=Low.Birth.Weight, fill="Low Birth Weight"), alpha=0.5, bins=30) +
  labs(title="Distribution of Health Indicators",
       x="Score",
       y="Frequency",
       fill="Health Indicators") +
  theme_minimal()

# 2. Scatter plot: Education vs Poverty with CES Score
scatter_plot <- ggplot(data_clean) +
  geom_point(aes(x=Education, y=Poverty, color=CES.3.0.Score), alpha=0.6) +
  scale_color_gradient(low="blue", high="red") +
  geom_smooth(aes(x=Education, y=Poverty), method="lm", color="black") +
  labs(title="Education vs Poverty with CES 3.0 Score",
       x="Education Score",
       y="Poverty Score",
       color="CES 3.0 Score") +
  theme_minimal()

# 3. Boxplot: Health Indicators by Disadvantaged Community Status
data_clean_long <- data_clean %>%
  pivot_longer(cols=c(Asthma, Cardiovascular.Disease, Low.Birth.Weight),
               names_to="Health_Indicator",
               values_to="Value")

box_plot <- ggplot(data_clean_long, aes(x=Health_Indicator, y=Value, fill=SB.535.Disadvantaged.Community)) +
  geom_boxplot() +
  labs(title="Health Indicators by Community Status",
       x="Health Indicator",
       y="Value",
       fill="Disadvantaged Community") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Line graph: Pollution Burden vs Health Indicators
line_plot <- ggplot(data_clean, aes(x=Pollution.Burden.Score)) +
  geom_line(aes(y=Asthma, color="Asthma"), stat="smooth", method="loess") +
  geom_line(aes(y=Cardiovascular.Disease, color="Cardiovascular Disease"), stat="smooth", method="loess") +
  geom_line(aes(y=Low.Birth.Weight, color="Low Birth Weight"), stat="smooth", method="loess") +
  labs(title="Pollution Burden vs Health Indicators Trend",
       x="Pollution Burden Score",
       y="Health Indicator Score",
       color="Health Indicators") +
  theme_minimal()

# Save all plots
ggsave("plots/health_distribution.png", 
       plot = hist_plot, width = 10, height = 6)

ggsave("plots/education_poverty_scatter.png", 
       plot = scatter_plot, width = 10, height = 6)

ggsave("plots/health_indicators_boxplot.png", 
       plot = box_plot, width = 10, height = 6)

ggsave("plots/pollution_health_trend.png", 
       plot = line_plot, width = 10, height = 6)

# Display summary table in console
print(summary_table)