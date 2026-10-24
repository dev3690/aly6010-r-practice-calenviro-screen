# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Read the data
data <- read.csv("foodAfford.csv")

# Clean data - remove empty regions
data <- data %>% filter(region_name != "")



# Group by race descriptive statistics
race_stats <- data %>%
  filter(region_name != "") %>%  # Remove empty region names
  group_by(race_eth_name) %>%
  summarise(
    n = sum(!is.na(median_income)),
    mean_income = mean(median_income, na.rm = TRUE),
    sd_income = sd(median_income, na.rm = TRUE),
    mean_ratio = mean(affordability_ratio, na.rm = TRUE),
    sd_ratio = sd(affordability_ratio, na.rm = TRUE)
  )

cat("\nRacial Group Descriptive Statistics:\n")
print(race_stats)




# 1. SCATTER PLOT
png("scatter_plot.png", width=800, height=600)
par(mar=c(5,5,4,2)) # Set margins (bottom, left, top, right)

# Create scatter plot
plot(data$median_income, data$affordability_ratio,
     xlab="Median Income ($)",
     ylab="Affordability Ratio",
     main="Income vs Food Affordability",
     pch=19,  # Point type
     col=rgb(0,0,1,0.5),  # Semi-transparent blue
     cex.lab=1.2,  # Larger axis labels
     cex.main=1.4)  # Larger title

# Add trend line
model <- lm(affordability_ratio ~ median_income, data=data)
abline(model, col="red", lwd=2)

# Add grid
grid()

dev.off()

# 2. JITTER PLOT
png("jitter_plot.png", width=1000, height=600)

ggplot(data, aes(x=region_name, y=affordability_ratio)) +
  geom_jitter(width=0.2, alpha=0.6, color="darkblue", size=2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=10),
    axis.text.y = element_text(size=10),
    axis.title = element_text(size=12, face="bold"),
    plot.title = element_text(size=14, face="bold", hjust=0.5)
  ) +
  labs(
    title="Food Affordability Ratio by Region",
    x="Region",
    y="Affordability Ratio"
  )

dev.off()

# 3. BOX PLOT
png("boxplot.png", width=1000, height=600)
par(mar=c(10,5,4,2)) # Adjust margins for long labels

# Create color palette
colors <- brewer.pal(length(unique(data$region_name)), "Set3")

# Create boxplot
boxplot(affordability_ratio ~ region_name, 
        data=data,
        main="Food Affordability Distribution by Region",
        xlab="",
        ylab="Affordability Ratio",
        las=2,  # Rotate x-axis labels
        col=colors,
        border="darkgray",
        outpch=16,  # Outlier point type
        outcol="red")  # Outlier color

# Add grid
grid(nx=NA, ny=NULL)

dev.off()

# Generate Summary Statistics
summary_stats <- data %>%
  group_by(region_name) %>%
  summarise(
    n = n(),
    min_ratio = min(affordability_ratio, na.rm=TRUE),
    q1_ratio = quantile(affordability_ratio, 0.25, na.rm=TRUE),
    median_ratio = median(affordability_ratio, na.rm=TRUE),
    mean_ratio = mean(affordability_ratio, na.rm=TRUE),
    q3_ratio = quantile(affordability_ratio, 0.75, na.rm=TRUE),
    max_ratio = max(affordability_ratio, na.rm=TRUE),
    sd_ratio = sd(affordability_ratio, na.rm=TRUE)
  ) %>%
  arrange(desc(median_ratio))

# Print summary statistics
print(summary_stats)

# Calculate correlation
correlation <- cor.test(data$median_income, data$affordability_ratio)
print(correlation)
# Calculate outlier statistics with explicit NA handling
outlier_analysis <- data %>%
  group_by(region_name) %>%
  summarise(
    n_total = n(),
    n_valid = sum(!is.na(affordability_ratio)),
    q1 = quantile(affordability_ratio, 0.25, na.rm = TRUE),
    q3 = quantile(affordability_ratio, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    outliers = sum(affordability_ratio > (q3 + 1.5*iqr) | 
                     affordability_ratio < (q1 - 1.5*iqr), 
                   na.rm = TRUE)
  ) %>%
  arrange(desc(outliers))

# Display outlier analysis results
knitr::kable(outlier_analysis,
             caption = "Outlier Analysis by Region",
             col.names = c("Region", "Total N", "Valid N", "Q1", "Q3", "IQR", "# Outliers"))

# Statistical Summary 
summary_stats <- data %>%
  group_by(region_name) %>%
  summarise(
    n_total = n(),
    n_valid = sum(!is.na(affordability_ratio)),
    mean_ratio = round(mean(affordability_ratio, na.rm = TRUE), 3),
    median_ratio = round(median(affordability_ratio, na.rm = TRUE), 3),
    sd_ratio = round(sd(affordability_ratio, na.rm = TRUE), 3)
  ) %>%
  arrange(desc(mean_ratio))

# Display summary statistics
knitr::kable(summary_stats, 
             caption = "Regional Food Affordability Statistics",
             col.names = c("Region", "Total N", "Valid N", "Mean Ratio", "Median Ratio", "Std Dev"))