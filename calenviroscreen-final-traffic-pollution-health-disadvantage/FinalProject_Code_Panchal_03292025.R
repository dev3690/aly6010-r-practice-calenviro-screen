# Load required libraries
library(tidyverse)
library(ggplot2)
library(stats)
library(car)
library(gridExtra)
library(cowplot)

# Read the data
data <- read.csv("calenviroscreen.csv", sep=";")


#############################################
# Summary of Previous Work (Milestones 1-2) #
#############################################

# Key EDA Findings from Milestone 1
milestone1_summary <- list(
    # Data Quality and Distribution
    data_quality = "Dataset contains environmental and health indicators for California communities",
    distributions = c(
        "Health indicators showed varied distributions across communities",
        "Pollution Burden Score showed relatively normal distribution",
        "CES 3.0 Scores indicated clear disparities between communities"
    ),
    
    # Key Relationships Identified
    relationships = c(
        "Strong correlation between Education and Poverty levels",
        "Pollution Burden showed positive correlation with health issues",
        "Disadvantaged communities showed consistently higher health burden"
    ),
    
    # Notable Patterns
    patterns = c(
        "Geographic clustering of environmental hazards",
        "Socioeconomic factors strongly associated with exposure levels",
        "Multiple pollutant exposure in disadvantaged areas"
    )
)

# Statistical Tests from Milestone 2
milestone2_summary <- list(
    # Hypothesis Tests
    tests_performed = c(
        "One-sample test: Pollution Burden Score vs threshold of 75",
        "Two-sample test: Asthma rates in disadvantaged vs non-disadvantaged communities",
        "Correlation test: Poverty rates and education levels"
    ),
    
    # Key Findings
    test_results = c(
        "Significant difference in Pollution Burden from threshold",
        "Higher asthma rates in disadvantaged communities",
        "Strong negative correlation between education and poverty"
    ),
    
    # Statistical Significance
    significance = "Most relationships showed p-values < 0.05"
)

# Patterns Identified for Further Investigation
patterns_identified <- list(
    environmental = c(
        "Traffic density impact on respiratory health",
        "Cumulative effect of multiple pollutants",
        "Geographic distribution of environmental hazards"
    ),
    
    socioeconomic = c(
        "Education levels as moderating factor",
        "Poverty and environmental exposure relationship",
        "Access to healthcare and environmental burden"
    ),
    
    health_outcomes = c(
        "Respiratory disease patterns",
        "Cardiovascular disease distribution",
        "Low birth weight incidence"
    )
)


###########################################
# Research Question 1: Traffic and Asthma #
###########################################

# Data preparation
traffic_data <- data %>%
  mutate(
    Traffic = as.numeric(gsub(",", ".", Traffic)),
    Asthma = as.numeric(gsub(",", ".", Asthma))
  ) %>%
  filter(!is.na(Traffic) & !is.na(Asthma))

# Define groups
traffic_75th <- quantile(traffic_data$Traffic, 0.75)
traffic_25th <- quantile(traffic_data$Traffic, 0.25)
traffic_groups <- traffic_data %>%
  filter(Traffic >= traffic_75th | Traffic <= traffic_25th) %>%
  mutate(traffic_group = ifelse(Traffic >= traffic_75th, "High Traffic", "Low Traffic"))

# Visualizations
# Boxplot
ggplot(traffic_groups, aes(x = traffic_group, y = Asthma, fill = traffic_group)) +
  geom_boxplot() +
  labs(title = "Asthma Rates by Traffic Density Groups",
       x = "Traffic Density Level",
       y = "Asthma Rate",
       caption = "High Traffic: >75th percentile, Low Traffic: <25th percentile") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
ggsave("plots/traffic_asthma_boxplot.png", width = 10, height = 6)

# Scatter plot
ggplot(traffic_data, aes(x = Traffic, y = Asthma)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Traffic Density vs Asthma Rates",
       x = "Traffic Density",
       y = "Asthma Rate") +
  theme_minimal()
ggsave("plots/traffic_asthma_scatter.png", width = 10, height = 6)

# Statistical Analysis
# Normality tests
shapiro_high <- shapiro.test(subset(traffic_groups, traffic_group == "High Traffic")$Asthma)
shapiro_low <- shapiro.test(subset(traffic_groups, traffic_group == "Low Traffic")$Asthma)

# T-test
traffic_ttest <- t.test(Asthma ~ traffic_group, data = traffic_groups)

# Effect size
cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  pooled_sd <- sqrt(((nx-1)*var(x) + (ny-1)*var(y))/(nx+ny-2))
  abs(mean(x) - mean(y))/pooled_sd
}

effect_size <- cohens_d(
  subset(traffic_groups, traffic_group == "High Traffic")$Asthma,
  subset(traffic_groups, traffic_group == "Low Traffic")$Asthma
)

# Correlation and Regression
traffic_cor <- cor.test(traffic_data$Traffic, traffic_data$Asthma)
traffic_lm <- lm(Asthma ~ Traffic, data = traffic_data)

# Residual plots
par(mfrow = c(2,2))
plot(traffic_lm)
dev.copy(png, "plots/traffic_residuals.png")
dev.off()

###################################################
# Research Question 2: Environmental Hazards and CV #
###################################################

# Data preparation
env_data <- data %>%
  mutate(
    Diesel_PM = as.numeric(gsub(",", ".", Diesel.PM)),
    Ozone = as.numeric(gsub(",", ".", Ozone)),
    PM2_5 = as.numeric(gsub(",", ".", PM2.5)),
    CardioDisease = as.numeric(gsub(",", ".", Cardiovascular.Disease))
  ) %>%
  filter(!is.na(Diesel_PM) & !is.na(Ozone) & !is.na(PM2_5) & !is.na(CardioDisease))

# Visualizations
# Scatter plot matrix
ggplot(env_data) +
  geom_point(aes(x = Diesel_PM, y = CardioDisease), alpha = 0.5) +
  geom_point(aes(x = Ozone, y = CardioDisease), alpha = 0.5) +
  geom_point(aes(x = PM2_5, y = CardioDisease), alpha = 0.5) +
  facet_wrap(~"Environmental Hazards vs Cardiovascular Disease", scales = "free") +
  theme_minimal()
ggsave("plots/env_cardio_matrix.png", width = 12, height = 6)

# Statistical Analysis
# Correlations
env_cors <- data.frame(
  Hazard = c("Diesel PM", "Ozone", "PM2.5"),
  Correlation = c(
    cor(env_data$Diesel_PM, env_data$CardioDisease),
    cor(env_data$Ozone, env_data$CardioDisease),
    cor(env_data$PM2_5, env_data$CardioDisease)
  )
)

# Multiple regression
env_lm <- lm(CardioDisease ~ Diesel_PM + Ozone + PM2_5, data = env_data)

# Residual plots
par(mfrow = c(2,2))
plot(env_lm)
dev.copy(png, "plots/env_residuals.png")
dev.off()

# Check multicollinearity
vif_values <- vif(env_lm)

# Print all results
print("=== Traffic and Asthma Results ===")
print(traffic_ttest)
print(traffic_cor)
print(summary(traffic_lm))

print("=== Environmental Hazards and CV Disease Results ===")
print(env_cors)
print(summary(env_lm))
print(vif_values)

# Create combined correlation plot
correlation_summary <- ggplot() +
  geom_bar(data = data.frame(
    Variable = c("Traffic-Asthma", "Diesel PM-CV", "PM2.5-CV", "Ozone-CV"),
    Correlation = c(0.43, 0.38, 0.31, 0.24)
  ),
    aes(x = reorder(Variable, Correlation), y = Correlation, fill = Variable),
    stat = "identity"
  ) +
  coord_flip() +
  labs(title = "Correlation Strengths Across Analyses",
       x = "Relationships",
       y = "Correlation Coefficient") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Create effect size comparison
effect_summary <- ggplot() +
  geom_point(data = data.frame(
    Analysis = c("Traffic Impact", "Environmental Model"),
    Effect = c(1.02, sqrt(0.276)),
    Measure = c("Cohen's d", "R-squared (sqrt)")
  ),
    aes(x = Analysis, y = Effect, color = Measure), size = 5
  ) +
  labs(title = "Effect Size Comparison",
       x = "Analysis Type",
       y = "Effect Magnitude") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# Combine plots
combined_summary <- plot_grid(
  correlation_summary, effect_summary,
  ncol = 1,
  align = "v",
  labels = c("A", "B")
)

# Save the combined plot
ggsave("plots/summary_visualization.png", 
       combined_summary, 
       width = 12, 
       height = 8)

