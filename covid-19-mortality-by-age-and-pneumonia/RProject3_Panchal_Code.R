# Load required libraries
library(tidyverse)
library(ggplot2)
library(stats)

# Read the data
corona_data <- read.csv("Corona_Data.csv")

###########################################
# Data Preparation and Validation
###########################################

# Filter data for United States and All Sexes
# Note: Using All Sexes to avoid double-counting in age group analysis
us_data <- corona_data %>%
  filter(State == "United States" & Sex == "All Sexes" & Age.Group != "All Ages")

# Validate data additivity
total_by_age <- sum(us_data$COVID.19.Deaths)
total_reported <- corona_data %>%
  filter(State == "United States" & Sex == "All Sexes" & Age.Group == "All Ages") %>%
  pull(COVID.19.Deaths) %>%
  sum()  # Add sum() to get a single total

cat("\nData Validation:")
cat("\nSum of deaths by age groups:", total_by_age)
cat("\nReported total deaths:", total_reported)
cat("\nDifference:", total_by_age - total_reported)

###########################################
# TASK 1: One-Sample T-Test Analysis
###########################################

# Based on CDC's historical data, testing against national average of 50,000 deaths
# Reference: CDC COVID Data Tracker (https://covid.cdc.gov/covid-data-tracker)

mean_deaths <- mean(us_data$COVID.19.Deaths)
sd_deaths <- sd(us_data$COVID.19.Deaths)

cat("\n\n=== TASK 1: One-Sample T-Test Analysis ===")
cat("\nResearch Context: Testing if age-group mortality differs from expected average")
cat("\nDescriptive Statistics for COVID-19 Deaths:")
cat("\n- Mean:", round(mean_deaths, 2))
cat("\n- Standard Deviation:", round(sd_deaths, 2))

cat("\n\nHypotheses (based on CDC historical data):")
cat("\n- H₀: μ = 50,000 (Average deaths per age group equals 50,000)")
cat("\n- H₁: μ ≠ 50,000 (Average deaths per age group differs from 50,000)")

t_test_result <- t.test(us_data$COVID.19.Deaths, mu = 50000)
print(t_test_result)

cat("\nInterpretation:")
cat("\n- p-value:", format.pval(t_test_result$p.value, digits = 3))
if(t_test_result$p.value < 0.05) {
    cat("\n- Decision: Reject H₀")
    cat("\n- Conclusion: Evidence suggests age-specific mortality differs significantly from 50,000")
} else {
    cat("\n- Decision: Fail to reject H₀")
    cat("\n- Conclusion: Insufficient evidence of difference from 50,000 deaths per age group")
}

# Add effect size calculation after t-test
cohens_d <- (mean_deaths - 50000) / sd_deaths
cat("\n- Effect Size (Cohen's d):", round(cohens_d, 3))
cat("\n- Practical Significance:", 
    if(abs(cohens_d) > 0.8) "Large" 
    else if(abs(cohens_d) > 0.5) "Medium" 
    else "Small")

# Add confidence interval interpretation
cat("\n- 95% Confidence Interval:", 
    paste0("[", round(t_test_result$conf.int[1], 2), ", ", 
           round(t_test_result$conf.int[2], 2), "]"))

# Add percentage difference from hypothesized value
percent_diff <- ((mean_deaths - 50000) / 50000) * 100
cat("\n- Percentage Difference:", round(percent_diff, 2), "%")

# Visualization with context
death_plot <- ggplot(us_data, aes(x = Age.Group, y = COVID.19.Deaths)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "COVID-19 Deaths by Age Group in the United States",
         subtitle = "Analysis of Age-Specific Mortality Patterns\nRed line: Hypothesized mean (50,000 deaths)",
         x = "Age Group",
         y = "Number of Deaths",
         caption = "Data Source: CDC COVID-19 Death Data") +
    geom_hline(yintercept = 50000, color = "red", linetype = "dashed")

print(death_plot)

###########################################
# TASK 2: Correlation Analysis (P-value Testing)
###########################################

cat("\n\n=== TASK 2: Correlation Analysis ===")
cat("\nResearch Context: Examining relationship between COVID-19 and Pneumonia deaths")
cat("\nNote: Analysis considers age-stratified data to avoid ecological fallacy")

cat("\n\nHypotheses (based on medical literature):")
cat("\n- H₀: ρ = 0 (No correlation between COVID-19 and pneumonia deaths)")
cat("\n- H₁: ρ ≠ 0 (Correlation exists between COVID-19 and pneumonia deaths)")

correlation_test <- cor.test(us_data$COVID.19.Deaths, us_data$Pneumonia.Deaths)
print(correlation_test)

cat("\nInterpretation:")
cat("\n- Correlation coefficient (r):", round(correlation_test$estimate, 3))
cat("\n- p-value:", format.pval(correlation_test$p.value, digits = 3))

if(correlation_test$p.value < 0.05) {
    cat("\n- Decision: Reject H₀")
    cat("\n- Conclusion: Evidence supports correlation between COVID-19 and pneumonia deaths")
    cat("\n- Strength:", if(abs(correlation_test$estimate) > 0.7) "Strong" 
        else if(abs(correlation_test$estimate) > 0.3) "Moderate" 
        else "Weak")
    cat("\n- Direction:", if(correlation_test$estimate > 0) "Positive" else "Negative")
    cat("\n- Clinical Relevance: Aligns with known COVID-19 complications")
} else {
    cat("\n- Decision: Fail to reject H₀")
    cat("\n- Conclusion: Insufficient evidence of correlation")
}

# Enhanced correlation visualization
correlation_plot <- ggplot(us_data, aes(x = COVID.19.Deaths, y = Pneumonia.Deaths)) +
    geom_point(color = "darkblue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(title = "Correlation: COVID-19 Deaths vs Pneumonia Deaths",
         subtitle = paste("Correlation Analysis by Age Group\nr =", 
                        round(correlation_test$estimate, 3)),
         x = "COVID-19 Deaths",
         y = "Pneumonia Deaths",
         caption = "Data Source: CDC COVID-19 Death Data") +
    theme(plot.caption = element_text(hjust = 0))

print(correlation_plot)

# Save plots with context
ggsave("task1_age_mortality_analysis.png", death_plot, width = 10, height = 6)
ggsave("task2_comorbidity_analysis.png", correlation_plot, width = 10, height = 6)