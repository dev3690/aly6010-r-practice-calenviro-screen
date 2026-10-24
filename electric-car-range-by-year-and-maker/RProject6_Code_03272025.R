# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read and prepare the dataset
ev_data <- read.csv("Electric_Vehicle_Population_Data.csv")

# Initial data cleaning and preparation
ev_clean <- ev_data %>%
  # Remove rows with missing values and zero electric range
  filter(!is.na(Electric_Range), Electric_Range > 0) %>%
  # Select relevant columns
  select(Make, Model, Model_Year, Electric_Range, 
         County, Clean_Alternative_Fuel_Vehicle_CAFV_Eligibility) %>%
  # Rename columns
  rename(
    CAFV_Eligibility = Clean_Alternative_Fuel_Vehicle_CAFV_Eligibility,
    Year = Model_Year
  )

# Convert to factors after cleaning
ev_clean <- ev_clean %>%
  mutate(
    Make = factor(Make),
    Model = factor(Model),
    County = factor(County),
    CAFV_Eligibility = factor(CAFV_Eligibility)
  )

# Create dummy variables for manufacturers
make_dummies <- model.matrix(~ Make - 1, data = ev_clean)
colnames(make_dummies) <- gsub("Make", "", colnames(make_dummies))

# Fit main regression model
model_full <- lm(Electric_Range ~ Year + Make, data = ev_clean)

# Create visualization of trends
ggplot(ev_clean, aes(x = Year, y = Electric_Range, color = Make)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Electric Range Trends by Manufacturer",
    x = "Model Year",
    y = "Electric Range (miles)"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Analyze top 5 manufacturers
top_makes <- names(sort(table(ev_clean$Make), decreasing = TRUE)[1:5])
ev_subset <- ev_clean %>% filter(Make %in% top_makes)

# Separate regression analysis for top manufacturers
models_by_make <- ev_subset %>%
  group_by(Make) %>%
  summarise(
    intercept = coef(lm(Electric_Range ~ Year))[1],
    slope = coef(lm(Electric_Range ~ Year))[2],
    r_squared = summary(lm(Electric_Range ~ Year))$r.squared
  )

# Print results
summary(model_full)
print(models_by_make)

# Save the plot
ggsave("electric_range_trends.png", width = 10, height = 6)

# Create separate visualization for top 5 manufacturers
ggplot(ev_subset, aes(x = Year, y = Electric_Range, color = Make)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Electric Range Trends - Top 5 Manufacturers",
    x = "Model Year",
    y = "Electric Range (miles)",
    subtitle = "Separate Regression Lines by Manufacturer"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the top 5 manufacturers plot
ggsave("top_5_manufacturers_trends.png", width = 10, height = 6)

# Compare regression coefficients
models_comparison <- models_by_make %>%
  arrange(desc(r_squared)) %>%
  mutate(
    annual_range_increase = round(slope, 2),
    model_fit = round(r_squared * 100, 2)
  ) %>%
  select(Make, annual_range_increase, model_fit)

# Print comparison results
print("Comparison of Manufacturer Trends:")
print(models_comparison)

# Add additional statistical analysis
# After your existing model_full code:

# Add residual analysis
residual_stats <- data.frame(
  Mean_Residual = mean(residuals(model_full)),
  Std_Dev_Residual = sd(residuals(model_full)),
  Min_Residual = min(residuals(model_full)),
  Max_Residual = max(residuals(model_full))
)

# Add ANOVA analysis
anova_results <- anova(model_full)

# Add more sophisticated visualizations
# After your existing plots:

# Create residual plot
ggplot(data.frame(fitted = fitted(model_full), 
                 residuals = residuals(model_full)), 
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Residual Plot",
       x = "Fitted Values",
       y = "Residuals")

# Save this plot
ggsave("residual_plot.png", width = 8, height = 6)