# Week 1 - R Script
# Dev Panchal


# 1. Install and Load Required Packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("gmodels")) install.packages("gmodels")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(gmodels)

# 2. Import the Dataset
tempdata <- read.csv("tempdata.csv", stringsAsFactors = FALSE, skip = 1)

# 3. Data Cleaning and Preparation
# Rename Columns
colnames(tempdata) <- c("DataAsOf", "StartDate", "EndDate", "Group", "Year", "Month",
                        "State", "ConditionGroup", "Condition", "ICD10Codes", "AgeGroup",
                        "COVID19Deaths", "NumberMentions", "Flag")
# Convert dates for Proper date Format
tempdata$DataAsOf <- as.Date(tempdata$DataAsOf, format = "%m/%d/%Y")
tempdata$StartDate <- as.Date(tempdata$StartDate, format = "%m/%d/%Y")
tempdata$EndDate <- as.Date(tempdata$EndDate, format = "%m/%d/%Y")

# Convert Year and Month to factors (categorical)
tempdata$Year <- as.factor(tempdata$Year)
tempdata$Month <- factor(tempdata$Month, levels = 1:12,
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Convert categorical variables to factors
tempdata$State <- as.factor(tempdata$State)
tempdata$ConditionGroup <- as.factor(tempdata$ConditionGroup)
tempdata$Condition <- as.factor(tempdata$Condition)
tempdata$AgeGroup <- as.factor(tempdata$AgeGroup)

# Clean and convert numeric variables
tempdata$COVID19Deaths <- ifelse(tempdata$COVID19Deaths == "", 0,
                                 as.numeric(tempdata$COVID19Deaths))
tempdata$NumberMentions <- ifelse(tempdata$NumberMentions == "", 0,
                                  as.numeric(tempdata$NumberMentions))

# Clean Flag Column
tempdata$Flag <- ifelse(tempdata$Flag == "", NA,
                        ifelse(tempdata$Flag == "One or more data cells have counts between 1-9 and have been suppressed in accordance with NCHS confidentiality standards.",
                               "Suppressed", tempdata$Flag))

# Create a new variable for Analysis
tempdata$Season <- ifelse(tempdata$Month %in% c("Dec", "Jan", "Feb"), "Winter",
                          ifelse(tempdata$Month %in% c("Mar", "Apr", "May"), "Spring",
                                 ifelse(tempdata$Month %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
tempdata$Season <- as.factor(tempdata$Season)

# Calculate Death Rate (if needed)
tempdata$DeathRate <- tempdata$COVID19Deaths/tempdata$NumberMentions
tempdata$DeathRate[is.nan(tempdata$DeathRate)] <- 0



# 4. Data Exploration
# Structure of cleaned data
print("Structure of cleaned dataset:")
str(tempdata)

# Summary of categorical variables
print("\nSummary by State:")
print(table(tempdata$State))

print("\nSummary by Year:")
print(table(tempdata$Year))

print("\nSummary by Age Group:")
print(table(tempdata$AgeGroup))

print("\nSummary by Season:")
print(table(tempdata$Season))

# Summary of numerical variables
print("\nSummary of COVID19Deaths:")
print(summary(tempdata$COVID19Deaths))

print("\nSummary of NumberMentions:")
print(summary(tempdata$NumberMentions))

print("\nSummary of DeathRate:")
print(summary(tempdata$DeathRate))

# Additional useful summaries
print("\nNumber of rows in dataset:")
print(nrow(tempdata))

print("\nNumber of unique conditions:")
print(length(unique(tempdata$Condition)))

# 5. Create Frequency Tables and Visualizations

# 5.1 Frequency Tables
# State-wise COVID deaths
print("\nState-wise COVID Deaths Summary:")
state_deaths <- aggregate(COVID19Deaths ~ State, data = tempdata,
                          FUN = function(x) c(sum = sum(x, na.rm = TRUE)))
print(state_deaths)

# Year-wise distribution by condition
print("\nYear-wise Distribution by Condition:")
year_condition <- table(tempdata$Year, tempdata$Condition)
print(ftable(year_condition))

# Cross-tabulation of Age Group and Year
print("\nCross-tabulation of Age Group and Year:")
CrossTable(tempdata$AgeGroup, tempdata$Year)

# 5.2 Visualizations

# Create directory for plots
dir.create("plots", showWarnings = FALSE)

# Seasonal Impact Plot
seasonal_plot <- ggplot(tempdata, aes(x = Season, y = COVID19Deaths)) +
  geom_bar(stat = "sum", fill = "grey40") +
  labs(title = "Seasonal Impact on COVID-19 Deaths",
       x = "Season",
       y = "Total Deaths") +
  theme_minimal() +
  scale_y_continuous(labels = scales::scientific_format(),
                     breaks = seq(0, 3e6, by = 1e6),
                     limits = c(0, 3.5e6))

# Death Rate Analysis by State
boxplot_state <- ggplot(tempdata, aes(x = State, y = DeathRate)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1) +
  labs(title = "Death Rate Analysis by State",
       x = "State",
       y = "Death Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))

# COVID-19 Deaths by Age Group and Year Heat Map
heatmap_age <- ggplot(tempdata, aes(x = Year, y = AgeGroup)) +
  geom_tile(aes(fill = COVID19Deaths)) +
  scale_fill_gradient2(low = "white", high = "red",
                       limits = c(0, 100000),
                       name = "Deaths") +
  labs(title = "COVID-19 Deaths by Age Group and Year",
       x = "Year",
       y = "Age Group") +
  theme_minimal()

# COVID-19 Deaths Trend Over Time
trend_plot <- tempdata %>%
  group_by(Year, Month) %>%
  summarize(TotalDeaths = sum(COVID19Deaths, na.rm = TRUE)) %>%
  ggplot(aes(x = Month, y = TotalDeaths, color = as.factor(Year), group = Year)) +
  geom_line() +
  geom_point() +
  labs(title = "COVID-19 Deaths Trend Over Time",
       x = "Month",
       y = "Total Deaths",
       color = "Year") +
  theme_minimal() +
  scale_y_continuous(labels = scales::scientific_format(),
                     limits = c(0, 2.5e6)) +
  scale_color_manual(values = c("2020" = "coral",
                                "2021" = "chartreuse3",
                                "2022" = "turquoise3",
                                "2023" = "purple",
                                "NA" = "grey50")) +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "NA"))

# Total COVID-19 Deaths by State Bar Plot
state_deaths <- tempdata %>%
  group_by(State) %>%
  summarize(TotalDeaths = sum(COVID19Deaths, na.rm = TRUE)) %>%
  arrange(desc(TotalDeaths))

state_bar <- ggplot(state_deaths, aes(x = reorder(State, -TotalDeaths), y = TotalDeaths)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Total COVID-19 Deaths by State",
       x = "State",
       y = "Total Deaths") +
  theme_minimal() +
  scale_y_continuous(labels = scales::scientific_format())

# Print and save all plots
print(seasonal_plot)
print(boxplot_state)
print(heatmap_age)
print(trend_plot)
print(state_bar)

# Save plots
ggsave("plots/seasonal_impact.png", seasonal_plot, width = 10, height = 6)
ggsave("plots/death_rate_by_state.png", boxplot_state, width = 12, height = 6)
ggsave("plots/deaths_by_age_year.png", heatmap_age, width = 10, height = 6)
ggsave("plots/deaths_trend.png", trend_plot, width = 12, height = 6)
ggsave("plots/total_deaths_by_state.png", state_bar, width = 10, height = 8)
write.csv(tempdata, "cleaned_tempdata.csv", row.names = FALSE)
