# Load required library
#install.packages(“MASS”)
library(MASS)

# Create separate vectors for male and female cats
male_cats <- subset(cats, subset=(Sex=="M"))$Bwt
female_cats <- subset(cats, subset=(Sex=="F"))$Bwt

# Perform two-sample t-test with unequal variance
t_test_result <- t.test(male_cats, female_cats, 
                        var.equal=FALSE,  # Assuming unequal variances
                        alternative="two.sided")

# Print the test result
print(t_test_result)

# Calculate basic statistics for visualization
male_mean <- mean(male_cats)
female_mean <- mean(female_cats)
cat("\nMean bodyweight for male cats:", male_mean)
cat("\nMean bodyweight for female cats:", female_mean)

# Create a boxplot for visualization
boxplot(Bwt ~ Sex, data=cats, 
        main="Bodyweight Distribution by Cat Sex",
        ylab="Body Weight (kg)",
        xlab="Sex")

# Add a density plot for better distribution visualization
par(mfrow=c(1,2))  # Set up 2 plots side by side

# Recreate the boxplot
boxplot(Bwt ~ Sex, data=cats, 
        main="Bodyweight Distribution by Cat Sex",
        ylab="Body Weight (kg)",
        xlab="Sex")

# Create density plot
plot(density(male_cats), 
     main="Density Plot of Cat Bodyweights",
     xlab="Body Weight (kg)",
     ylab="Density",
     col="blue")
lines(density(female_cats), col="red")
legend("topright", 
       legend=c("Male", "Female"),
       col=c("blue", "red"),
       lty=1)
#====================================================================
# Part 2: Meditation Effect on Sleep Quality Analysis
# Define the data
before_meditation <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after_meditation <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

# Perform paired t-test
sleep_test <- t.test(after_meditation, before_meditation, 
                     paired = TRUE,
                     alternative = "greater")  # One-sided test as we're testing for improvement
print(sleep_test)

# Calculate basic statistics
mean_before <- mean(before_meditation)
mean_after <- mean(after_meditation)
sd_before <- sd(before_meditation)
sd_after <- sd(after_meditation)

# Create visualizations for sleep quality data
par(mfrow=c(1,2))

# Boxplot
boxplot(list(Before=before_meditation, After=after_meditation),
        main="Sleep Quality Before vs After Meditation",
        ylab="Sleep Quality Score (0-10)",
        col=c("lightblue", "lightgreen"))

# Line plot for individual changes
plot(1:10, before_meditation, type="b", col="blue",
     xlim=c(1,10), ylim=c(min(c(before_meditation, after_meditation)),
                          max(c(before_meditation, after_meditation))),
     xlab="Student", ylab="Sleep Quality Score",
     main="Individual Changes in Sleep Quality",
     pch=16)
lines(1:10, after_meditation, type="b", col="green", pch=16)
legend("topright", legend=c("Before", "After"),
       col=c("blue", "green"), lty=1, pch=16)
