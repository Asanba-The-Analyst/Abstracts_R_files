
library(tidyverse)
library(BlandAltmanLeh)


# Simulate the data from Table 1
method_a <- c(1, 5, 10, 20, 50, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 300,
              350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000)
method_b <- c(8, 16, 30, 24, 39, 54, 40, 68, 72, 62, 122, 80, 181, 259, 275, 380,
              320, 434, 479, 587, 626, 648, 738, 766, 793, 851, 871, 957, 1001, 960)

data <- data.frame(method_a, method_b)


# Calculate mean, difference, and percentage difference
data <- data %>%
  mutate(mean_ab = (method_a + method_b) / 2,
         diff_ab = method_a - method_b,
         perc_diff = (method_a - method_b) / mean_ab * 100)

# Compute stats
mean_diff <- mean(data$diff_ab)
sd_diff <- sd(data$diff_ab)

# Limits of agreement
loa_upper <- mean_diff + 1.96 * sd_diff
loa_lower <- mean_diff - 1.96 * sd_diff



# Bland-Altman plot: units
ggplot(data, aes(x = mean_ab, y = diff_ab)) +
  geom_point() +
  geom_hline(yintercept = mean_diff, color = "blue", linetype = "solid") +
  geom_hline(yintercept = loa_upper, color = "red", linetype = "dashed") +
  geom_hline(yintercept = loa_lower, color = "red", linetype = "dashed") +
  labs(title = "Bland-Altman Plot (Units)",
       x = "Mean of Method A and B",
       y = "Difference (A - B)") +
  theme_minimal()



# Mean and SD for percentage difference
mean_perc_diff <- mean(data$perc_diff)
sd_perc_diff <- sd(data$perc_diff)

loa_perc_upper <- mean_perc_diff + 1.96 * sd_perc_diff
loa_perc_lower <- mean_perc_diff - 1.96 * sd_perc_diff

# Bland-Altman plot: percentage
ggplot(data, aes(x = mean_ab, y = perc_diff)) +
  geom_point() +
  geom_hline(yintercept = mean_perc_diff, color = "blue") +
  geom_hline(yintercept = loa_perc_upper, color = "red", linetype = "dashed") +
  geom_hline(yintercept = loa_perc_lower, color = "red", linetype = "dashed") +
  labs(title = "Bland-Altman Plot (Percentage Differences)",
       x = "Mean of Method A and B",
       y = "Percentage Difference (%)") +
  theme_minimal()


# Shapiro-Wilk test
shapiro.test(data$diff_ab)



n <- nrow(data)
se_mean <- sd_diff / sqrt(n)
t_value <- qt(0.975, df = n - 1)

ci_mean <- c(mean_diff - t_value * se_mean, mean_diff + t_value * se_mean)
se_loa <- sqrt((1.96^2 * sd_diff^2 * 2) / n)
ci_loa_upper <- c(loa_upper - t_value * se_loa, loa_upper + t_value * se_loa)
ci_loa_lower <- c(loa_lower - t_value * se_loa, loa_lower + t_value * se_loa)

list(
  mean_diff_CI = ci_mean,
  loa_upper_CI = ci_loa_upper,
  loa_lower_CI = ci_loa_lower
)

