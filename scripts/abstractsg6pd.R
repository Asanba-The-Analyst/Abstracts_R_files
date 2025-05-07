setwd("D:/DATA_ABSTRACTS/data")

# Load required libraries
# install.packages("pROC")
# install.packages("BlandAltmanLeh")
# install.packages("caret")

library(tidyverse)
library(pROC)
library(BlandAltmanLeh)
library(caret)
library(reshape2)  # for matrix reshaping


# Load dataset
data1 <- read.csv("g6pd_processed.csv")

data1 <- data1 %>% rename(ghaid=subjid)

all <- read.csv("all.csv")


#ids <- data1$ghaid

#dat2 <- demog %>% filter(ghaid %in% ids)
 #52 have missing cbclbore values and 37 had NA's

data <- data1 %>% inner_join(all,by="ghaid")

#"cbc_hb_lborres","rbc_g6pd_lborres"
#The gold standard :cbc_hb_lborres
# removing all NA in the totol_hb and rbc_gluc6
data <- data %>% filter(!(is.na(data$total_hb)|is.na(data$rbc_gluc6_lborres)))
data <- data %>% filter(!is.na(data$cbc_hb_lborres))
data$cbc_hb_lborres[data$cbc_hb_lborres<0] <- NA

summary(data$momage)

table(data$school_scorres)

# Format date
data$analysis_date <- as.Date(data$analysis_date, format = "%d/%m/%Y")


summary(data$rbc_gluc6_lborres)
summary(data$cbc_hb_lborres)
summary(data$total_hb)
#missing 

age_mis <- data %>% filter(is.na(data$momage))
MIS <- data %>% filter(is.na(data$rbc_gluc6_lborres))
MIS1 <- data %>% filter(is.na(data$total_hb))


diff_ab <- data$cbc_hb_lborres-data$total_hb

# 
# diff_log <- log(data$total_hb) - log(data$cbc_hb_lborres)
# shapiro.test(diff_log)
method_a <- data$cbc_hb_lborres
method_b <- data$total_hb
perc_diff <- (method_a - method_b) / ((method_a + method_b)/2) * 100
shapiro.test(perc_diff)


data <- data %>% mutate(age_group = case_when(
  momage < 30         ~ "Young",       
  momage >= 30 & momage < 60 ~ "Middle-aged", 
    TRUE                 ~ NA_character_ 
  ))

table(data$age_group)
  6 
# Classify G6PD status using WHO thresholds
data <- data %>%
  mutate(g6pd_status = case_when(
    rbc_gluc6_lborres < 4 ~ "Deficient",
    rbc_gluc6_lborres >= 4~ "Normal"
  ))


data$g6pd_status <- factor(data$g6pd_status, levels = c("Deficient", "Normal"))



# Tabulate G6PD status by age group
g6pd_by_age <- table(data$age_group, data$g6pd_status)

# Print the table
cat("G6PD Status by Age Group:\n")
print(g6pd_by_age)

# Add proportions within age groups (row-wise)
cat("\nProportion of G6PD Status within Age Groups:\n")
print(round(prop.table(g6pd_by_age, margin = 1), 3))

# Optionally add margins for totals
cat("\nG6PD Status by Age Group with Totals:\n")
print(addmargins(g6pd_by_age))

# Define anaemia threshold
anaemia_cutoff <- 11

# Binary anaemia status from lab and POCT Hb
data <- data %>%
  mutate(anaemia_cbc_hb = ifelse(cbc_hb_lborres < anaemia_cutoff, 1, 0),
         anaemia_total_hb = ifelse(total_hb< anaemia_cutoff, 1, 0))


install.packages("blandr")
library(blandr)


blandr.statistics(method_a, method_b)


# Overall Bland-Altman Plot
bland.altman.plot(data$cbc_hb_lborres,data$total_hb,
                  main = "Bland-Altman Plot: POCT Hb vs Lab Hb",
                  xlab = "Mean Hb", ylab = "Difference (POCT - Lab)")


# Calculate mean and difference
data$mean_hb <- rowMeans(data[, c("cbc_hb_lborres", "total_hb")], na.rm = TRUE)
data$difference <- data$cbc_hb_lborres - data$total_hb

# Create the Bland-Altman plot
ggplot(data, aes(x = mean_hb, y = difference)) +
  geom_point(color = "black", size = 2.5, alpha = 0.6) +  # Points
  geom_hline(yintercept = mean(data$difference, na.rm = TRUE), color = "red", linetype = "dashed", size = 1) +  # Mean line
  geom_hline(yintercept = mean(data$difference, na.rm = TRUE) + 1.96 * sd(data$difference, na.rm = TRUE), color = "darkgreen", linetype = "dotted", size = 1) +  # Upper limit
  geom_hline(yintercept = mean(data$difference, na.rm = TRUE) - 1.96 * sd(data$difference, na.rm = TRUE), color = "darkgreen", linetype = "dotted", size = 1) +  # Lower limit
  labs(title = "Bland-Altman Plot: Methond A(gold standard) Hb vs Method B(Alternative)Hb",
       x = "Mean Hb",
       y = "Difference (Method A - Method B)") +
  theme_minimal(base_size = 15) +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center title
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))






# Calculate statistics
mean_bias <- mean(data$difference, na.rm = TRUE)
loa_upper <- mean_bias + 1.96 * sd(data$difference, na.rm = TRUE)
loa_lower <- mean_bias - 1.96 * sd(data$difference, na.rm = TRUE)

# Enhanced Bland-Altman plot
ggplot(data, aes(x = mean_hb, y = difference)) +
  geom_point(color = "black", size = 2.5, alpha = 0.6) +
  
  # Mean Bias Line
  geom_hline(yintercept = mean_bias, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = min(mean_hb, na.rm = TRUE) + 0.5, y = mean_bias + 0.5),
            label = paste0("Mean Bias = ", round(mean_bias, 2)),
            color = "blue", size = 5, hjust = 0) +
  
  # Upper LOA Line
  geom_hline(yintercept = loa_upper, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_text(aes(x = min(mean_hb, na.rm = TRUE) + 0.5, y = loa_upper + 0.5),
            label = paste0("Upper LOA = ", round(loa_upper, 2)),
            color = "red", size = 5, hjust = 0) +
  
  # Lower LOA Line
  geom_hline(yintercept = loa_lower, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_text(aes(x = min(mean_hb, na.rm = TRUE) + 0.5, y = loa_lower - 0.5),
            label = paste0("Lower LOA = ", round(loa_lower, 2)),
            color = "red", size = 5, hjust = 0) +theme_minimal()
  
  





# Overall diagnostic performance
cat("\nOverall Diagnostic Performance:\n")
overall_cm <- confusionMatrix(factor(anaemia_cbc_hb),
                              factor(anaemia_total_hb),
                              positive = "1")
print(overall_cm)

# ROC Curve (overall)
roc_overall <- roc(data$anaemia_lab, data$rbc_gluc6_lborres)
plot(roc_overall, main = "ROC Curve: POCT Hb vs Lab Hb (Overall)")




# Plot ROC curve with enhancements
plot(roc_overall,
     main = "ROC Curve: POCT Hb vs Lab Hb (Overall)",
     col = "blue",               # Line color
     lwd = 2,                    # Line width
     legacy.axes = TRUE,         # Use legacy axes
     print.auc = TRUE,           # Print AUC on the plot
     print.auc.x = 0.5,          # Position of AUC
     print.auc.y = 0.2,          # Position of AUC
     xlim = c(0, 1),             # X-axis limits
     ylim = c(0, 1),             # Y-axis limits
     grid = TRUE,                # Add grid
     cex.main = 1.5,             # Title size
     cex.axis = 1.2)             # Axis label size

# Add diagonal line for reference
abline(a = 0, b = 1, col = "red", lty = 2)
cat("Overall AUC:", auc(roc_overall), "\n")

# === Subgroup Analysis by G6PD Status ===
for (group in levels(data$g6pd_status)) {
  cat(paste0("\nSubgroup: ", group, "\n"))
  
  subgroup_data <- data %>% filter(g6pd_status == group)
  
  # Confusion matrix
  cm <- confusionMatrix(factor(subgroup_data$anaemia_poct),
                        factor(subgroup_data$anaemia_lab),
                        positive = "1")
  print(cm)
  
  # ROC curve
  roc_obj <- roc(subgroup_data$anaemia_lab, subgroup_data$rbc_gluc6_lborres)
  plot(roc_obj, main = paste("ROC Curve -", group))
  cat("AUC:", auc(roc_obj), "\n")
}


plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  cm_table <- as.table(cm$table)
  cm_df <- as.data.frame(cm_table)
  
  ggplot(data = cm_df, aes(Prediction, Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold") +
    scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +
    labs(title = title, x = "Predicted", y = "Actual") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

#This is for the overall

print(overall_cm)
plot_confusion_matrix(overall_cm, title = "Confusion Matrix - Overall")

#This is for the sub group
print(cm)
plot_confusion_matrix(cm, title = paste("Confusion Matrix -", group))




# Optional: Save enriched dataset
write.csv(data, "g6pd_analysis_with_status.csv", row.names = FALSE)

















bland_altman_analysis <- function(method_a, method_b, title_prefix = "Bland-Altman") {
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  
  df <- data.frame(method_a, method_b) %>%
    mutate(mean_ab = (method_a + method_b) / 2,
           diff_ab = method_a - method_b,
           perc_diff = (method_a - method_b) / mean_ab * 100)
  
  # Stats for unit differences
  mean_diff <- mean(df$diff_ab)
  sd_diff <- sd(df$diff_ab)
  loa_upper <- mean_diff + 1.96 * sd_diff
  loa_lower <- mean_diff - 1.96 * sd_diff
  
  # Stats for percentage differences
  mean_perc <- mean(df$perc_diff)
  sd_perc <- sd(df$perc_diff)
  perc_loa_upper <- mean_perc + 1.96 * sd_perc
  perc_loa_lower <- mean_perc - 1.96 * sd_perc
  
  # Plot 1: Units
  p1 <- ggplot(df, aes(x = mean_ab, y = diff_ab)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, color = "blue") +
    geom_hline(yintercept = loa_upper, color = "red", linetype = "dashed") +
    geom_hline(yintercept = loa_lower, color = "red", linetype = "dashed") +
    labs(title = paste0(title_prefix, " (g/dL)"),
         x = "Mean of Method A and B", y = "Difference (A - B)") +
    theme_minimal()
  
  # Plot 2: Percent
  p2 <- ggplot(df, aes(x = mean_ab, y = perc_diff)) +
    geom_point() +
    geom_hline(yintercept = mean_perc, color = "blue") +
    geom_hline(yintercept = perc_loa_upper, color = "red", linetype = "dashed") +
    geom_hline(yintercept = perc_loa_lower, color = "red", linetype = "dashed") +
    labs(title = paste0(title_prefix, " (Percentage)"),
         x = "Mean of Method A and B", y = "Percentage Difference (%)") +
    theme_minimal()
  
  return(list(plots = list(p1,p2),
              stats = list(mean_diff = mean_diff, sd_diff = sd_diff,
                           loa_upper = loa_upper, loa_lower = loa_lower,
                           mean_perc = mean_perc, sd_perc = sd_perc,
                           perc_loa_upper = perc_loa_upper, perc_loa_lower = perc_loa_lower)))
}


method_a <- data$cbc_hb_lborres
method_b <- data$total_hb

result <- bland_altman_analysis(method_a, method_b, title_prefix = "Method A vs B")
result
gridExtra::grid.arrange(grobs = result$plots, ncol = 2)

