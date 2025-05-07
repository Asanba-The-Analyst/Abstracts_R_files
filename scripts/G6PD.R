setwd("D:/DATA_ABSTRACTS/data")
#install.packages("blandr")


# Load required libraries
# install.packages("pROC")
# install.packages("BlandAltmanLeh")
# install.packages("caret")
library(blandr)
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

#assigning the measures
method_a <- data$cbc_hb_lborres
method_b <- data$total_hb
#statistics
blandr.statistics(method_a, method_b)

# Calculate mean and difference
data$mean_hb <- rowMeans(data[, c("cbc_hb_lborres", "total_hb")], na.rm = TRUE)
data$difference <- data$cbc_hb_lborres - data$total_hb


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
overall_cm <- confusionMatrix(factor(data$anaemia_cbc_hb),
                              factor(data$anaemia_total_hb),
                              positive = "1")
print(overall_cm)

# ROC Curve (overall)
roc_overall <- roc(data$anaemia_cbc_hb,data$anaemia_total_hb)
plot(roc_overall) #main = "ROC Curve: anaemia_cbc_hb  vs anaemia_total_hb (Overall)")
