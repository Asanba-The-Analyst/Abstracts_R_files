#statistical description of seroprevalence of ZCD (Zika, Chikungunya, Dengue) 
#and explore pregnancy outcomes 

#R Script: Seroprevalence of ZCD and Birth Outcomes



# Load required libraries
library(tidyverse)

setwd("D:/DATA_ABSTRACTS/data")

data <- read.csv("zc.csv")

# 1. Recode test result variables for clarity
recode_test_result <- function(x) {
  factor(x, levels = c(0, 1, 2),
         labels = c("Negative", "Positive", "Inconclusive"))
}




data <- data %>% select(-X)

data <- data %>%
  rename(
    delivery_date = deliv_dsstdat_inf1,
    birthoutcome = birth_dsterm_inf1,
    gonorhea=ctng_ng_lborres,
    chlamydia=ctng_ct_lborres,
    gonor_test =ctng_lbperf_2,
    chlam_test= ctng_lbperf_1
    
  )



data <- data %>%
  mutate(birthoutcome = factor(case_when(
    birthoutcome == 1 ~ "Live birth",
    birthoutcome == 2 ~ "Fetal death",
    birthoutcome == 77~ "Miscarriages",
    TRUE ~ NA_character_
  ), levels = c("Live birth", "Fetal death","misscarrigies")))


data <- data %>%
  mutate(
    zikv_igm_result = recode_test_result(zcd_zikigm_lborres),
    zikv_igg_result = recode_test_result(zcd_zikigg_lborres),
    denv_igm_result = recode_test_result(zcd_denigm_lborres),
    denv_igg_result = recode_test_result(zcd_denigg_lborres),
    chkv_igm_result = recode_test_result(zcd_chkigm_lborres),
    chkv_igg_result = recode_test_result(zcd_chkigg_lborres)
  )

# 2. Calculate seroprevalence of each infection type
calculate_seroprevalence <- function(var) {
  round(prop.table(table(var)) * 100, 1)
}

seroprevalence <- list(
  ZIKV_IgM = calculate_seroprevalence(data$zikv_igm_result),
  ZIKV_IgG = calculate_seroprevalence(data$zikv_igg_result),
  DENV_IgM = calculate_seroprevalence(data$denv_igm_result),
  DENV_IgG = calculate_seroprevalence(data$denv_igg_result),
  CHKV_IgM = calculate_seroprevalence(data$chkv_igm_result),
  CHKV_IgG = calculate_seroprevalence(data$chkv_igg_result)
)

print("Seroprevalence (%):")
print(seroprevalence)

# 3. Create combined seroprevalence variable (positive to any)
data <- data %>%
  mutate(
    any_zcd_positive = ifelse(
      zcd_chkigm_lborres == 1 | zcd_chkigm_lborres == 1 |
        zcd_denigm_lborres == 1 | zcd_denigm_lborres == 1 |
        zcd_chkigm_lborres == 1 | zcd_chkigm_lborres == 1,
      1, 0
    )
  )


# 4. Tabulate seropositivity among pregnant women
cat("Overall ZCD Seropositivity among pregnant women:\n")
print(table(data$any_zcd_positive))
print(round(prop.table(table(data$any_zcd_positive)) * 100, 1))




# 5. Analyze association with birth outcomes
# Assume a binary variable "adverse_birth_outcome" exists: 1 = adverse, 0 = normal
if ("adverse_birth_outcome" %in% names(data)) {
  cat("\nAssociation between ZCD positivity and birth outcome:\n")
  outcome_table <- table(data$any_zcd_positive, data$adverse_birth_outcome)
  print(outcome_table)
  print(prop.table(outcome_table, 1))  # row-wise percentage
  
  # Perform Chi-squared test
  chisq_test <- chisq.test(outcome_table)
  print(chisq_test)
}

# Optional: Visualization
# ggplot(data, aes(x = factor(any_zcd_positive), fill = factor(adverse_birth_outcome))) +
#   geom_bar(position = "fill") +
#   labs(x = "ZCD Seropositivity", y = "Proportion", fill = "Birth Outcome") +
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal()
