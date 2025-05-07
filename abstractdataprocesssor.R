#setwd("D:/DATA_ABSTRACTS/data")


library(tidyverse)
library(readr)
library(ggplot2)
library(haven)
library(foreign)

#loading the datasets

setwd("D:/CLEARPACKAGE2/prisma/excel/excel/Combine 04_04_25")

mnh00 <- read.csv("mnh00.csv",check.names = FALSE)

mnh01 <- read.csv("mnh01.csv",check.names = FALSE)
mnh02 <- read.csv("mnh02.csv",check.names = FALSE)
mnh03 <- read.csv("mnh03.csv",check.names = FALSE)
mnh08 <- read.csv("mnh08.csv",check.names = FALSE)
mnh09 <- read.csv("mnh09.csv",check.names = FALSE)

master <- read_dta("welcome_data.dta")


check_variable_existence <- function(datasets, variable) {
  for (dataset_name in names(datasets)) {
    if (variable %in% names(datasets[[dataset_name]])) {
      cat(dataset_name, "YES\n")
    } else {
      cat(dataset_name, "NO\n")
    }
  }
}


#datasets <- list(mnh03 = mnh03,mnh04 = mnh04,mnh05 = mnh05,mnh06 = mnh06,mnh08 = mnh08,mnh09 = mnh09, mnh11 = mnh11, mnh13 = mnh13, mnh14 = mnh14, mnh15 = mnh15, mnh19 = mnh19)
#check_variable_existence(datasets, "TYPE_VISIT")  


convert_names_to_lower <- function(dataset) {
  names(dataset) <- tolower(names(dataset))
  return(dataset)
}

# Example usage
mnh00 <- convert_names_to_lower(mnh00)
mnh01 <- convert_names_to_lower(mnh01)
mnh02<- convert_names_to_lower(mnh02)
mnh03 <- convert_names_to_lower(mnh03)
mnh08 <- convert_names_to_lower(mnh08)
mnh09 <- convert_names_to_lower(mnh09)



mnh00_var <- c("momid","pregid","estimated_age", "brthdat","school_scorres")


mnh00 <- mnh00[,mnh00_var]
mnh00 <- mnh00 %>% rename(momage=estimated_age)

mnh01_var <- c("momid","pregid","type_visit","us_ohostdat","estimated_edd_scdat","us_ga_days_age_fts1","us_ga_wks_age_fts1")
mnh01 <- mnh01[,mnh01_var]


mnh02 <-mnh02[,c("momid","pregid","scrn_obsstdat")] 

mnh03 <- mnh03 %>% rename(ethnic=cethnic,Marita_status=marital_scorres)

mnh03_variables <- c("momid", "pregid","ethnic","Marita_status")

mnh03 <- mnh03[,mnh03_variables]

mnh09_var <- c("momid","pregid","birth_dsterm_inf1","deliv_dsstdat_inf1","sex_inf1")

mnh09 <- mnh09[,mnh09_var]



mnh08_var <- c("momid","pregid","type_visit","malbl_lbperf_1","malbl_lborres","cbc_hb_lborres","rbc_g6pd_lborres","ctng_lbperf_1",
               "ctng_ct_lborres" ,"ctng_lbperf_2","ctng_ng_lborres")

zc <- c("zcd_lbtstdat",
  "zcd_lbperf_1",
  "zcd_zikigm_lborres",
  "zcd_lbperf_2",
  "zcd_zikigg_lborres",
  "zcd_lbperf_3",
  "zcd_denigm_lborres",
  "zcd_lbperf_4",
  "zcd_denigg_lborres",
  "zcd_lbperf_5",
  "zcd_chkigm_lborres",
  "zcd_lbperf_6",
  "zcd_chkigg_lborres")

#daf <- mnh08[,c("momid","pregid","type_visit","malbl_lbperf_1","malbl_lborres","ctng_ng_lborres","ctng_lbperf_2","ctng_ng_lborres","ctng_lbperf_2","ctng_ct_lborres")]

mnh08 <- mnh08[,c(zc,mnh08_var)]

mnh00_unid <- mnh00 %>%
  distinct(pregid, .keep_all = TRUE)

mnh01_unid <- mnh01 %>%
  distinct(pregid, .keep_all = TRUE)

mnh02_unid <- mnh02 %>%
  distinct(pregid, .keep_all = TRUE)

mnh03_unid <- mnh03 %>%
  distinct(pregid, .keep_all = TRUE)

mnh09_unid <- mnh09 %>%
  distinct(pregid, .keep_all = TRUE)

#only add 8 if you need lab test
mnh08_un <- mnh08 %>%
  distinct(pregid, .keep_all = TRUE)

dat <- master %>%left_join(mnh00_unid,by=c("momid","pregid"))

data <- dat %>%left_join(mnh01_unid,by=c("momid","pregid"))


mnh0_2 <- data %>%left_join(mnh02_unid,by=c("momid","pregid"))


data2  <- mnh0_2 %>%left_join(mnh09,by=c("momid","pregid"))


data3 <- data2 %>%left_join(mnh08_un,by=c("momid","pregid"))


#data2$deliv_dsstdat_inf1 <- dmy(data2$deliv_dsstdat_inf1)

# date_ex <- dmy("31-12-2024")
# data2<- data2%>% filter(data2$deliv_dsstdat_inf1<=date_ex)

# 
# write.csv(data2,"prisma_demographs.csv")
# 
#write.csv(data3,"gono_chlam.csv")

#write.csv(data3,"zc.csv")
write.csv(data3,"all.csv")



# 
# wider_mnh08 <- mnh08 %>% 
#   pivot_wider(names_from = "type_visit",
#               values_from = "malbl_lborres",names_prefix="malaria")
# 
# malr8 <- c("malaria1","malaria2","malaria3","malaria4","malaria5","malaria7","malaria8",
#            "malaria9","malaria10","malaria11","malaria12")
# 
# wider_mnh08 <- check_n_generat(wider_mnh08,malr8,"malaria")
# 
# wider_mnh08 <- wider_mnh08[,c("momid","pregid","malaria")]