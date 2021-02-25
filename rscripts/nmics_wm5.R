# ---------------------------------------------------------------
# tITLE: Computation of Fertility Rates from NMICS Datasets 2014
# ---------------------------------------------------------------
# Authors: Dhruba Raj Ghimire
# Date: 2021-02-22

# Preparation R
rm(list=ls()) # Clear Working Memory
setwd("C:/Users/dghimire/Dropbox (Personal)/nexus/blockchain")
(.packages())          # First, let’s check the currently loaded packages after restarting RStudio
# installed.packages()  # Returns a matrix with a row for each package that has been installed
# available.packages()  # Returns a matrix of details corresponding to packages currently available at one or more repositories

# ==============================================================================
# install.packages("tidyverse")         # A collection of R packages designed for data science
# install.packages("data.table")	# Extension of "data.frame"
# install.packages("dtplyr")	        # provides a data.table backend for dplyr.
# install.packages("labelled")	        # Manipulating Labelled Data Importing from haven or foreign packages
# install.packages("sjlabelled")	# Labelled Data Utility Functions from SAS,SPSS, and Stata
# install.packages("sjmisc")	        # Re-coding Variables
# install.packages("reshape2")	        # Flexibly Reshape Data
# install.packages("DHS.rates")
# ==============================================================================

library(tidyverse) # readr, tidyr, dplyr, purrr, tibble, stringr, ggplot2, and forcats 
library(haven)
library(readxl)
# library(data.table)
set.seed(1L)
library(dtplyr)
library(reshape2)
# library(labelled)
# library(sjlabelled)
library(sjmisc)
library(DHS.rates)
# ==============================================================================

# Prepare Datasets: (i) Province, (ii) Residence, (iii) Caste/Ethnicity, and (iv) Education 
# (i) For NMICS 2014 Datasets

# Prepare Datasets: (i) Province, (ii) Residence, (iii) Caste/Ethnicity, and (iv) Education 
# (i) For NMICS 2014 Datasets
nmics_hh5 <- read_sav("./rawdata/nmics/np5_hh.SAV") # Read and Check Data (NMICS 2019), ID(HH1 HH2)
nmics_bh5 <- read_sav("./rawdata/nmics/np5_bh.SAV") # Read and Check Data (NMICS 2014)
nmics_wm5 <- read_sav("./rawdata/nmics/np5_wm.SAV") # Read and Check Data (NMICS 2014), ID(HH1 HH2 LN)
class(nmics_bh5)
class(nmics_wm5)

# keep the necessary variables only: Residence (HH6), Province (HH7 in 2019), Development Region (HH7 in 2014), Educational Attainment (WB5) 
# keep the necessary variables only: Residence (HH6), Province (HH7), Educational Attainment (WB5) 
nmics_hh5 <- nmics_hh5 %>% # Households
        select("HH1","HH2","HC1C","HH9") %>% 
        filter(HH9==1)
nmics_wm5 <- nmics_wm5 %>% # Women in reproductive age (15-49 years of age)
        select("HH1","HH2","HH6","LN","WDOI","WDOB","wmweight","HH7","WB5","welevel","WM7") %>% 
        filter(WM7==1)
nmics_bh5 <- nmics_bh5 %>% # Birth history of ever-married Women in reproductive age (15-49 years of age)
        select("HH1","HH2","HH6","LN","WDOI","BH9C","wmweight","HH7","BH4C","BHLN")

# nmics_bh5 <- nmics_bh5[c("HH1","HH2","LN","BHLN","BH4C")]
nmics_wm5$ID <- seq.int(nrow(nmics_wm5))
# merge the women data to the birth data
nmics_wm5c <- merge(nmics_wm5, nmics_bh5, by = c("HH1","HH2","LN"),all.y = TRUE)

# recode the line number variable "BHLN" to a character
# variable that starts with "b3_"in each cell
nmics_wm5c$BHLN <- ifelse(nmics_wm5c$BHLN < 10, paste("b3_0",nmics_wm5c$BHLN,sep = ""), paste("b3_",nmics_wm5c$BHLN,sep = ""))
# merge the date of birth variables, b3_, to the women datasets
nmics_wm5m <- dcast(data = nmics_wm5c,formula = ID~BHLN,value.var = "BH4C")
nmics_wm5 <- merge(nmics_wm5,nmics_wm5m,by = c("ID"),all.x = TRUE)
nmics_wm5$wmweight = nmics_wm5$wmweight*1000000

# ==============================================================================
# Preparing Background Variables: Residence, Province, Caste/Ethnic Group and Education
# Place of Residence
nmics_wm5 <- as_tibble(nmics_wm5) %>% 
        rename(residence = HH6, province = HH7)
lblResidence <- factor(c("Urban", "Rural"))
nmics_wm5$residence <- factor(nmics_wm5$residence, levels=1:2, labels=lblResidence) # Create it
levels(nmics_wm5$residence)

# Province
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)
frq(nmics_wm6$province)
# nmics_wm6$province <- factor(nmics_wm6$province, levels=1:7, labels=lblProvince) # Create it
# levels(nmics_wm6$province)
# fct_unique(nmics_wm6$province)

# Educational Attainment: Lower Basic (Gr 1-5), Upper Basic (Gr 6-8), Lower Secondary (9-10), Upper Secondary (11-12), and Higher (13-15)
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)

nmics_wm5$education <- rec(
        nmics_wm5$WB5,
        rec = c("1:5=2; 6:8=3; 9:10=4; 11:12=5; 13:14=6; NA,0,94,99=1"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
frq(nmics_wm5$education)
nmics_wm5$education <- factor(nmics_wm5$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(nmics_wm5$education)
frq(nmics_wm5$education)
flat_table(nmics_wm5, education, residence) # Print cross-tables with labels

# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
nmics_wm5 <- left_join(nmics_wm5, nmics_hh5, by = c("HH1","HH2"))

nmics_wm5$ethnicity <- rec(
        nmics_wm5$HC1C,
        rec = c("1,2,14,20,27,48,49=1; 3:6,10,11,13,24,29,32,35,36,42,45,46,52,53,57,60:62,66:69,74,80,85,88,98,110,112,119,120,124:127,129,132:134,137:138,992 =2; 9,16,18,19,21,25,26,28,30,31,33,34,37,38,43,44,47,50,51,55,56,58,59,63,64,72,73,87,122,131,136,993,995,996,998,999=3; 8,12,15,17,22,23,39:41,54,75,79,113,114,118,123,991=4; 7,82=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
frq(nmics_wm5$ethnicity)
nmics_wm5$ethnicity <- factor(nmics_wm5$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(nmics_wm5$ethnicity)
frq(nmics_wm5$ethnicity)
flat_table(nmics_wm5, ethnicity, residence) # Print crosstables with labels
fct_unique(nmics_wm5$ethnicity)
# ==============================================================================
# Removing some objects
rm(nmics_hh5, nmics_bh5, nmics_wm5m, nmics_wm5c)

# In the code below, the fert function is used to calculate the TFR, where the function arguments are used to identify the relevant names for the sampling strata, sampling clusters, survey weight, date of interview, and women’s date of birth.
# ==============================================================================
fert(nmics_wm5, Indicator = "tfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB")
TFR5 <- fert(nmics_wm5, Indicator = "tfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB",JK = "Yes", Class = "ethnicity")

fert(nmics_wm5, Indicator = "asfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB",JK="Yes")
ASFR5 <- fert(nmics_wm5, Indicator = "asfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB",JK="Yes", Class = "ethnicity")
