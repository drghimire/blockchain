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
# install.packages("sjmisc")	        # Re-coding Variables
# install.packages("reshape2")	        # Flexibly Reshape Data
# install.packages("DHS.rates")
# ==============================================================================
# Loading Packages 
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
nmics_hh6 <- read_sav("./rawdata/nmics/np6_hh.SAV") # Read and Check Data (NMICS 2019), ID(HH1 HH2)
nmics_wm6 <- read_sav("./rawdata/nmics/np6_wm.SAV") # Read and Check Data (NMICS 2019), ID(HH1 HH2 LN)
nmics_bh6 <- read_sav("./rawdata/nmics/np6_bh.SAV") # Read and Check Data (NMICS 2019), ID(HH1 HH2 LN BHLN)

class(nmics_bh6)
class(nmics_wm6)
# keep the necessary variables only: Residence (HH6), Province (HH7), Educational Attainment (WB5) 
nmics_hh6 <- nmics_hh6 %>% # Households
        select("HH1","HH2","HC2","HH46") %>% 
        filter(HH46==1)
nmics_wm6 <- nmics_wm6 %>% # Women in reproductive age (15-49 years of age)
        select("HH1","HH2","HH6","LN","WDOI","WDOB","wmweight","HH7","welevel2","WM17") %>% 
        filter(WM17==1)
nmics_bh6 <- nmics_bh6 %>% # Birth history of ever-married Women in reproductive age (15-49 years of age)
        select("HH1","HH2","HH6","LN","WDOI","BH9C","wmweight","HH7","BH4C","BHLN")

# nmics_bh5 <- nmics_bh5[c("HH1","HH2","LN","BHLN","BH4C")]
nmics_wm6$ID <- seq.int(nrow(nmics_wm6))
# merge the women data to the birth data
nmics_wm6c <- merge(nmics_wm6, nmics_bh6, by = c("HH1","HH2","LN"),all.y = TRUE)


# recode the line number variable "BHLN" to a character
# variable that starts with "b3_"in each cell
nmics_wm6c$BHLN <- ifelse(nmics_wm6c$BHLN < 10, paste("b3_0",nmics_wm6c$BHLN,sep = ""), paste("b3_",nmics_wm6c$BHLN,sep = ""))
# merge the date of birth variables, b3_, to the women datasets
nmics_wm6m <- dcast(data = nmics_wm6c,formula = ID~BHLN,value.var = "BH4C")
nmics_wm6 <- merge(nmics_wm6,nmics_wm6m,by = c("ID"),all.x = TRUE)
nmics_wm6$wmweight = nmics_wm6$wmweight*1000000

# Preparing Background Variables: Residence, Province, Caste/Ethnic Group and Education
# Place of Residence
nmics_wm6 <- as_tibble(nmics_wm6) %>% 
        rename(residence = HH6, province = HH7)
lblResidence <- factor(c("Urban", "Rural"))
nmics_wm6$residence <- factor(nmics_wm6$residence, levels=1:2, labels=lblResidence) # Create it
levels(nmics_wm6$residence)
# Province
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)
frq(nmics_wm6$province)
nmics_wm6$province <- factor(nmics_wm6$province, levels=1:7, labels=lblProvince) # Create it
levels(nmics_wm6$province)
frq(nmics_wm6$province)
flat_table(nmics_wm6, province, residence) # Print crosstables with labels
fct_unique(nmics_wm6$province)
# Educational Attainment
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)
nmics_wm6$education <- rec(
        nmics_wm6$welevel2,
        rec = c("0=1; 3=2; 4=3; 6=4; 7=5; 8=6"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
frq(nmics_wm6$education)
nmics_wm6$education <- factor(nmics_wm6$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(nmics_wm6$education)
frq(nmics_wm6$education)
flat_table(nmics_wm6, education, residence) # Print cross-tables with labels
# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
nmics_wm6 <- left_join(nmics_wm6, nmics_hh6, by = c("HH1","HH2"))
nmics_wm6$ethnicity <- rec(
        nmics_wm6$HC2,
        rec = c("1,2,14,20,27,48,49,106,996=1; 3:6,10,11,13,24,29,32,35,36,42,45,46,52,53,57,60,61,66:69,71,74,77,78,80,81,83,84,87:91,95,98,104,105,111,112,114,117:119,127=2; 9,16,18,19,21,25,26,28,30,31,33,34,37,38,43,44,47,50,51,55,56,58,59,63,64,72,73,76,116,121,128=3; 8,12,15,17,22,23,39:41,54,70,75,82,101,108,113,126=4; 7=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
frq(nmics_wm6$ethnicity)
nmics_wm6$ethnicity <- factor(nmics_wm6$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(nmics_wm6$ethnicity)
frq(nmics_wm6$ethnicity)
flat_table(nmics_wm6, ethnicity, residence) # Print crosstables with labels
fct_unique(nmics_wm6$ethnicity)

# Removing some objects
rm(nmics_hh6, nmics_wm6m, nmics_wm6c)

# In the code below, the fert function is used to calculate the TFR, where the function arguments are used to identify the relevant names for the sampling strata, sampling clusters, survey weight, date of interview, and women’s date of birth.
# ==============================================================================
fert(nmics_wm6, Indicator = "tfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB")
TFR6 <- fert(nmics_wm6, Indicator = "tfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB",JK = "Yes", Class = "ethnicity")

fert(nmics_wm6, Indicator = "asfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB",JK="Yes")
ASFR6 <- fert(nmics_wm6, Indicator = "asfr", Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Woman_DOB = "WDOB",JK="Yes", Class = "ethnicity")
