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
nmics_bh5 <- read_sav("./rawdata/nmics/np5_bh.SAV") # Read and Check Data (NMICS 2019), ID(HH1 HH2 LN BHLN)
nmics_hh5 <- read_sav("./rawdata/nmics/np5_hh.SAV") # Read and Check Data (NMICS 2019), ID(HH1 HH2)

class(nmics_bh5)
class(nmics_hh5)
# keep the necessary variables only: Residence (HH6), Province (HH7), Educational Attainment (WB5) 
nmics_bh5 <- nmics_bh5 %>% # Birth history of ever-married Women in reproductive age (15-49 years of age)
        select("HH1","HH2","HH6","HH7", "LN","WDOI","BH9C","wmweight","HH7","BH4C","BHLN")
nmics_hh5 <- nmics_hh5 %>% # Households
        select("HH1","HH2","HC1C","HH9") %>% 
        filter(HH9==1)
nmics_bh5$wmweight = nmics_bh5$wmweight*1000000

# Preparing Background Variables: Residence, Province, Caste/Ethnic Group and Education
# Place of Residence
nmics_bh5 <- as_tibble(nmics_bh5) %>% 
        rename(residence = HH6, province = HH7)
lblResidence <- factor(c("Urban", "Rural"))
nmics_bh5$residence <- factor(nmics_bh6$residence, levels=1:2, labels=lblResidence) # Create it
levels(nmics_bh5$residence)
# Province
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)
frq(nmics_bh5$province)
# nmics_bh5$province <- factor(nmics_bh5$province, levels=1:7, labels=lblProvince) # Create it
# levels(nmics_bh5$province)
# frq(nmics_bh5$province)
# flat_table(nmics_bh5, province, residence) # Print crosstables with labels
# fct_unique(nmics_bh5$province)

# Educational Attainment: Lower Basic (Gr 1-5), Upper Basic (Gr 6-8), Lower Secondary (9-10), Upper Secondary (11-12), and Higher (13-15)
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)
nmics_bh6$education <- rec(
        nmics_bh5$WB5,
        rec = c("1:5=2; 6:8=3; 9:10=4; 11:12=5; 13:14=6; NA,0,94,99=1"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
frq(nmics_bh5$education)
nmics_bh5$education <- factor(nmics_bh5$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(nmics_bh5$education)
frq(nmics_bh6$education)
flat_table(nmics_bh5, education, residence) # Print cross-tables with labels
# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
nmics_bh5 <- left_join(nmics_bh5, nmics_hh5, by = c("HH1","HH2"))
nmics_bh5$ethnicity <- rec(
        nmics_bh5$HC1C,
        rec = c("1,2,14,20,27,48,49=1; 3:6,10,11,13,24,29,32,35,36,42,45,46,52,53,57,60:62,66:69,74,80,85,88,98,110,112,119,120,124:127,129,132:134,137:138,992 =2; 9,16,18,19,21,25,26,28,30,31,33,34,37,38,43,44,47,50,51,55,56,58,59,63,64,72,73,87,122,131,136,993,995,996,998,999=3; 8,12,15,17,22,23,39:41,54,75,79,113,114,118,123,991=4; 7,82=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
frq(nmics_bh5$ethnicity)
nmics_bh5$ethnicity <- factor(nmics_bh5$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(nmics_bh5$ethnicity)
frq(nmics_bh5$ethnicity)
flat_table(nmics_bh5, ethnicity, residence) # Print crosstables with labels
fct_unique(nmics_bh5$ethnicity)

rm(nmics_hh5)

# Similarly in the code below, the chmort function is used to calculate the five childhood mortality rates based on the 2017 Sierra Leone MICS.
chmort(nmics_bh5, Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Date_of_birth = "BH4C", Age_at_death = "BH9C")
chmort5 <- chmort(nmics_bh5, Strata = "province", Cluster = "HH1", Weight = "wmweight", Date_of_interview = "WDOI", Date_of_birth = "BH4C", Age_at_death = "BH9C", Class = "ethnicity")
# ==============================================================================
