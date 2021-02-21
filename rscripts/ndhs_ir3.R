# ---------------------------------------------------------------
# tITLE: Computation of Fertility Rates from NDHS Datasets 2006
# ---------------------------------------------------------------
# Authors: Dhruba Raj Ghimire
# Date: 2021-02-12

# Preparation R
rm(list=ls()) # Clear Working Memory
cat("\f") # Clear the R Console, alternatively press the “Ctrl” + “ L ” keys simultaneously.
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
# install.packages("DHS.rates")
# ==============================================================================

library(tidyverse) # readr, tidyr, dplyr, purrr, tibble, stringr, ggplot2, and forcats 
library(haven)
library(readxl)
library(data.table)
set.seed(1L)
library(dtplyr)
# library(labelled)
# library(sjlabelled)
library(sjmisc)
library(DHS.rates)
# ==============================================================================

# Prepare Datasets: (i) Province, (ii) Residence, (iii) Caste/Ethnicity, and (iv) Education 
# (i) For NDHS 1996 Datasets
ndhs_ir3 <- read_dta("./rawdata/ndhs/NPIR/NPIR51FL.DTA") # Read and Check Data (NDHS 2006)
class(ndhs_ir3)
ndhs_ir3 <- ndhs_ir3 %>%
        select(v001, v002, v003, v005, v008, v011, v012, v013, v016, v020, v021, v022, v023, v024, v025, v034, v106, v130, v131, v133, v136, v137, v149, sdist, svill, sward, bord_01, bidx_01, b3_01, b3_02, b3_03, b3_04, b3_05, b3_06, b3_07, b3_08, b3_09, b3_10, b3_11, b3_12, b3_13, b3_14, b3_15, b3_16, b3_17, b3_18, b3_19, b3_20, awfactt, awfactu, awfactr, awfacte) %>% 
        mutate(district = as.integer(v021/100)) %>% 
        rename(residence = v025) # new_name = old_name
# frq(ndhs_ir1$district)
frq(ndhs_ir3$residence)

# Generating Residence Factor variable.
lblResidence <- factor(c("Urban", "Rural"))
ndhs_ir3$residence <- factor(ndhs_ir3$residence, levels=1:2, labels=lblResidence) # Create it
levels(ndhs_ir3$residence)

# Generating province variable from recoding district variable by using sjmisc packages.
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)
ndhs_ir3$province <- rec(
        ndhs_ir3$district,
        rec = c("35=3; 54,55=6; 1:14=1; 15:19=2; 32:34=2; 20:31=3; 36:45=4; 46:53=5; 56:58=5; 59:66=6; 67:75=7"),
        var.label = "Province",
        as.num = FALSE # we want a factor
)
frq(ndhs_ir3$province)
ndhs_ir3$province <- factor(ndhs_ir3$province, levels=1:7, labels=lblProvince) # Create it
levels(ndhs_ir3$province)
frq(ndhs_ir3$province)
flat_table(ndhs_ir3, province, residence) # Print crosstables with labels
fct_unique(ndhs_ir3$province)
# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
ndhs_ir3$ethnicity <- rec(
        ndhs_ir3$v131,
        rec = c("1,2,14,20,27,48,49,90,96=1; 3:6,10,11,13,24,29,32,35,36,42,45,46,52,57,61,62,67,69,80,86=2; 9,16,18,19,21,25,26,28,30,31,33,34,37,38,43,44,47,50,51,55,56,58,59,64,72,73,76,88,91=3; 8,12,15,17,22,23,39,40,41,54,75,79,84=4; 7=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
frq(ndhs_ir3$ethnicity)
ndhs_ir3$ethnicity <- factor(ndhs_ir3$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(ndhs_ir3$ethnicity)
frq(ndhs_ir3$ethnicity)
flat_table(ndhs_ir3, ethnicity, residence) # Print crosstables with labels
fct_unique(ndhs_ir3$ethnicity)
# Generating Education Category
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)

ndhs_ir3$education <- rec(
        ndhs_ir3$v149,
        rec = c("0=1; 1=2; 2=3; 3=4; 4=5; 5=6"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
frq(ndhs_ir3$education)
ndhs_ir3$education <- factor(ndhs_ir3$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(ndhs_ir3$education)
frq(ndhs_ir3$education)
flat_table(ndhs_ir3, education, residence) # Print crosstables with labels
# ==============================================================================

# Computing total fertility rate
# help(DHS.rates)
TFR <- fert(ndhs_ir3,Indicator="tfr")
TFR <- fert(ndhs_ir3,Indicator="tfr",JK="Yes")
# Sub-national indicators
TFR <- fert(ndhs_ir3,Indicator="tfr",JK="Yes", Class="province")

ASFR <- fert(ndhs_ir3,Indicator="asfr")
ASFR <- fert(ndhs_ir3,Indicator="asfr", Class = "province")
# ==============================================================================
