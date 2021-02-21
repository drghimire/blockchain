# ---------------------------------------------------------------
# tITLE: Computation of Fertility Rates from NDHS Datasets 2011
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
ndhs_ir4 <- read_dta("./rawdata/ndhs/NPIR/NPIR60FL.DTA") # Read and Check Data (NDHS 2011)
class(ndhs_ir4)
ndhs_ir4 <- ndhs_ir4 %>%
        select(v001, v002, v003, v005, v008, v011, v012, v013, v016, v020, v021, v022, v023, v024, v025, v034, v040, v106, v107, v130, v131, v133, v136, v137, v149, sdist, svillage, sward, s105, bord_01, bidx_01, b3_01, b3_02, b3_03, b3_04, b3_05, b3_06, b3_07, b3_08, b3_09, b3_10, b3_11, b3_12, b3_13, b3_14, b3_15, b3_16, b3_17, b3_18, b3_19, b3_20, awfactt, awfactu, awfactr, awfacte) %>% 
        mutate(district = as.integer(v021/100)) %>% 
        rename(residence = v025) # new_name = old_name
# frq(ndhs_ir1$district)
frq(ndhs_ir4$residence)

# Generating Residence Factor variable.
lblResidence <- factor(c("Urban", "Rural"))
ndhs_ir4$residence <- factor(ndhs_ir4$residence, levels=1:2, labels=lblResidence) # Create it
levels(ndhs_ir4$residence)

# Generating province variable from recoding district variable by using sjmisc packages.
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)
ndhs_ir4$province <- rec(
        ndhs_ir4$district,
        rec = c("35=3; 54,55=6; 1:14=1; 15:19=2; 32:34=2; 20:31=3; 36:45=4; 46:53=5; 56:58=5; 59:66=6; 67:75=7"),
        var.label = "Province",
        as.num = FALSE # we want a factor
)
frq(ndhs_ir4$province)
ndhs_ir4$province <- factor(ndhs_ir4$province, levels=1:7, labels=lblProvince) # Create it
levels(ndhs_ir4$province)
frq(ndhs_ir4$province)
flat_table(ndhs_ir4, province, residence) # Print crosstables with labels
fct_unique(ndhs_ir4$province)
# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
ndhs_ir4$ethnicity <- rec(
        ndhs_ir4$v131,
        rec = c("1:3=1; 7:9=2; 4,996=3; 5:6=4; 10=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
frq(ndhs_ir4$ethnicity)
ndhs_ir4$ethnicity <- factor(ndhs_ir4$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(ndhs_ir4$ethnicity)
frq(ndhs_ir4$ethnicity)
flat_table(ndhs_ir4, ethnicity, residence) # Print crosstables with labels
fct_unique(ndhs_ir4$ethnicity)
# Generating Education Category
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)

ndhs_ir4$education <- rec(
        ndhs_ir4$v149,
        rec = c("0,8=1; 1=2; 2=3; 3=4; 4=5; 5=6"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
frq(ndhs_ir4$education)
ndhs_ir4$education <- factor(ndhs_ir4$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(ndhs_ir4$education)
frq(ndhs_ir4$education)
flat_table(ndhs_ir4, education, residence) # Print crosstables with labels
# ==============================================================================

# Computing Total Fertility Rate (TFR) and Age-Specific Fertility Rates (ASFR)
# help(DHS.rates)
TFR <- fert(ndhs_ir4,Indicator="tfr",JK="Yes")
# Sub-national indicators
TFR <- fert(ndhs_ir4,Indicator="tfr",JK="Yes", Class="province")

ASFR <- fert(ndhs_ir4,Indicator="asfr")
ASFR <- fert(ndhs_ir4,Indicator="asfr", Class = "province")
# ==============================================================================
