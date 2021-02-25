# ---------------------------------------------------------------
# tITLE: Computation of Fertility Rates from NDHS Datasets 2001
# ---------------------------------------------------------------
# Authors: Dhruba Raj Ghimire
# Date: 2021-02-12

# Preparation R
rm(list=ls()) # Clear Working Memory
cat("\f") # Clear R Console, alternatively press the “Ctrl” + “ L ” keys simultaneously.
setwd("C:/Users/dghimire/Dropbox (Personal)/nexus/blockchain")
(.packages())          # First, let’s check the currently loaded packages after restarting RStudio
# installed.packages()  # Returns a matrix with a row for each package that has been installed
# available.packages()  # Returns a matrix of details corresponding to packages currently available at one or more repositories

# ==============================================================================
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
ndhs_br2 <- read_dta("./rawdata/ndhs/NPBR/NPBR41FL.DTA") # Read and Check Data (NDHS 2001)
class(ndhs_br2)
ndhs_br2 <- ndhs_br2 %>%
        select(v001, v002, v003, v005, v008, v011, v012, v013, v016, v020, v021, v022, v023, v024, v025, v034, v106, v130, v131, v133, v136, v137, v149, sdistr, svillag, sward, bord, bidx, b3, b7, awfactt, awfactu, awfactr, awfacte) %>% 
        mutate(district = as.integer(v021/100)) %>% 
        rename(residence = v025) # new_name = old_name
# frq(ndhs_ir1$district)
frq(ndhs_br2$residence)

# Generating Residence Factor variable.
lblResidence <- factor(c("Urban", "Rural"))
ndhs_br2$residence <- factor(ndhs_br2$residence, levels=1:2, labels=lblResidence) # Create it
levels(ndhs_br2$residence)

# Generating province variable from recoding district variable by using sjmisc packages.
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)
ndhs_br2$province <- rec(
        ndhs_br2$district,
        rec = c("35=3; 54,55=6; 1:14=1; 15:19=2; 32:34=2; 20:31=3; 36:45=4; 46:53=5; 56:58=5; 59:66=6; 67:75=7"),
        var.label = "Province",
        as.num = FALSE # we want a factor
)
ndhs_br2$province <- factor(ndhs_br2$province, levels=1:7, labels=lblProvince) # Create it
levels(ndhs_br2$province)
frq(ndhs_br2$province)
fct_unique(ndhs_br2$province)
# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
ndhs_br2$ethnicity <- rec(
        ndhs_br2$v131,
        rec = c("2,8,9,26,31:34,60,96=1; 10,16,17,21,35:42,44,45,47,48,53,55,61,62=2; 1,3,4,6,7,11,12,14,15,19,20,22,27:30=3; 5,23:25,46,56:59=4; 13=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
ndhs_br2$ethnicity <- factor(ndhs_br2$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(ndhs_br2$ethnicity)
frq(ndhs_br2$ethnicity)
# Generating Education Category
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)

ndhs_br2$education <- rec(
        ndhs_br2$v149,
        rec = c("0=1; 1=2; 2=3; 3=4; 4=5; 5=6"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
ndhs_br2$education <- factor(ndhs_br2$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(ndhs_br2$education)
frq(ndhs_br2$education)
flat_table(ndhs_br2, education, residence) # Print crosstables with labels
# ==============================================================================

# Computing Child Mortality Rates
# help(DHS.rates)
cmr2 <- chmort(ndhs_br2,JK="Yes")
cmr2 <- chmort(ndhs_br2,JK="Yes", Class = "ethnicity")
(chmortp(ndhs_br2)) # Childhood Mortality Probabilities
# ==============================================================================
