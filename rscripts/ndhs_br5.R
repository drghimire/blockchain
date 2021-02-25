# ---------------------------------------------------------------
# tITLE: Computation of Fertility Rates from NDHS Datasets 2016
# ---------------------------------------------------------------
# Authors: Dhruba Raj Ghimire
# Date: 2021-02-21

# Preparation R
rm(list=ls()) # Clear Working Memory
cat("\f") # Clear the R Console, alternatively press the “Ctrl” + “ L ” keys simultaneously.
setwd("C:/Users/dghimire/Dropbox (Personal)/nexus/blockchain")
(.packages())          # First, let’s check the currently loaded packages after restarting RStudio
# installed.packages()  # Returns a matrix with a row for each package that has been installed
# available.packages()  # Returns a matrix of details corresponding to packages currently available at one or more repositories

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
ndhs_br5 <- read_dta("./rawdata/ndhs/NPBR/NPBR7HFL.DTA") # Read and Check Data (NDHS 2016)
class(ndhs_br5)
ndhs_br5 <- ndhs_br5 %>%
        select(v001, v002, v003, v005, v008, v011, v012, v013, v016, v020, v021, v022, v023, v024, v025, v034, v040, v106, v107, v130, v131, v133, v136, v137, v149, sdist, bord, bidx, b3, b7, awfactt, awfactu, awfactr, awfacte, awfactw) %>% 
        # mutate(district = as.integer(v021/100)) %>% 
        rename(province = v024, residence = v025, district = sdist) # new_name = old_name
# frq(ndhs_ir1$district)
frq(ndhs_br5$residence)

# Generating Residence Factor variable.
lblResidence <- factor(c("Urban", "Rural"))
ndhs_br5$residence <- factor(ndhs_br5$residence, levels=1:2, labels=lblResidence) # Create it
levels(ndhs_br5$residence)

# Generating province variable from recoding district variable by using sjmisc packages.
lblProvince <- factor(c("Province-1", "Province-2", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpaschim"))
levels(lblProvince)

ndhs_br5$province <- factor(ndhs_br5$province, levels=1:7, labels=lblProvince) # Create it
levels(ndhs_br5$province)
frq(ndhs_br5$province)
flat_table(ndhs_br5, province, residence) # Print crosstables with labels
fct_unique(ndhs_br5$province)
# Generating Caste/Ethnicity Group
lblEthnicity <- factor(c("Aryan", "Janjati", "Madhesi", "Dalit", "Muslim"))
levels(lblEthnicity)
ndhs_br5$ethnicity <- rec(
        ndhs_br5$v131,
        rec = c("1:3=1; 7:9=2; 4,96=3; 5:6=4; 10=5"),
        var.label = "Caste/Ethnicity",
        as.num = FALSE # we want a factor
)
ndhs_br5$ethnicity <- factor(ndhs_br5$ethnicity, levels=1:5, labels=lblEthnicity) # Create it
levels(ndhs_br5$ethnicity)
frq(ndhs_br5$ethnicity)
fct_unique(ndhs_br5$ethnicity)
# Generating Education Category
lblEducation <- factor(c("no education", "some primary", "completed primary", "completed lower-secondary", "completed upper-secondary", "completed post-secondary"))
levels(lblEducation)

ndhs_br5$education <- rec(
        ndhs_br5$v149,
        rec = c("0=1; 1=2; 2=3; 3=4; 4=5; 5=6"),
        var.label = "Educational Attainment",
        as.num = FALSE # we want a factor
)
ndhs_br5$education <- factor(ndhs_br5$education, ordered = TRUE, levels=1:6, labels=lblEducation) # Create it
fct_unique(ndhs_br5$education)
frq(ndhs_br5$education)
flat_table(ndhs_br5, education, residence) # Print crosstables with labels
# ==============================================================================

# Computing Child Mortality Rates
# help(DHS.rates)
cmr5 <- chmort(ndhs_br5,JK="Yes")
cmr5 <- chmort(ndhs_br5,JK="Yes", Class = "ethnicity")

(chmortp(ndhs_br5)) # Childhood Mortality Probabilities
# ==============================================================================
