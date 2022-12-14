
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vectorTools

The goal of vectorTools is to provide a common set of functions used to
analyze data on vector, a local compute server at the institute for
medical epidemiology and biostatistics.

## Installation

You can install the development version of vectorTools like so:

``` r
library(devtools)
install_github("ararder/vectorTools")
```

## UK Biobank

On vector, phenotype for UKB is stored in three separate files. The
files are very large, so it is not feasible to read the entire dataset
in, and then drop the variables you dont want. For this, there is the
ukb_extract() function, which takes as argument a vector of fields to
extract. The function will first see if the field exists in each of the
datasets, and then extract it if it exists.

``` r
library(tidyverse)
library(vectorTools)
library(data.table)

tt <- ukb_extract("f.20446")
cools <- fread("/nfs/AGE/UKB/data/180524/r/ukb22140.tab", nrows = 0)


  
tibble(x=c(colnames(cools), c("f.20.0.0", "f.20.0.4", "f.20"))) %>% 
  filter(str_detect(x, "f.20$"))
  filter(str_detect(x, "f.20\\."))
```

# Extracting ICD definitions from UKB

A common task is to get the list of individuals with atleast one
instance of an ICD code. In UKB, many fields are stored in a wide
format - you have many columns for each individual, and the code could
by in any of these columns.

For this i use check_for_code, which checks if the specified code is is
any of the columns in the dataset provided (except column 1, which is
assumed to be id)

``` r
library(tidyverse)
library(vectorTools)

# assuming you have read permission for UKB
# list of all unique icd codes
icd_data <- ukb_extract("f.41270")

anxiety <- check_for_code(codes = "F43", col_name = "anxiety", data = icd_data)


# many definitions, with some definitions having more than 1 possible code
codes <- list(
  bip =  c("F30", "F31"),
  mdd_recur = c("F33"),
  scz = c("F20", "F25"),
  mdd_single = c("F32")
)


# Create several definitions at once, with varying number of icd codes used
# for each definition
cases <- map2_df(codes, names(codes), check_for_code, data = icd_codes)
```

# Crime 3

``` r
library(vectorTools)
# Establish connection to Crime3
con <- crime3_con()

# set the ICD codes you are interested in
diagnosis_codes <- c("F25", "F29")

# extract all observations any of those codes from specialist care
scz_dia <- crime3_icd(diagnosis_codes)

# or from primary care
scz_dia_primary <- crime3_icd(diagnosis_codes, specialist=FALSE)

# For each individual, find the first diagnosis of each unique diagnosis
# with 3 digits codes like F3222A becomes F32. 
scz_first_dia <- crime3_first_unique_diagnosis(scz_dia, icd_digits = 3)

# we can increase the number of digits for each unique diagnosis, so that we find
# the first occurence of both F323 and F322 if the individual has both.
scz_first_dia_sub <- crime3_first_unique_diagnosis(scz_dia, icd_digits = 4)

## basic example code
```
