
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vectorTools

<!-- badges: start -->
<!-- badges: end -->

The goal of vectorTools is to …

## Installation

You can install the development version of vectorTools like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

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
