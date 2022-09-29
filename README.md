
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

tt <- ukb_extract("f.34\\.")
cools <- fread("/nfs/AGE/UKB/data/180524/r/ukb22140.tab", nrows = 0)


  
tibble(x=c(colnames(cools), c("f.20.0.0", "f.20.0.4", "f.20"))) %>% 
  filter(str_detect(x, "f.20$"))
  filter(str_detect(x, "f.20\\."))
```

The matching against the field name only check if the provided string
matches any of the column names. Therefore, ukb_extract(“f.32”) would
also match any columns starting with “f.32\*

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
#> ✔ tibble  3.1.8      ✔ dplyr   1.0.10
#> ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
#> ✔ readr   2.1.2      ✔ forcats 0.5.2 
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
ex <- tibble(string=c("f.32", "f.322", "f.3220.0.1", "f.20005.0.0"))

filter(ex, str_detect(string, "f.32^"))
#> # A tibble: 0 × 1
#> # … with 1 variable: string <chr>
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
