janitors and lawyers
================
Ari Anisfeld
April 21, 2019

R Markdown
----------

``` r
rm(list = ls())
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
    ## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.1       ✔ stringr 1.3.1  
    ## ✔ readr   1.1.1       ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(haven)
library(readxl)
library(labelled)
library(yaml)
library(broom)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(rprojroot)

if (Sys.getenv()[["USER"]] == "peterganong") {
  dropbox_path <- "~/repo/data_ipums/convergence/"
} else {
  dropbox_path <- "~/gnlab/data_ipums/convergence/"
}

make_path <- is_git_root$make_fix_file()
src_path <- make_path("journalist/eduardo_porter/src")
out_path <- make_path("journalist/eduardo_porter/out")
working_path <- make_path("journalist/eduardo_porter/")
source(file.path(working_path, "/code/binscatter.R"))
CONFIG <- yaml.load_file(file.path(working_path, "/code/config.yml"))
source(str_c(CONFIG$lab_code, "prelim.R"))
```

    ## Loading required package: RColorBrewer

    ## Loading required package: testthat

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

``` r
# load main data ----
raw_ipums <- read_dta(file.path(dropbox_path, "usa_00019.dta")) 
```

``` r
filtered_sample <-
  raw_ipums %>%    
  select(-datanum, -serial, -hhwt) %>%
  filter(statefip %in% c(1,5, 13, 28, 45, 9, 36, 34, 6, 53)) %>%
  filter(age >= 25, age < 65, gq == 1) %>%
  mutate(newyork = statefip %in% c(9, 36, 34),
         lawyer = (occ==2100 & year==2017) | (year==1960 & occ==105), 
         janitor = (occ==4220 & year==2017) | (year==1960 & occ==834),
         occupation = case_when(
                          lawyer ~ "lawyer",
                          janitor ~ "janitor",
                          TRUE ~ as.character(NA))) %>%
  filter(!is.na(occupation))


ipums_prepared <-
  filtered_sample %>%
    mutate(incwage = ifelse(incwage >= 999998, NA, incwage),
           inctot = ifelse(inctot >= 999998, NA, inctot),
           annual_housing = ifelse(valueh > 999998, 12*rentgrs, valueh*.05),
           scaled_wage = incwage - annual_housing,
           scaled_income = inctot- annual_housing) %>% 
    select(-age, -valueh, -rentgrs,-lawyer, -janitor, -occ, -gq)

(tristate_v_south <-
    ipums_prepared %>%
      filter(!statefip %in% c(6, 53)) %>%
      select(-statefip, -countyfip) %>%
  group_by(newyork, year, occupation) %>%
  summarise_at(vars(inctot, incwage, scaled_income, scaled_wage), ~weighted.mean(., w=perwt, na.rm=TRUE)))
```

    ## # A tibble: 8 x 7
    ## # Groups:   newyork, year [4]
    ##   newyork year      occupation  inctot incwage scaled_income scaled_wage
    ##   <lgl>   <dbl+lbl> <chr>        <dbl>   <dbl>         <dbl>       <dbl>
    ## 1 FALSE   1960      janitor      1904.   1773.         1565.       1434.
    ## 2 FALSE   1960      lawyer      10714.   2833.         9669.       1788.
    ## 3 FALSE   2017      janitor     23617.  20118.        16621.      13122.
    ## 4 FALSE   2017      lawyer     137704. 115661.       121423.      99379.
    ## 5 TRUE    1960      janitor      3351.   3111.         2760.       2520.
    ## 6 TRUE    1960      lawyer      12286.   4161.        10944.       2819.
    ## 7 TRUE    2017      janitor     29832.  26764.        16533.      13464.
    ## 8 TRUE    2017      lawyer     185880. 159587.       164377.     138081.

``` r
tristate_v_south %>% write_csv(file.path(out_path, "janitor_v_lawyer_tristate_v_south.csv"))
```

``` r
(counties <-
  ipums_prepared %>%
    select(-newyork) %>%
    filter(statefip %in% c(6, 53) & countyfip %in% c(85, 33)) %>%
  group_by(statefip, countyfip, year, occupation)  %>%
  summarise_at(vars(inctot, incwage, scaled_income, scaled_wage), ~weighted.mean(., w=perwt)))
```

    ## # A tibble: 8 x 8
    ## # Groups:   statefip, countyfip, year [4]
    ##   statefip countyfip year  occupation inctot incwage scaled_income
    ##   <dbl+lb>     <dbl> <dbl> <chr>       <dbl>   <dbl>         <dbl>
    ## 1  6              85 1960  janitor    4.14e3   3431.         3404.
    ## 2  6              85 1960  lawyer     1.13e4   4792.        10011.
    ## 3  6              85 2017  janitor    2.82e4  25258.         6439.
    ## 4  6              85 2017  lawyer     1.96e5 169677.       177176.
    ## 5 53              33 1960  janitor    3.11e3   2650.         2527.
    ## 6 53              33 1960  lawyer     1.27e4   2879.        11508.
    ## 7 53              33 2017  janitor    2.88e4  25303.        13991.
    ## 8 53              33 2017  lawyer     1.59e5 137428.       135213.
    ## # … with 1 more variable: scaled_wage <dbl>

``` r
counties %>% write_csv(file.path(out_path, "janitor_v_lawyer_kings_santa_clara.csv"))
```
