# Where should and where do low-skill workers move today?
# convergence issue #3
rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(labelled)

fresh_run = FALSE

if (Sys.getenv()[["USER"]] == "peterganong") {
  dropbox_path <- "~/Dropbox/convergence/draft4/work_final/"
} else {
  dropbox_path <- "~/gnlab/convergence/journalist/eduardo_porter"
}

if (fresh_run) {
  raw_ipums <- read_dta(file.path(dropbox_path, "usa_00013.dta")) %>%
    select(-c(empstatd, migplac1, related, educ, perwt, puma, statefip))
  # print(object.size(raw_ipums), unit="Gb")
  # sample <- raw_ipums %>% filter(serial %% 10 == 1)
  ipums_low_inc_hh_sample <-
      raw_ipums %>%
        group_by(serial) %>%
        mutate(incwage = ifelse(incwage >= 999998, NA, incwage),
               hhinc = sum(incwage, na.rm = TRUE),
               annual_housing = 12*ifelse(owncost == 99999, rentgrs, owncost),
               real_wage = hhinc - annual_housing) %>%
        filter(age >= 25, age <= 65, educd < 101, relate == 1, empstat %in% c(1,2))
} else {
  ipums_low_inc_hh_sample <- read_rds(file.path(dropbox_path, "filtered_ipums_with_hhinc.rds")) 
}


# Analysis

msa_out_migration <-
  ipums_low_inc_hh_sample %>%
  ungroup()  %>%
  # Only include households that moved
  filter(migpuma1 > 0) %>%
  group_by(migmet131) %>%
    summarize(n_out = sum(hhwt, na.rm=TRUE)) %>%
  transmute(migmet131, n_out)

msa_main_output <-
  ipums_low_inc_hh_sample %>%
  group_by(met2013) %>%
  summarize(
    median_real_wage = matrixStats::weightedMedian(real_wage, w=hhwt, na.rm = TRUE),
    n_in = sum(ifelse(migpuma1 > 0, hhwt,0))) %>%
  full_join(msa_out_migration, by=c("met2013" = "migmet131")) %>%
  mutate(n_out = ifelse(is.na(n_out), 0, n_out),
         `net migration`= n_in - n_out,
         name = to_factor(met2013))

msa_main_output %>%
  select(name, `net migration`, everything(), -met2013) %>%
  arrange(desc(`net migration`)) %>%
  write_csv("low-skill_migration_by_msa.csv")
