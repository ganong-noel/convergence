# Where should and where do low-skill workers move today?
# convergence issue #3
rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(labelled)
library(rprojroot)

fresh_run = FALSE

if (Sys.getenv()[["USER"]] == "peterganong") {
  dropbox_path <- "~/Dropbox/convergence/draft4/work_final/"
} else {
  dropbox_path <- "/scratch/midway2/anisfeld"
  setwd("~/gnlab/convergence/")
}

make_path <- is_git_root$make_fix_file()
out <- make_path("/journalist/eduardo_porter/out")
src <- make_path("/journalist/eduardo_porter/src")



if (fresh_run) {
  raw_ipums <- read_dta(file.path(dropbox_path, "usa_00013.dta")) %>%
    select(-c(empstatd, related, educ, perwt))
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
  
  ipums_low_inc_hh_sample %>% write_rds(file.path(src, "filtered_ipums_with_hhinc.rds")) 
} else {
  ipums_low_inc_hh_sample <- read_rds(file.path(src, "filtered_ipums_with_hhinc.rds")) 
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


# Assign names to migpumas
raw_puma_data <- read_xls(file.path(src,"PUMA2010_crosswalk.xls"))

puma_to_migpuma_2010 <- read_xls(file.path(src, "puma_migpuma1_pwpuma00.xls"), skip=3, 
                      col_names = c("statefip", "puma", "migplac1", "migpuma1")) %>%
  mutate_all(as.numeric)

puma_data_2010 <-
  raw_puma_data %>%
  transmute(name = `PUMA Name`,
            msa = `MSA Code`,
            population_by_puma = `PUMA 2010 Population`,
            puma = as.numeric(`PUMA Code`),
            statefip = as.numeric(`State FIPS Code`))

by_place_2010 <-
  puma_to_migpuma_2010 %>%
    left_join(puma_data_2010, by=c("puma", "statefip")) %>%
    filter(!is.na(name)) %>%
    mutate(name = str_replace_all(name, "( \\(.*|--.*| PUMA)", ""),
           migpuma1 = as.numeric(migpuma1)) %>%
    group_by(statefip, migpuma1, name) %>%
    summarize(population_by_town = sum(population_by_puma))  %>%
    arrange(desc(population_by_town)) 

top_line_place_2010 <-
 by_place_2010 %>%
    group_by(statefip, migpuma1) %>%
    summarize(migpuma_name = first(name),
              place_count = n(),
              top_place_population = first(population_by_town),
              proportion_in_named_place = top_place_population/sum(population_by_town)) %>%
    ungroup()

migpuma_in_migration <-
  ipums_low_inc_hh_sample %>%
    ungroup()  %>%
    # Only include households that moved
    filter(migpuma1 > 0) %>% 
    group_by(statefip, puma) %>%
    summarize(n_in = sum(hhwt, na.rm=TRUE)) %>%
    ungroup() %>%
    left_join(puma_to_migpuma_2010, by = c("puma", "statefip")) %>%
    transmute(migplac1, migpuma1, n_in) %>%
    group_by(migpuma1, migplac1) %>%
    summarise(n_in = sum(n_in))

migpuma_out_migration <-
  ipums_low_inc_hh_sample %>%
    ungroup()  %>%
    # Only include households that moved
    filter(migpuma1 > 0) %>% 
    group_by(migpuma1, migplac1) %>%
    summarize(n_out = sum(hhwt, na.rm=TRUE)) %>%
    ungroup() %>%
    transmute(migpuma1, migplac1, n_out)

migpuma_main_output <-
  ipums_low_inc_hh_sample %>%
  group_by(migpuma1, migplac1) %>%
  summarize(
    median_real_wage = matrixStats::weightedMedian(real_wage, w=hhwt, na.rm = TRUE)) %>%
  full_join(migpuma_out_migration, by=c("migpuma1", "migplac1")) %>%
  full_join(migpuma_in_migration, by=c("migpuma1", "migplac1")) %>%
  mutate(n_out = ifelse(is.na(n_out), 0, n_out),
         n_in = ifelse(is.na(n_in), 0, n_in),
         `net migration`= n_in - n_out) %>%
  left_join(top_line_place_2010, by=c("migpuma1", "migplac1" = "statefip")) %>%
  mutate(migpuma_name = ifelse(is.na(migpuma_name), str_c(migplac1, "_", migpuma1), migpuma_name)) %>%
  ungroup()
  

puma_names <- read_csv(file.path(src,"2010_PUMA_Names.txt"), skip=1, col_names = c("statefip", "migpuma1", "name")) %>%
              mutate_at(c("statefip", "migpuma1"), as.numeric)


migpuma_main_output %>%
  mutate(statefip = ifelse(str_detect(migpuma_name, "^[0-9]*_[0-9]"), as.numeric(str_extract(migpuma_name, "^[0-9]*")), NA),
         migpuma1 = ifelse(str_detect(migpuma_name, "^[0-9]*_[0-9]"), as.numeric(str_extract(migpuma_name, "[0-9]*$")), NA)
  ) %>%
  left_join(puma_names, by = c("statefip", "migpuma1"))  %>% 
  mutate(migpuma_name = ifelse(is.na(name), migpuma_name, name)) %>%
  select(-c(statefip, migpuma1, name, place_count)) %>%
  select(migpuma_name, `net migration`, everything()) %>%
  arrange(desc(`net migration`)) %>%
  write_csv(file.path(out, "low-skill_migration_by_migpuma.csv"))



  