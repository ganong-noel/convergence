# Where should and where do low-skill (high-skill) workers move today?
# convergence issue #3 and #6

rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(labelled)
library(rprojroot)

any_na <- function(data) data %>% ungroup() %>% mutate_all(is.na) %>% summarise_all(sum)


if (Sys.getenv()[["USER"]] == "peterganong") {
  dropbox_path <- "~/Dropbox/convergence/draft4/work_final/"
  # point the working directory to your local repo
  # setwd("~/../convergence/")
} else {
  dropbox_path <- "~/gnlab/data_ipums/convergence/" #"/scratch/midway2/anisfeld/"
  setwd("~/gnlab/convergence/")
}

make_path <- is_git_root$make_fix_file()
src_path <- make_path("journalist/eduardo_porter/src")
out_path <- make_path("journalist/eduardo_porter/out")


##########################
# Data loading functions #
##########################

load_ipums_sample <- function(level, 
                              fresh_run = TRUE,
                              raw_ipums_in_memory = FALSE,
                              src_path. = src_path,
                              dropbox_path. = dropbox_path)
  {
  
  stopifnot(level %in% c("high_skill", "low_skill"))
  
  if (fresh_run) {
    if (!raw_ipums_in_memory){
      raw_ipums <- read_dta(file.path(dropbox_path., "full_ipums_extract.dta")) %>% 
        select(-c(empstatd, related, educ, perwt))
      }
     
      ipums_sample <-
          raw_ipums %>%
            group_by(serial) %>%
            mutate(incwage = ifelse(incwage >= 999998, NA, incwage),
                   hhinc = sum(incwage, na.rm = TRUE),
                   annual_housing = 12*ifelse(owncost == 99999, rentgrs, owncost),
                   real_wage = hhinc - annual_housing) %>%
            filter(age >= 25, age <= 65, relate == 1, empstat %in% c(1,2)) %>%
            filter(if(level == "low-skill") educd < 101 else  educd >= 101)
    
      ipums_sample %>%  write_rds(file.path(src_path., glue::glue("{level}_filtered_ipums_with_hhinc.rds"))) 
      
    } else {
      ipums_sample <- read_rds(file.path(src_path., glue::glue("{level}_filtered_ipums_with_hhinc.rds"))) 
    }
  
  ipums_sample
  }
    


load_puma_to_migpuma <- function(puma_population_from_scratch, 
                                 src_path. = src_path){
    state_fip_to_name <- read_csv(file.path(src_path., "state_fip_to_name.csv"))
    
    puma_to_migpuma_2010 <- read_xls(file.path(src_path., "puma_migpuma1_pwpuma00.xls"), skip=3, 
                                     col_names = c("statefip", "puma", "migplac1", "migpuma1")) %>%
                            mutate_all(as.numeric) %>% 
                            left_join(state_fip_to_name, by = c("statefip"))
    }
    
load_top_line_place <- function(puma_to_migpuma_2010,
                                puma_population_from_scratch=FALSE,
                                src_path. = src_path){
  
    puma_names <- read_csv(file.path(src_path.,"2010_PUMA_Names.txt"), 
                           skip=1, col_names = c("statefip", "puma", "name")) %>%
                  mutate_at(c("statefip", "puma"), as.numeric)
    
    if(puma_population_from_scratch){
      # This is directly from IPUMS and takes up about 400 MB in memory
      # You migth also fail here because of git lfs
      puma_population <- read_dta(file.path(src_path., "obs_by_puma.dta")) %>%
        group_by(statefip, puma) %>%
        summarize(population = sum(hhwt))
      puma_population %>% write_csv(file.path(src_path., "puma_population.csv"))
    } else {
      puma_population <- read_csv(file.path(src_path., "puma_population.csv"))
    }
      
    puma_data_2016 <-
      puma_population %>%
      # this drops Puerto Rico and other islands
      left_join(puma_names, by=c("statefip", "puma"))
    
    by_place_2016 <-
      puma_data_2016 %>%
      left_join(puma_to_migpuma_2010, by=c("puma", "statefip")) %>%
      mutate(name = str_replace_all(name, "( \\(.*|--.*| PUMA)", ""),
             migpuma1 = as.numeric(migpuma1)) %>%
      group_by(statefip, migpuma1, name) %>%
      summarize(population_by_town = sum(population))  %>%
      # this step helps capture name of most populous puma
      arrange(desc(population_by_town)) 
    
    top_line_place_2016 <-
      by_place_2016 %>%
      group_by(statefip, migpuma1) %>%
      summarize(migpuma_name = first(name),
                migpuma_population = sum(population_by_town),
                top_place_population = first(population_by_town),
                proportion_in_named_place = top_place_population/sum(population_by_town)) %>%
      ungroup()
    
    top_line_place_2016
}



############################
#  Data Munging Functions  #
############################
get_msa_net_migration_df <- function(level, 
                                    ipums_data,  
                                     src_path. = src_path)
  {
  
  msa_data <- read_xls(file.path(src_path.,"MSA2013_PUMA2010_crosswalk.xls"))  %>%
              mutate_at(c("MSA Code", "PUMA Code", "State FIPS Code"), as.numeric) %>% 
              select(starts_with("MSA")) %>%
              dplyr::distinct(`MSA Title`, .keep_all=TRUE)
  
  msa_out_migration <-
    ipums_sample %>%
    ungroup()  %>%
    # Only include households that moved
    filter(migpuma1 > 0) %>%
    group_by(migmet131) %>%
      summarize(n_out = sum(hhwt, na.rm=TRUE)) %>%
    transmute(migmet131, n_out)
  
  msa_main_output <-
    ipums_sample %>%
    group_by(met2013) %>%
    summarize(
      n_obs_for_median_real_wage = n(),
      median_real_wage = matrixStats::weightedMedian(real_wage, w=hhwt, na.rm = TRUE),
      n_in = sum(ifelse(migpuma1 > 0, hhwt,0))) %>%
    full_join(msa_out_migration, by=c("met2013" = "migmet131")) %>%
    left_join(msa_data,
              by =c("met2013" = "MSA Code")) %>%
    mutate(n_out = ifelse(is.na(n_out), 0, n_out),
             `raw net migration`= n_in - n_out,
             `net migration` = `raw net migration`/`MSA 2010 Population`) %>%
    select(`MSA Title`, `net migration`,`raw net migration`, everything()) %>%
    arrange(desc(`net migration`)) 
  
}  

get_migpuma_net_migration_df <- function(level, 
                                    ipums_sample)
  {
    
    migpuma_in_migration <-
      ipums_sample %>%
        ungroup()  %>%
        # Only include households that moved
        filter(migplac1 > 0) %>% 
        group_by(statefip, puma) %>%
        summarize(n_obs_n_in = n(), 
                  n_in = sum(hhwt, na.rm=TRUE),
                  n_in_not_foreign = sum(ifelse(migplac1  < 100, hhwt, 0))) %>%
        ungroup() %>%
        left_join(puma_to_migpuma_2010, by = c("puma", "statefip")) %>%
        group_by(migpuma1, migplac1) %>%
        summarise(n_in = sum(n_in),
                  n_obs_n_in = sum(n_obs_n_in),
                  n_in_not_foreign = sum(n_in_not_foreign))
    
    migpuma_out_migration <-
      ipums_sample %>%
        ungroup()  %>%
        # Only include households that moved
        filter(migplac1 > 0) %>% 
        group_by(migpuma1, migplac1) %>%
        summarize(n_out = sum(hhwt, na.rm=TRUE),
                  n_out_not_foreign = sum(ifelse(migplac1 < 100, hhwt, 0))) %>%
        ungroup() %>%
        transmute(migpuma1, migplac1, n_out, n_out_not_foreign)
    
    migpuma_real_wages <-
      ipums_sample %>% 
        ungroup() %>%
        select(real_wage, statefip, puma, hhwt) %>%
        left_join(puma_to_migpuma_2010, by = c("statefip", "puma")) %>%   
        group_by(migpuma1, migplac1) %>%
        summarize(
          state = first(stname), 
          n_obs_for_median_real_wage = n(),
          median_real_wage = matrixStats::weightedMedian(real_wage, w=hhwt, na.rm = TRUE)
        )
    
    migpuma_main_output <-
      migpuma_real_wages %>%
      left_join(top_line_place_2016, by=c("migpuma1", "migplac1" = "statefip")) %>%
      full_join(migpuma_out_migration, by=c("migpuma1", "migplac1")) %>%
      full_join(migpuma_in_migration, by=c("migpuma1", "migplac1")) %>%
      mutate(n_out = ifelse(is.na(n_out), 0, n_out),
             n_in = ifelse(is.na(n_in), 0, n_in),
             `raw net migration` = n_in - n_out, 
             `net migration`= `raw net migration`/migpuma_population,
             `net migration (not foreign)` = (n_in_not_foreign - n_out_not_foreign)/migpuma_population) %>%
      mutate(migpuma_name = ifelse(is.na(migpuma_name), str_c(migplac1, "_", migpuma1), migpuma_name)) %>%
      ungroup() %>%
      select(state, migpuma_name, `net migration`, `raw net migration`, everything(), -c(migplac1, n_in_not_foreign, n_out_not_foreign)) %>%
      arrange(desc(`net migration`)) 
    
  }

############
# RUN CODE #
############
puma_to_migpuma_2010 <- load_puma_to_migpuma()
top_line_place_2016 <- load_top_line_place(puma_to_migpuma_2010)

fresh_run = FALSE
raw_ipums_in_memory = TRUE

for(level in c("high_skill", "low_skill")){
   ipums_sample <- load_ipums_sample(level, fresh_run, raw_ipums_in_memory)

   msa_main_output <- get_msa_net_migration_df(level, ipums_sample)
   
   msa_main_output %>%
     write_csv(file.path(out_path, glue::glue("{level}_migration_by_msa.csv")))

   migpuma_main_output <-get_migpuma_net_migration_df(level, ipums_sample)
   
   migpuma_main_output %>%
     write_csv(file.path(out_path, glue::glue("{level}_migration_by_migpuma.csv")))
}