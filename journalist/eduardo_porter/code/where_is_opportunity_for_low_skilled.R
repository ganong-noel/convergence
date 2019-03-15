# Ari Version -- Depricated

# Where should and where do low-skill (high-skill) workers move today?
# convergence issue #3 and #6

# This code relies on data produced where_is_opportunity_for_low_skills_core_results.R and
# is NOT MEANT FOR POSTERITY.

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

load_puma_to_migpuma <- function(src_path. = src_path){
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
             migpuma1 = as.numeric(res_puma)) %>%
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
migpuma_real_wages_ <-
  ipums_sample %>% 
  select(incwage, real_wage, statefip, puma, skill, hhwt) %>%
  left_join(puma_to_migpuma_2010, by = c("statefip", "puma")) %>% 
  rename(migpuma1 = res_puma, migplac1 = res_state) %>%
  group_by(migpuma1, migplac1, skill) %>%
  summarize(
    state = first(res_stname), 
    mean_nominal_wage = weighted.mean(incwage, w=hhwt, na.rm=TRUE),
    mean_real_wage = weighted.mean(real_wage, w=hhwt, na.rm=TRUE)
  )


  migpuma_in_migration_ <-
      ipums_sample %>%
        filter(migpuma1 > 2) %>% 
        # filter(migplac1 > 0) %>% 
        group_by(statefip, puma, skill) %>%
        summarize(n_in = sum(hhwt, na.rm=TRUE)) %>%
        left_join(puma_to_migpuma_2010, by = c("puma", "statefip")) %>%
      rename(migpuma1 = res_puma, migplac1 = res_state) %>%
        group_by(migpuma1, migplac1, skill) %>%
        summarise(n_in = sum(n_in))
    
    migpuma_out_migration_ <-
      ipums_sample %>%
        filter(migpuma1 > 2) %>% 
        # filter(migplac1 > 0) %>% 
        group_by(migpuma1, migplac1, skill) %>%
        summarize(n_out = sum(hhwt, na.rm=TRUE),
                  n_out_not_foreign = sum(ifelse(migplac1 < 100, hhwt, 0))) %>%
        ungroup() %>%
        transmute(migpuma1, migplac1, skill,  n_out, n_out_not_foreign)
    
    place_pop_ <-
      place_pop %>%
      rename(migpuma1 = res_puma, migplac1 = res_state)
    
    place_wages_nominal_all_ <- 
      # ipums_sample %>% 
      # filter(migpuma1 <= 2) %>% 
      # group_by(res_state, res_puma) %>%
      # summarise(nominal_wage_everyone = weighted.mean(hh_wage_inc, hhwt)) %>%
      place_wages_nominal_all %>%
      rename(migpuma1 = res_puma, migplac1 = res_state)
    

    
    migpuma_main_output_ <-
      migpuma_real_wages_ %>%
      left_join(top_line_place_2016, by=c("migpuma1", "migplac1" = "statefip")) %>%
      full_join(migpuma_out_migration_, by=c("migpuma1", "migplac1", "skill" = "skill")) %>%
      full_join(migpuma_in_migration_, by=c("migpuma1", "migplac1",  "skill" = "skill")) %>% 
      full_join(place_pop_, by=c("migpuma1", "migplac1")) %>% 
      full_join(place_wages_nominal_all_, by=c("migpuma1", "migplac1")) %>% 
        mutate(n_out = ifelse(is.na(n_out), 0, n_out),
             n_in = ifelse(is.na(n_in), 0, n_in),
             `raw net migration` = n_in - n_out, 
             net_mig = 1000*`raw net migration`/pop,
             net_mig_full = 1000*`raw net migration`/migpuma_population) %>%
      select(skill, state, migpuma_name, net_mig, `raw net migration`, everything())
    
  
############
# RUN CODE #
############
puma_to_migpuma_2010 <- load_puma_to_migpuma()
top_line_place_2016 <- load_top_line_place(puma_to_migpuma_2010)

    
main_filter_on_plac1 <- main_filter_on_plac1  %>% 
      full_join(place_wages_nominal_all_, by=c("migpuma1", "migplac1"))

skill_filter = c("low_skill", "high_skill", "low_skill", "high_skill")
wage_type = c(rep("nominal_wage_everyone", 2), "mean_real_wage", "mean_real_wage")
mig_type = c(rep("net_mig", 4))
x_label = c(rep("Nominal income ($000s)", 2),
            "Income - housing cost for low skill ($000s)",
            "Income - housing cost for high skill ($000s)")

convergence_plots <- pmap(list(wage_type=wage_type, mig_type=mig_type, 
                               x_label=x_label, skill_filter=skill_filter),
                              make_plot, 
                              data=migpuma_main_output_,
                              weights="pop")

(grid_of_plots <- 
    grid.arrange(
      convergence_plots[[1]], 
      convergence_plots[[2]],
      convergence_plots[[3]],
      convergence_plots[[4]])
)


ggsave("temp_for_peter.png", grid_of_plots, width=7, height = 5)
