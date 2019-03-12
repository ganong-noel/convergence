rm(list = ls())
library(tidyverse)
library(haven)
library(readxl)
library(labelled)
library(rprojroot)

if (Sys.getenv()[["USER"]] == "peterganong") {
  dropbox_path <- "~/repo/data_ipums/convergence/"
  setwd("~/repo/convergence/")
} else {
  dropbox_path <- "~/gnlab/data_ipums/convergence/" #"/scratch/midway2/anisfeld/"
  setwd("~/gnlab/convergence/")
}

make_path <- is_git_root$make_fix_file()
src_path <- make_path("journalist/eduardo_porter/src")
out_path <- make_path("journalist/eduardo_porter/out")
working_path <- make_path("journalist/eduardo_porter/")
source(file.path(working_path, "/code/binscatter.R"))

# helper data ----

load_puma_to_migpuma <- function(puma_population_from_scratch, 
                                 src_path. = src_path){
  
  state_fip_to_name <- read_csv(file.path(src_path., "state_fip_to_name.csv"))
  
  puma_to_migpuma_2010 <- 
    read_xls(
      file.path(src_path., "puma_migpuma1_pwpuma00.xls"), 
      skip = 3, 
      col_names = c("statefip", "puma", "migplac1", "migpuma1")
    ) %>%
    mutate_all(as.numeric) %>% #throws a warning because of table note
    filter(!is.na(statefip) & statefip != 72) %>% #drop table note and Puerto Rico
    left_join(state_fip_to_name, by = c("statefip")) %>%
    rename(res_state = migplac1, res_puma = migpuma1, res_stname = stname)
}
puma_to_migpuma_2010 <- load_puma_to_migpuma()

# load main data ----
raw_ipums <- read_dta(file.path(dropbox_path, "full_ipums_extract.dta")) %>% 
  select(-c(empstatd, related, educ, perwt)) 
  #41 seconds
test_that("n row raw", expect_equal(nrow(raw_ipums), 15681927))

ipums_sample <-
  raw_ipums %>%
  mutate(incwage = ifelse(incwage >= 999998, NA, incwage)) %>%
  group_by(serial) %>%
  mutate(hh_wage_inc = sum(incwage, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(age >= 25, age <= 65, relate == 1, empstat %in% c(1,2)) %>%
  mutate(annual_housing = 12*ifelse(owncost == 99999, rentgrs, owncost),
         real_wage = hh_wage_inc - annual_housing,
         skill = ifelse(educd < 101, "low_skill", "high_skill"))  %>% 
  select(-age, -educd, -relate, -empstat) %>%
  left_join(puma_to_migpuma_2010, by = c("statefip", "puma")) #30 seconds
test_that(
  "serial is unique id", 
  expect_equal(n_distinct(ipums_sample$serial), nrow(ipums_sample))
)

#collapse to puma-level stats ----
#it seems possible that some function could generate these five data frames
#but the function would have to be sufficiently flexible that I doubt it would
#improve clarity

place_pop <- 
  ipums_sample %>% 
  group_by(res_state, res_puma) %>%
  summarise(pop = sum(hhwt))

place_wages_nominal <- 
  ipums_sample %>% 
  filter(migpuma1 <= 2) %>% 
  group_by(res_state, res_puma) %>%
  summarise(nominal_wage_everyone = weighted.mean(hh_wage_inc, hhwt))

place_wages_real <- 
  ipums_sample %>% 
  filter(migpuma1 <= 2) %>% 
  group_by(res_state, res_puma, skill) %>%
  summarise(real_wage = weighted.mean(real_wage, hhwt)) %>% 
  spread(key = "skill", value = "real_wage") %>%
  rename(real_wage_high_skill = high_skill, real_wage_low_skill = low_skill)

place_in_migration <- 
  ipums_sample %>% 
  filter(migpuma1 > 2) %>% 
  group_by(res_state, res_puma, skill) %>%
  summarise(n_move_in  = sum(hhwt)) %>% 
  spread(key = "skill", value = "n_move_in") %>%
  rename(n_move_in_high_skill = high_skill, n_move_in_low_skill = low_skill)

place_out_migration <- 
  ipums_sample %>% 
  filter(migpuma1 > 2) %>% 
  group_by(migplac1, migpuma1, skill) %>%
  summarise(n_move_out  = sum(hhwt)) %>% 
  spread(key = "skill", value = "n_move_out") %>%
  rename(n_move_out_high_skill = high_skill, n_move_out_low_skill = low_skill)

test_that("check n rows same", {
  expect_equal(nrow(place_pop), nrow(place_wages_nominal))
  expect_equal(nrow(place_pop), nrow(place_wages_real))
  expect_equal(nrow(place_pop), nrow(place_in_migration))
  expect_equal(nrow(place_pop), nrow(place_out_migration))
})

test_that("migration in = migration out", {
  expect_equal(
    sum(place_in_migration$n_move_in_high_skill),
    sum(place_out_migration$n_move_out_high_skill)
  )
  expect_equal(
    sum(place_in_migration$n_move_in_low_skill),
    sum(place_out_migration$n_move_out_low_skill)
  )
})

net_mig_wage_by_puma <- 
  place_pop %>%
  left_join(place_wages_real, by = c("res_state", "res_puma")) %>%
  left_join(place_wages_nominal, by = c("res_state", "res_puma")) %>%
  left_join(place_in_migration, by = c("res_state", "res_puma")) %>%
  left_join(place_out_migration, by = c("res_state" = "migplac1", "res_puma" = "migpuma1")) %>%
  mutate(
    net_mig_low_skill = (n_move_in_low_skill - n_move_out_low_skill) / pop,
    net_mig_high_skill = (n_move_in_high_skill - n_move_out_high_skill) / pop
  )

regs_figure5 <- 
  c(
    "net_mig_low_skill ~ log(nominal_wage_everyone)",
    "net_mig_high_skill ~ log(nominal_wage_everyone)",
    "net_mig_low_skill ~ log(real_wage_low_skill)",
    "net_mig_high_skill ~ log(real_wage_high_skill)"
  ) %>%
  map(~ lm(., data = net_mig_wage_by_puma, weights = pop)) %>%
  map(summary)

test_that("signs on coefficients from Ganong-Shoag Figure 5 hold in 2012-2017", {
  expect_equal(regs_figure5[[1]]$coefficients[2,1], -0.00216, tolerance = 1e-3)
  expect_equal(regs_figure5[[2]]$coefficients[2,1], 0.003127, tolerance = 1e-3)
  expect_equal(regs_figure5[[3]]$coefficients[2,1], 0.00562, tolerance = 1e-3)
  expect_equal(regs_figure5[[4]]$coefficients[2,1], 0.00390, tolerance = 1e-3)
})

#Ari, please repeat for all four panels and place them in a grid, as in Figure 5
binscatter_output <- 
  net_mig_wage_by_puma %>%
  binscatter(x = "nominal_wage_everyone", y = "net_mig_low_skill", weights = "pop")
ggplot(binscatter_output$df_bin) + 
  geom_point(aes(x = x, y = y)) + 
  fte_theme() + 
  scale_x_log10(breaks = seq(40000, 100000, 20000),
                labels = seq(40000, 100000, 20000))

