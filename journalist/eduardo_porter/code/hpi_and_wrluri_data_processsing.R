library(tidyverse)
library(haven)
library(rprojroot)
library(yaml)
library(readxl)
setwd("~/gnlab/convergence/")
make_path <- is_git_root$make_fix_file()
working_path <- make_path("journalist/eduardo_porter/")
out_path <- make_path("journalist/eduardo_porter/out")


county_name <- read_dta("~/Downloads/saiz_county_crosswalk.dta") %>% 
  filter(fips %in%  c(6085, 53033)) %>% 
  transmute(fips, county, msa99 = msanecma, peters_WRLURI = WRLURI)


# Clean and check Warton Land Regulation Data
wharton <- 
  read_dta(file.path(working_path, "src/WHARTON LAND REGULATION DATA_1_24_2008.dta")) %>% 
    filter(msa99 %in% c(7600,7400), !ufips %in% c(2585, 22640, 37900, 65170, 79625)) %>% 
    left_join(county_name, by=c("msa99"))

wharton %>% 
group_by(county, peters_WRLURI) %>%
    summarize(WRLURI = mean(WRLURI, na.rm = TRUE)) %>%
    knitr::kable(type = "markdown", digits = 2)


wharton %>% select(-peters_WRLURI) %>% write_csv(file.path(out_path, "WRLURI_by_municiplaties_within_counties.csv"))
  


# Clean and check FHFA HPI data
hpi <- read_excel("~/Downloads/HPI_AT_BDL_county.xlsx", skip = 6) %>% 
        filter(`FIPS code` %in% c('06085', '53033')) %>%
        mutate_at(vars(-State, -County), as.numeric) 

hpi %>% 
  ggplot(aes(x=Year, y=HPI, color = County)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() +
  labs(y = "Housing Price Index (HPI = 100 in 1975)")

ggsave(file.path(out_path, "hpi_by_county.png"), width = 6, height = 4)
hpi %>% write_csv(file.path(out_path, "FHFA_kings_santa_clara.csv"))
