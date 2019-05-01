rm(list = ls())
library(tidyverse)
library(haven)
library(readxl)
library(labelled)
library(yaml)
library(broom)
library(gridExtra)
library(rprojroot)

if (Sys.getenv()[["USER"]] == "peterganong") {
  dropbox_path <- "~/repo/data_ipums/convergence/"
  setwd("~/repo/convergence/")
} else {
  dropbox_path <- "~/gnlab/data_ipums/convergence/"
  setwd("~/gnlab/convergence/")
}

make_path <- is_git_root$make_fix_file()
src_path <- make_path("journalist/eduardo_porter/src")
out_path <- make_path("journalist/eduardo_porter/out")
working_path <- make_path("journalist/eduardo_porter/")
source(file.path(working_path, "/code/binscatter.R"))
CONFIG <- yaml.load_file(file.path(working_path, "/code/config.yml"))
source(str_c(CONFIG$lab_code, "prelim.R"))


# helper data ----

load_puma_to_migpuma <- function(src_path. = src_path){
  
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
raw_ipums <- read_dta(file.path(dropbox_path, "ipums_2017_no_gq.dta")) %>% 
  select(-c(related, educ, perwt, datanum, year, multyear, gq, cbserial)) 
  #41 seconds                                                                               
test_that("n row raw 2017", expect_equal(nrow(raw_ipums),object =  15014706))
                                              
ipums_sample <-
  raw_ipums %>%                           
  mutate(incwage = ifelse(incwage >= 999998, NA, incwage)) %>%
  group_by(serial) %>%
  mutate(hh_wage_inc = sum(incwage, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(age >= 25, age <= 65, relate == 1, empstat %in% c(1,2)) %>%
  mutate(annual_housing = ifelse(valueh > 999998, 12*rentgrs, valueh*.05),
         real_wage = hh_wage_inc - annual_housing,
         skill = ifelse(educd < 101, "low_skill", "high_skill"))  %>% 
  select(-age, -educd, -relate, -empstat) %>%
  left_join(puma_to_migpuma_2010, by = c("statefip", "puma")) #30 seconds
test_that(
  "serial is unique id", 
  expect_equal(n_distinct(ipums_sample$serial), nrow(ipums_sample))
)

#write_rds(ipums_sample, file.path(src_path, "ipums_sample.rds"))
# ipums_sample <- read_rds(file.path(src_path, "ipums_sample.rds"))

#collapse to puma-level stats ----
#it seems possible that some function could generate these five data frames
#but the function would have to be sufficiently flexible that I doubt it would
#improve clarity

place_pop <- 
  ipums_sample %>% 
  group_by(res_state, res_puma) %>%
  # below we add back out-migrators for the analysis
  summarise(pop = sum(hhwt))


place_wages_nominal_all <- 
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

place_wages_nominal <- 
  ipums_sample %>% 
  filter(migpuma1 <= 2) %>% 
  group_by(res_state, res_puma, skill) %>%
  summarise(nominal_wage = weighted.mean(hh_wage_inc, hhwt)) %>% 
  spread(key = "skill", value = "nominal_wage") %>%
  rename(nominal_wage_high_skill = high_skill, nominal_wage_low_skill = low_skill)

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
  expect_equal(nrow(place_pop), nrow(place_wages_nominal_all))
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
  left_join(place_wages_nominal_all, by = c("res_state", "res_puma")) %>%
  left_join(place_wages_nominal, by = c("res_state", "res_puma")) %>%
  left_join(place_in_migration, by = c("res_state", "res_puma")) %>%
  left_join(place_out_migration, by = c("res_state" = "migplac1", "res_puma" = "migpuma1")) %>%
  mutate(
    pop = pop + n_move_out_low_skill + n_move_out_high_skill,
    net_mig_low_skill = 1000 * (n_move_in_low_skill - n_move_out_low_skill) / pop,
    net_mig_high_skill = 1000 * (n_move_in_high_skill - n_move_out_high_skill) / pop
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
  expect_equal(regs_figure5[[1]]$coefficients[2,1], -2.68, tolerance = 1e-3)
  expect_equal(regs_figure5[[2]]$coefficients[2,1], 1.84, tolerance = 1e-3)
  expect_equal(regs_figure5[[3]]$coefficients[2,1], 5.25, tolerance = 1e-3)
  expect_equal(regs_figure5[[4]]$coefficients[2,1], 1.87, tolerance = 1e-3)
})


####

get_model_as_title <- function(tidy_model, group="", round_to=2) {
  
  stopifnot( nrow(tidy_model) == 2)
  tidy_model <- tidy_model %>% 
    filter(term != "(Intercept)") %>%
    transmute(Coef = estimate,
              SE = std.error) %>%
    round(round_to)
  
  
  title = group
  for(col in names(tidy_model)){
    title = glue::glue("{title} {tolower(col)}: {tidy_model[, col]}")
  }
  title
}

get_plot_data <- 
  function(
    data = net_mig_wage_by_puma, 
    mig_type = "net_mig_low_skill",
    wage_type = "nominal_wage_everyone", 
    .weights = "pop",
    skill_filter = NULL,
    ...) {
      
        if (.weights == 1){
          data$weights = 1
          .weights = "weights"
        }
        
        if (is.null(skill_filter)){
          skill_type = case_when(str_detect(mig_type, "low") ~ "Low skill",
                                 str_detect(mig_type, "high") ~ "High skill",
                                 TRUE ~ "Error")
          
        } else {
          data <- data %>% filter(skill==skill_filter)
          skill_type = ifelse(skill_filter == 1,
                              "High skill",
                              "Low skill")
        
        }
          
        
        binscatter_output <-
          data %>%
          binscatter(x = wage_type, 
                     y = mig_type,
                     weights = .weights
          ) %>%
          .$df_bin %>%
          transmute(!!sym(wage_type) := x,
                 !!sym(mig_type) := y,
                 type = skill_type)
        
        model <- lm(as.formula(str_c(mig_type, "~ log(", wage_type,")")), 
                    data = data, 
                    weights = data %>% pull(!!sym(.weights)))
        tidy_model <- tidy(model) 
    
        return(list(binscatter_data = binscatter_output, 
                    tidy_model = tidy_model, 
                    skill_type = skill_type))

}          


make_plot <- 
  function(
    data = net_mig_wage_by_puma, 
    mig_type = "net_mig_low_skill",
    wage_type = "nominal_wage_everyone", 
    .weights = "pop",
    x_label = "",
    x_in_logs = FALSE,
    skill_filter = NULL,
    ylims = 5
    ) {
    
    if (.weights == 1){
      data$weights = 1
      .weights = "weights"
    }
      

    plot_data <- get_plot_data(data, 
                               mig_type, 
                               wage_type, 
                               .weights,
                               skill_filter = skill_filter, 
                             )
    
    binscatter_data <- plot_data[[1]]
    tidy_model <- plot_data[[2]]
    skill_type <- plot_data[[3]]
    
    base <-
      data %>%
        ggplot(
          aes(x=!!sym(wage_type), y=!!sym(mig_type))
        ) +
        geom_smooth(method = "lm",
                    mapping = aes(weight = !!sym(.weights)),
                    se = FALSE) +
        geom_point(data = binscatter_data) +
        coord_cartesian(ylim=c(-ylims,ylims))+
        fte_theme() +
        labs(x = x_label, 
             y = glue::glue("{skill_type} net migration"), 
             title=get_model_as_title(tidy_model, group= skill_type)) +
        theme(plot.title = element_text(size=12),
              text = element_text(size=11),
              axis.text = element_text(size=10))
      
      if (x_in_logs){
        base + 
        scale_x_continuous(trans = scales::log_trans(),
                           breaks = scales::trans_breaks("log", function(x) exp(x)),
                           labels = scales::trans_format("log", scales::math_format(.x)))
      } else {
      base + 
      scale_x_log10(breaks = seq(40000, 120000, 20000),
                    labels = seq(40, 120, 20) %>%
                      scales::dollar())
      }
}


# Eduardo plot
wage_type = c(rep("nominal_wage_everyone", 2), "real_wage_low_skill", "real_wage_high_skill")
mig_type = c("net_mig_low_skill", "net_mig_high_skill", "net_mig_low_skill","net_mig_high_skill")
x_label = c(rep("Nominal income ($000s)", 2),
            "Income - housing cost for low skill ($000s)",
            "Income - housing cost for high skill ($000s)")

convergence_plots <- pmap(list(wage_type=wage_type, mig_type=mig_type, x_label=x_label), make_plot)

(grid_of_plots <- 
    grid.arrange(
     convergence_plots[[1]], 
     convergence_plots[[2]],
     convergence_plots[[3]],
     convergence_plots[[4]])
)

ggsave(file.path(out_path, "eduardo_plot.png"), grid_of_plots, width = 7, height = 5)

# Replication plot

x_label = c(rep("Log nominal income", 2),
            "Log (income - housing cost) for low skill",
            "Log (income - housing cost) for high skill")

convergence_plots <- pmap(list(wage_type=wage_type, mig_type=mig_type, x_label=x_label), 
                          make_plot,
                          x_in_logs=TRUE)

(grid_of_plots <- 
    grid.arrange(
      convergence_plots[[1]], 
      convergence_plots[[2]],
      convergence_plots[[3]],
      convergence_plots[[4]])
)

ggsave(file.path(out_path, "replication_plot.png"), grid_of_plots, width = 7, height = 5)


### ISSUE 23 getting binscatter data as csv


ep_data <- read_excel(file.path(src_path, "Peter.s.2016.data.xlsx"))

ep_data <-
  ep_data %>%
  mutate(net_mig_low_skill = net_mig_low_skill*1000,
         net_mig_high_skill = net_mig_high_skill*1000)


# set .weights = 1 for unweighted results
low_2016 <- get_plot_data(data = ep_data,
                   wage_type = "nominal_wage_everyone",
                   mig_type = "net_mig_low_skill",
                   .weights = "n_working_households")

high_2016 <- get_plot_data(data = ep_data,
                           wage_type = "nominal_wage_everyone",
                          mig_type = "net_mig_high_skill",
                          .weights = "n_working_households")

# 1940 data
borjas <- read_dta(file.path(src_path, "BORJAS1940FINAL.dta")) 


borjas <-
  borjas %>%
    mutate(netMig = netMig*1000)

low_1940 <- get_plot_data(data=borjas,
                          wage_type = "incShared",
                          mig_type = "netMig",
                          .weights = "basePop",
                          skill_filter = 0)

high_1940 <- get_plot_data(data=borjas,
                          wage_type = "incShared",
                          mig_type = "netMig",
                          .weights = "basePop",
                          skill_filter = 1)



binscatter_data_2016 <-
  bind_rows(
    low_2016$binscatter_data %>%
      rename("net_mig" = net_mig_low_skill),  
    high_2016$binscatter_data  %>%
      rename("net_mig" = net_mig_high_skill)
  ) %>% 
  ungroup() %>%
  mutate(year = 2016) %>%
  select(-cut)

binscatter_data_1940 <-
  bind_rows(
    low_1940$binscatter_data,  
    high_1940$binscatter_data  
  ) %>% 
  ungroup() %>%
  rename("net_mig" = netMig,
         "nominal_wage_everyone" = incShared
  ) %>%
  mutate(year = 1940) %>%
  select(-cut)

binscatter_data <-
  bind_rows(
    binscatter_data_1940,
    binscatter_data_2016
  )

write_csv(binscatter_data, file.path(out_path, "binscatter_data_1940_2016.csv"))


models <-
  bind_rows(
    low_2016$tidy_model %>%
      mutate(type = low_2016$skill_type,
             year = 2016),
    high_2016$tidy_model %>%
      mutate(type = high_2016$skill_type,
             year = 2016),
    low_1940$tidy_model %>%
      mutate(type = low_1940$skill_type,
             year = 1940),
    high_1940$tidy_model %>%
      mutate(type = high_1940$skill_type,
             year = 1940),
  ) %>%
  select(type, year, term, estimate, std.error)

write_csv(models, file.path(out_path, "pop_weighted_model_outputs_1940_2016.csv"))


read_csv(file.path(out_path, "pop_weighted_model_outputs_1940_2016.csv"))

# binscatter_data %>%
#   filter(year == 2016) %>%
#   ggplot(aes(x=nominal_wage_everyone, y = net_mig)) + 
#   geom_point() + 
#   facet_grid(~type, scales = "free") +
#   fte_theme() +
#   geom_smooth(model = "lm", 
#                mapping = aes(net_mig~nominal_wage_everyone),
#                se = FALSE)  +
#   scale_x_log10(breaks = seq(40000, 120000, 20000),
#                 labels = seq(40, 120, 20) %>%
#                   scales::dollar()) +
#   labs(x = "2016 nominal income  ($000)", y = "Net migration by skill level")

### ISSUE 11: Give names to data
load_place_names <- function(puma_to_migpuma_2010,
                                src_path. = src_path){
  
  puma_names <- read_csv(file.path(src_path.,"2010_PUMA_Names.txt"), 
                         skip=1, col_names = c("statefip", "puma", "name")) %>%
    mutate_at(c("statefip", "puma"), as.numeric)
  
  puma_pop_2016 <-
    read_csv(file.path(src_path., "puma_population.csv")) %>%
    # this drops Puerto Rico and other islands
    left_join(puma_names, by=c("statefip", "puma"))
  
  migpuma_names <-
    puma_pop_2016 %>%
    left_join(puma_to_migpuma_2010, by=c("puma", "statefip")) %>%
    mutate(name = str_replace_all(name, "( \\(.*|--.*| PUMA)", "")) %>%
    group_by(res_stname, res_state, res_puma, name) %>%
    summarize(population_by_town = sum(population))  %>%
    # this step helps capture name of most populous puma
    arrange(desc(population_by_town)) %>%
    group_by(res_stname, res_state, res_puma) %>%
    summarize(place_name = first(name)) %>%
    ungroup()
}

place_names <- load_place_names(puma_to_migpuma_2010)

place_names %>%
  left_join(net_mig_wage_by_puma, by = c("res_state", "res_puma")) %>%
  arrange(desc(pop)) %>% View
  write_csv(file.path(out_path, "puma_2017_mig_wage_data.csv"))




