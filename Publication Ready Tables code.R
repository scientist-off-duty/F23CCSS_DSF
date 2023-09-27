# ANS FA23 CCSS DSF
# Publication Ready Tables in R

# Syntax Style and  Organization ----
# show MIDUS code as style example

# Data Prep ----

# set working directory 
setwd("/Users/aishatsadiq/Desktop/descriptive_wellbeing_replication")

# set seed
set.seed(31415)

# load libraries
#install.packages("gtsummary")
library(gtsummary)
library(tidyverse)
library(kableExtra)
library(rmarkdown)

## load data
# data("EquationCitations", package = "AER")

# load dfs
# Finder -> Folder -> Double/Right click -> Copy "Folder" as Pathname -> add file name to the end to import file 
EquationCitations <- read.csv("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/CCSS Data Fellow/teaching datasets//EquationCitations.csv", header=TRUE)
#Q: EquationCitations vs View(EquationCitations) to visualize the dataframe

# learn and select variables based on question of interest
colnames(EquationCitations)
# read data doc (https://vincentarelbundock.github.io/Rdatasets/doc/AER/EquationCitations.html)

# Summary stats, p-values, and descriptive statistics with tbl_summary() ----
SelfvOther <- EquationCitations %>% 
  select(journal, pages, mainequations, selfcites, othercites, cites)

tbl_summary(SelfvOther)

# get descriptive stats for defined treatment groups ----
tbl_summary(SelfvOther, by = journal)


# Summarize regression results with tbl_regression & tbl_uvregression ----
SelfvOther %>%
  tbl_summary( by = journal) %>%
  add_p()
# Review: add_p() %>% add_difference() %>% add_q() %>% add_overall() %>% add_n() %>% add_ci() %>% add_stat_label()


# Paired.t.test for continuous & paired Mcnemar.test for categorical variables ----
          # Need an “id” column & complete pairs in df
SelfvOther_paired <-
  SelfvOther %>%
  select(selfcites,othercites,cites, journal) %>%
  group_by(journal) %>%
  mutate(id = row_number()) %>%
  ungroup()

SelfvOther_paired %>%
  filter(complete.cases(.)) %>%
  group_by(id) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  tbl_summary(by = journal, include = -id) %>%
  add_p(test = list(
    marker   ~ "paired.t.test",
    response ~ "mcnemar.test"), 
    group    = id)

# Summarize regression model ----
EqReg <- glm(cites  ~ mainequations + journal + pages, data = SelfvOther) %>%
  tbl_regression(conf.level= .95, tidy_fun = broom.helpers::tidy_parameters)
EqReg

# removed: othercites + selfcites for clarity

#Several univariate regression at once
SelfvOther_uvlm_table <- SelfvOther %>%
  tbl_uvregression(
    method       = glm,
    y            = cites,
    method.args  = list(family = gaussian),
    exponentiate = TRUE
  ) 
SelfvOther_uvlm_table

# Edit code ----
#Combining different tables with each other
uvcm_table <- tbl_uvregression(
  trial %>% 
    select(ttdeath, death, trt, age, grade), # 300 predictors
  method       = coxph,
  y            = Surv(ttdeath, death),
  exponentiate = TRUE
  ) 

uvcm_table

# stacking two tbl_regression objects w/ tbl_stack()
t1 <-
    glm(response ~ trt, trial, family = binomial) %>%
    tbl_regression(
        exponentiate = TRUE,
        label = list(trt ~ "Treatment (unadjusted)")
    )

t2 <-
    glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
    tbl_regression(
        include = "trt",
        exponentiate = TRUE,
        label = list(trt ~ "Treatment (adjusted)")
    )

tbl_stack_ex1 <- tbl_stack(list(t1, t2))

# Regression model where the predictor remains the same, and the outcome changes
trial %>%
  select(age, marker, ttdeath, trt) %>%
  tbl_uvregression(
    method = lm,
    x = trt,
    show_single_row = "trt",
    hide_n = TRUE
  ) %>%
  modify_header(list(
    label ~"**Model Outcome**",
    estimate ~ "**Treatment Coef.**"
  )) %>%
  modify_footnote(estimate ~ "Values larger than 0 indicate larger values in the Drug B group.")

# Side-by-side Regression Models with tbl_merge()
# Different models with same predictors
fancy_table <-
  tbl_merge(
    tbls        = list(bm_table, cm_table),
    tab_spanner = c("Tumor Response", "Time to Death")
  )

fancy_table

# Uni- Multi-variate models with same predictors + Descriptive stats
uni_multi <- tbl_merge(
    tbls        = list(tbl_summary(d), uvlm_table, bm_table),
    tab_spanner = c("**Describe**", "**Univariate Models**", "**Multivariate Model**")
  )

uni_multi

# tbl_stack()
# stacking two tbl_regression objects
t1 <-
    glm(response ~ trt, trial, family = binomial) %>%
    tbl_regression(
        exponentiate = TRUE,
        label = list(trt ~ "Treatment (unadjusted)")
    )

t2 <-
    glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
    tbl_regression(
        include = "trt",
        exponentiate = TRUE,
        label = list(trt ~ "Treatment (adjusted)")
    )

tbl_stack_ex1 <- tbl_stack(list(t1, t2))

# Modifying aesthetics (ADD BACK AFTER CORRECTING) ----
glm(response ~ trt + age + ttdeath + grade, trial, family = binomial) %>% 
  tbl_regression(
    #pvalue_fun   = ~style_pvalue(.x, digits = 3),
    exponentiate = TRUE
  ) %>% 
  
  # add_* helpers
  add_n(location = "level") %>%
  add_nevent(location = "level") %>%  
  add_global_p() %>%   
  add_q() %>%        
  add_significance_stars(hide_p = F, hide_se = T, hide_ci = F) %>% 
  add_vif() %>% 
  
  # modify_* helpers
  modify_header(label = "**Predictor**") %>% 
  modify_caption("**Table 1. Really cool looking table!**") %>% 
  modify_footnote(
    ci = "CI = Credible Intervals are incredible ;)", abbreviation = TRUE) %>%
  sort_p() %>% 
  
  # aesthetics helpers
  bold_p(t = 0.10, q = TRUE) %>% 
  bold_labels() %>% 
  #bold_levels() %>% 
  #italicize_labels() %>% 
  italicize_levels()

# with add {gt} arguments
library(gt)
trial %>%
  # create a gtsummary table
  tbl_summary(by = trt) %>%
  # convert from gtsummary object to gt object
  as_gt() %>%
  # modify with gt functions
  tab_header("Table 1: Baseline Characteristics") %>% 
  tab_spanner(
    label = "Randomization Group",  
    columns = starts_with("stat_")
  ) %>% 
  tab_options(
    table.font.size = "small",
    data_row.padding = px(1)) 

# themes()

# JUNKYARD ----

# get descriptive stats for subgroup within a treatment group
SelfvOther %>%
  tbl_strata(
    strata = selfcites,
    ~.x) %>%
tbl_summary( by = journal)

# Question: How to find variable types? Sol: DF docs, class(), typeof(), is.numeric()/is.factor()
# Numeric (1.2, 5, 7, 3.14159)
# Integer (1, 2, 3, 4, 5)
# Complex (i + 4)
# Logical (TRUE / FALSE)
# Character ("a", "apple")