library(tidyverse)
library(tidybayes)
library(isdbayes)
library(poweRlaw)
library(readxl)
library(here)
here::i_am("code/01_wrangle-data.R")

inverts_full = read_excel(
  path = here("data/20160906_Analysis_SRJS_Macroinvertebrates.xlsx"),
  sheet = "Site Data",
  range = cell_cols("A:AW")
  )

inverts_int = inverts_full %>% 
  select(site, event, month, year, sort, taxon_lifestage,
         code, count, matches("l\\d{1}"),matches("h\\d{1}"), length_a, length_b, head_a, head_b) %>% 
  rowwise %>% 
  mutate(across(matches("l\\d{1,2}"), ~length_a*(.x^length_b), .names = "{.col}_dw")) %>% 
  mutate(across(matches("h\\d{1,2}"), ~head_a*(.x^head_b), .names = "{.col}_dw")) %>% 
  mutate(meas = sum(!is.na(across(l1_dw:h10_dw)), na.rm = TRUE)) %>% 
  mutate(flag = case_when(sum(across(l1_dw:h10_dw, ~!is.na(.x))) == count ~ "0",
                          sum(across(l1_dw:h10_dw, ~!is.na(.x))) == (2*count) ~ "dbl",
                          sum(across(l1_dw:h10_dw, ~!is.na(.x))) > count ~ "up",
                          sum(across(l1_dw:h10_dw, ~!is.na(.x))) < count ~ "dn",
                          .default = "1")) %>% 
  select(site, event, month, year, sort, code, count, meas, flag, everything())
        