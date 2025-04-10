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
  mutate(across(matches("l\\d{1}"), ~length_a*(.x^length_b), .names = )) %>% 
  mutate(across(matches("h\\d{1}"), ~head_a*(.x^head_b)))
  
  group_by(site, event, month, year, sort, taxon_lifestage, code, count, length_a, length_b, head_a, head_b) %>% 
  pivot_longer()