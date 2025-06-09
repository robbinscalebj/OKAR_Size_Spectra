here::i_am("code/01_wrangle-data.R")
source(here::here('code/helpers.R'))
### --- Purpose ----
# This script wrangles the invertebrate data and adds some missing information,
# removes adults and pupae, and cleans the masses by
# 1) resampling massses when count > measurements
# 2) averaging masses when count = 0.5 * measurements (e.g., both length and hw)
# 3) readjusting counts when measurement > counts (but not double)
#

### --- Output ---
# inverts_int = intermediate inverts object with some cleaning and QAQC flags. Use for exploratory analysis in 02_mass-distr.R
# inverts_clean = a cleaned data frame with necessary sample metadata, taxa, mass, and counts
# inverts_clean_dw = a cleaned data frame with necessary sample metadata, mass and counts--no taxa information! This is for size spectra


### Code ----
inverts_full = read_excel(
  path = here("data/20160906_Analysis_SRJS_Macroinvertebrates.xlsx"),
  sheet = "Site Data",
  range = cell_cols("A:AW")
  )

# inverts_int for 02_mass-distr.R
inverts_int = inverts_full %>% 
  select(site, event, month, year, sort, taxon_lifestage,
         code, count, matches("l\\d{1}"),matches("h\\d{1}"), length_a, length_b, head_a, head_b, `ct/m2`) %>% 
  mutate(length_a = case_when(code == 'Chironom' ~ 0.0018,
                              .default = length_a),
         length_b = case_when(code == 'Chironom' ~ 2.617,
                              .default = length_b)) %>% 
  mutate(across(matches("l\\d{1,2}"), ~length_a*(.x^length_b), .names = "{.col}_dw")) %>% 
  mutate(across(matches("h\\d{1,2}"), ~head_a*(.x^head_b), .names = "{.col}_dw")) %>% 
  rowwise %>% 
  mutate(meas = sum(!is.na(across(l1_dw:h10_dw)), na.rm = TRUE)) %>% 
  mutate(flag = case_when(sum(across(l1_dw:h10_dw, ~!is.na(.x))) == count ~ "0",
                          sum(across(l1_dw:h10_dw, ~!is.na(.x))) == (2*count) &
                            sum(across(l1_dw:l10_dw, ~!is.na(.x))) != 0 &
                            sum(across(h1_dw:h10_dw, ~!is.na(.x))) != 0  ~ "dbl",
                          sum(across(l1_dw:h10_dw, ~!is.na(.x))) > count ~ "up",
                          sum(across(l1_dw:h10_dw, ~!is.na(.x))) < count ~ "dn",
                          .default = "1")) %>%
  ungroup %>% 
  # remove all the pupae and adults for now
  filter(!grepl("*. \\(p\\)", taxon_lifestage),
         !grepl("*. \\(a\\)", taxon_lifestage)) %>% 
  select(site, event, month, year, sort, code, count, meas, flag, everything())

set.seed(1312)
inverts_int2 = 
  extract_details(
    inverts_int
    ) 

inverts_clean = inverts_int2 %>% 
  select(site, event, month, year, code, taxon_lifestage,mass) %>% 
  unnest(mass) %>% 
  mutate(across(mass, ~round(.x,4))) %>% 
  summarise(count = n(), .by = c('site','event','month','year', 'code','taxon_lifestage','mass'))
# save the 
saveRDS(inverts_clean, here('data/derived-data/inverts_clean.rds'))


inverts_clean_dw = inverts_int2 %>% 
  select(site, event, month, year, mass) %>% 
  unnest(mass) %>% 
  mutate(across(mass, ~round(.x,5))) %>% 
  summarise(count = n(), .by = c('site','event','month','year','mass'))
# save the 
saveRDS(inverts_clean_dw, here('data/derived-data/inverts_clean_dw.rds'))
