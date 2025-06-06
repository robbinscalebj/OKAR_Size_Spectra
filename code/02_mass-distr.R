i_am('code/02_mass-distr.R')
source(here::here('code/01_wrangle-data.R'))
### --- Purpose ----
# This script is a dive into the taxa that we have fully sampled for length (i.e., 10 measurement)
# and what distributions best fit the data. We are assessing how variability will be 
# affected if we re-sample the masses or draw from a distribution.
# Ultimately, we will likely increase variability if we draw from distribution as 
# many have more dispersion than the data themselves.
### Code ----

full_counts = 
inverts_int %>% 
  filter(meas == 10 & count >= 10) %>% 
  select(site, event, month, year, code, taxon_lifestage, matches("*._dw")) %>% 
  pivot_longer(cols = matches("*._dw"), names_to = 'meas', values_to = 'mass') %>% 
  na.omit %>% 
  # filter(!grepl("*. \\(p\\)", taxon_lifestage),
  #        !grepl("*. \\(a\\)", taxon_lifestage))


dw_list = full_counts %>% 
  named_group_split(site, taxon_lifestage)


dw_distML = dw_list %>% 
  map(~model_select(.x$mass, models = c("gamma","norm","lnorm","nbinom","pareto","power"), return = 'all'))


map(dw_distML, \(x){
  x %>% pluck('model') %>% pluck(1)
}) %>% bind_rows(.id = 'top') %>% t %>% 
  table

n_sample =10
dw_distML %>% pluck(n_sample) %>% pluck('univariateML') %>% pluck('mlpower') %>% 
  unlist -> x1

x1_sample = data.frame(mass = extraDistr::rpower(n = 1e3, alpha = x1[1], beta = x1[2]))

dw_list %>% 
  pluck(n_sample) %>% 
  ggplot()+
  geom_density(aes(x = mass, after_stat(scaled)))+
  geom_density(data = x1_sample, aes(x = mass, after_stat(scaled)), fill = 'grey', alpha = 0.5 )
