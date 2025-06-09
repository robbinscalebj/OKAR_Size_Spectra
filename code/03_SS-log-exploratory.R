i_am('code/03_SS-slope-exploratory.R')

inverts_clean_dw = readRDS('data/derived-data/inverts_clean_dw.rds')

breaks <- 2^seq(min(floor(log2(inverts_clean_dw$mass))),max(ceiling(log2(inverts_clean_dw$mass))))

test <- min(breaks) < min(inverts_clean_dw$mass) & max(breaks) > max(inverts_clean_dw$mass)

inverts_clean_long_list = 
  inverts_clean_dw %>% 
  uncount(count) %>% 
  named_group_split(site, event, month, year)

break_list = inverts_clean_long_list %>% 
  map(\(x){
    breaks <- 2^seq(min(floor(log2(x$mass))),max(ceiling(log2(x$mass))))
  })


binned_df = 
  map2(inverts_clean_long_list,
       break_list,
       \(x,y){
         bin_and_center(x,
                        var = "mass",
                        breaks = y
                        )
       }
       ) %>% 
  bind_rows(.id = 'sampleID') %>% 
  separate(sampleID, c('site','event','month','year'), remove = TRUE) 

binned_list = binned_df %>% 
  named_group_split(site)


# for(i in 1:length(binned_list)){
i = 6
 name = binned_list[[i]]$site %>% unique

binned_list %>% 
  pluck(i) %>% 
  ggplot()+
  geom_point(aes(x = log_mids_center, log_count_corrected))+
  geom_smooth(aes(x = log_mids_center, log_count_corrected), method = 'lm')+
  ggtitle(name)+
  facet_wrap(~event)
# }


dw_blmm = brm(bf(log_count_corrected ~ log_mids_center*site + 
                    (log_mids_center | event/site))
               , data = binned_df, 
               iter = 5e3,
               thin = 1,
               cores = 4,
               chains = 4)
summary(dw_blmm)

get_variables(dw_blmm)

site_int_posts <- dw_blmm %>% 
  tidybayes::spread_draws(`b_Intercept`, `b_site.*`, regex = TRUE, ndraws = 100) %>% 
  pivot_longer(cols = -c(.chain, .iteration, .draw, b_Intercept), names_to = 'variable', values_to = 'offset') %>% 
  mutate(site_int = b_Intercept + offset) %>%
  mutate(site = gsub("b_site(\\w{4}\\d{0,1})","\\1", variable)) %>% 
  select(site, b_Intercept, b_site = site_int, .chain, .iteration, .draw) %>% 
  pivot_longer(cols = c(b_Intercept, b_site), names_to = "parameter", values_to = "value")

site_int_posts %>% 
  mutate(site = case_when(parameter == 'b_Intercept' ~ 'BALL1',
                          .default = site)) %>% 
  ggplot()+
  geom_vline(data = site_int_posts %>% summarise(value = mean(value)), aes(xintercept = value))+
  geom_density(aes(x = value, after_stat(scaled)),trim = TRUE, fill = 'lightgrey', alpha = 0.5)+
  facet_wrap(~site)

ggsave(filename = here::here('figures/site_int_posts.jpg'),
       height = 6, width = 6)
               

site_slope_posts <- dw_blmm %>% 
  tidybayes::spread_draws(`b_log_mids_center`, `b_log_mids_center:site.*`, regex = TRUE, ndraws = 100) %>% 
  pivot_longer(cols = -c(.chain, .iteration, .draw, b_log_mids_center), names_to = 'variable', values_to = 'offset') %>% 
  mutate(site_slope = b_log_mids_center + offset) %>%
  mutate(site = gsub("b_log_mids_center:site(\\w{4}\\d{0,1})","\\1", variable)) %>% 
  select(site, b_slope = b_log_mids_center, b_site = site_slope, .chain, .iteration, .draw) %>% 
  pivot_longer(cols = c(b_slope, b_site), names_to = "parameter", values_to = "value")

site_slope_posts %>% 
  mutate(site = case_when(parameter == 'b_Intercept' ~ 'BALL1',
                          .default = site)) %>% 
  ggplot()+
  geom_vline(data = site_slope_posts %>% summarise(value = mean(value)), aes(xintercept = value))+
  geom_density(aes(x = value, after_stat(scaled)), trim = TRUE, fill = 'lightgrey', alpha = 0.5)+
  facet_wrap(~site)

ggsave(filename = here::here('figures/site_slope_posts.jpg'),
       height = 6, width = 6)
