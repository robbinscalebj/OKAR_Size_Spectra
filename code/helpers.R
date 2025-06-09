library(junkR)
library(tidyverse)
library(tidybayes)
library(isdbayes)
library(poweRlaw)
library(readxl)
library(here)
library(univariateML)
library(poweRlaw)
library(extraDistr)
library(tidybayes)
here::i_am('code/helpers.R')


### Helper functions ----
estimate_lnnorm = function(mean = NULL, cv = 'rlnorm(n = 1,meanlog = 0.4951105, sdlog = 0.4314428)'){
  if(is.numeric(cv)){
    cv = cv
  } else if(is.character(cv)){
    cv = eval(parse(text = cv))
  }
  sigma_square = log(1+cv^2)
  meanlog = log(mean) - (sigma_square/2)
  return(c(meanlog = meanlog, sdlog = sqrt(sigma_square)))
}

extract_details = function(df = NULL, count_var = 'count',...){
  
  taxaVec = as.list(df$code)
  countVec = unlist(df[count_var])
  measVec = as.list(df$meas)
  
  
  massList = df %>%
    rowwise %>% 
    mutate(masses = list(na.omit(c_across(l1_dw:h10_dw)))) %>% 
    select(masses) %>% 
    split(., seq(nrow(.)))
  
  flagList = as.list(df$flag)
  
  sampleList = 
    pmap(list(
      taxaVec,
      flagList,
      countVec,
      measVec,
      massList
    ),\(a,b,c,d,e){
      if(a == 'Chironom'){
        return(rlnorm(n = c, estimate_lnnorm(mean = 0.568)[1], estimate_lnnorm(mean = 0.568)[2]))
      } else{
        if(b == "0"){
          return(unlist(e))
        } else if(b =="dn"){
          return(unlist(sample(e, size = c, replace = TRUE)))
        } else if(b =="up"){
          return(unlist(e))
        }
      }
    })
  df$mass = sapply(sampleList, unname)
  return(df)
}

# function to bin and center data
bin_and_center <- function(data, var, breaks, ...){
  # data is a data frame
  # var is a string, and is the name of a column in data which you want to bin
  # breaks controls the number of bins as defined in hist() 
  # See ?hist for details
  
  # bin values using hist()
  binned_hist = hist(data[[var]], 
                     breaks = breaks, # need to predefine breaks
                     # e.g. Log2 breaks = 2^seq(min, max) 
                     # Log10 breaks = 10^seq(min, max)
                     include.lowest = TRUE, plot = FALSE)
  # calculate "left" and "right" edge of bins
  breaks_orig = binned_hist$breaks[1:(length(breaks)-1)]
  breaks_offset = binned_hist$breaks[2:length(breaks)]
  # total bin width = right edge - left edge
  break_width = breaks_offset - breaks_orig
  count = binned_hist$counts 
  dataout = data.frame(
    # normalize counts =count / width (White et al 1997)
    log_count_corrected = log10(count / break_width),
    # original midpoint of bin log10 transformed
    log_mids = log10(binned_hist$mids),
    log_mids_center = NA)
  # remove bins with 0 counts
  # -Inf comes from log10(count / break_width) above
  dataout = dataout[dataout$log_count_corrected !=-Inf,]
  # recenter data at x=0
  mid_row = ceiling(nrow(dataout)/2)
  # subtract value of mid row from all mids
  dataout$log_mids_center = 
    dataout[,"log_mids"] - dataout[mid_row,"log_mids"]
  dataout
}
### Settings -----

theme_set(theme_minimal())
