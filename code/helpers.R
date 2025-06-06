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


### Settings -----

theme_set(theme_minimal())
