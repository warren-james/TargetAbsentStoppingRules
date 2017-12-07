library(rethinking)
library(tidyverse)
library(scales)

#################################################################
# create functions 
#################################################################

# show random intercepts


#################################################################

# load in data 
load("scratch/processed_data_nar.rda")


#################################################################
# model 1 - getting started
#################################################################

load("scratch/models/m_tp_diff_mixed_2")
precis(m_tp_diff_1)

# extract samples from model
post <- extract.samples(m_tp_diff_1)

# get 95% HPDI for each person 
intercepts_p <- lapply(as.data.frame(post$a_p), quantile, probs = c(0.025, 0.975))

plt <- ggplot()
plt <- plt + geom