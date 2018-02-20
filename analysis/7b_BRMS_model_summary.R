#### plotting models fitted using brms ####

#### load libraries #### 
library(brms)
library(rstan)
library(tidyverse)

#### For first model ####
# This is a simple test to make sure we can get the plots working 
# The first model is simply RT by Theta (with random effect of participant)
# not sure how to get a plot per participant though... could look into that?

#### load in model #### 
load("scratch/models/brm_m1")

#### get marginal effects #### 
m1_effects <- marginal_effects(m1_rt_theta)

#### make plot ####
# the [[1]] part refers to the what part of the model you want to model
# In this case, there is only the effect of theta, so that will be it 
# for future plots, this number will need to change
# But for this one we can just write "plot(m1_effects, plot = T)" and 
# get the same thing as here... but we can't edit the plot any further 
# if we do that. 
plot(m1_effects, plot = FALSE)[[1]] + 
  ggplot2::ggtitle("Model of Response Times (RT) by 'Difficulty' (Theta)") + 
  ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
  ggplot2::labs(y = "RT", x = "Theta")


