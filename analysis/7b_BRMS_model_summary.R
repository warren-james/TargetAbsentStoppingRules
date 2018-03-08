#### plotting models fitted using brms ####

#### load libraries #### 
library(brms)
library(rstan)
library(tidyverse)

#### Model 1 ####
# This is a simple test to make sure we can get the plots working 
# The first model is simply RT by Theta (with random effect of participant)
# not sure how to get a plot per participant though... could look into that?

# load in model # 
load("scratch/models/brm_m1")

# get marginal effects # 
m1_effects <- marginal_effects(m1_rt_theta)

# make plot #
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

# tidy
rm(list = ls())

#### Model 2 ####
# Just adds in block type as a predictor
# Same as above otherwise

# load in model 
load("scratch/models/brm_m2")

# marginal effects 
m2_effects <- marginal_effects(m2_rt_theta_bt)

# make plots 
plot(m2_effects, plot = FALSE)[[2]] + 
  ggplot2::ggtitle("Model 2 with effect of block type on average RT on Difficulty (Theta)") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5)) + 
  ggplot2::labs(y = "RT", x = "Theta")

# tidy
rm(list = ls())

#### Model 3 #### 
# This model adds in the interaction of block_type and theta
# random effects are still just on the slope, so no effect of block type
# or the interaction 

# load in model 
load("scratch/models/brm_m3")

# marginal effects
m3_effects <- marginal_effects(m3_rt_theta_bt)

# make plots 
plot(m3_effects, plot = FALSE)[[3]] + 
  ggplot2::ggtitle("Model with interaction of Block type and Difficulty (Theta)") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5)) + 
  ggplot2::labs(y = "RT", x = "Theta")

# tidy
rm(list = ls())

#### Model 4 ####
# This is the same as above, except now the effect of block_type has 
# been added into the random effects structure 

# NB: seems to be quite worth it... reduces the looic value by quite a lot. 
# Also, divergence issues were fixed

# load in model 
load("scratch/models/brm_m4")

# marginal effects
m4_effects <- marginal_effects(m4_rt_theta_bt)

# make plots 
plot(m4_effects, plot = FALSE)[[3]] + 
  ggplot2::ggtitle("Model with interaction of Block type and Difficulty (Theta)") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5)) + 
  ggplot2::labs(y = "RT", x = "Theta")

#### Model 5 ####
# Added in a fixed effect of previous response time
# at the moment, this has not been centred, but from what I've read this seems to happen 
# automatically within this package

# NB: running loo() on this and the previous models shows that this one fits the data a lot better
# Still need to re run it with the increased tree depth though to make sure this is still the same 
# after fixing divergence issues

# load model
load("scratch/models/brm_m5")

# marginal effects 
m5_effects <- marginal_effects(m5_rt_theta_bt_prt)

# make plots 
plot(m5_effects, plot = TRUE)
# use this to see which plots you would like to use
# however, I think this makes it clear that we need to centre
# also, we need to inlcude previous accuracy in this model...
# need to look at contrasts

#### model 5_5 ####
# Model with interaction of block type and theta in random effects
# no model after this one seems to improve fit...
# maybe need to consider including previous accuracy?

# load model 
load("scratch/models/brm_m5_5")

# marginal effects 
m5_5_effects <- marginal_effects(m5_5_rt_theta_bt_prt)

# plot 
plot(m5_5_effects, plot = FALSE)[[4]] + 
  ggplot2::ggtitle("Model with interaction of Block type and Difficulty (Theta)") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5)) + 
  ggplot2::labs(y = "RT", x = "Theta")

#### Model 8 ####
# This has the change_th in there, so might be more interesting
# Adding change_th as a fixed effect doesn't seem to help the model fit that much better than
# model 4... should try with the interaction as this will probably be more revealing...

# load model 
load("scratch/models/brm_m8")

# marginal effects 
m8_effects <- marginal_effects(m8_rt_theta_bt_cht)

# make plot 
plot(m8_effects, plot = TRUE)


