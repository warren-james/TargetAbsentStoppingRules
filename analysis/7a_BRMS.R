#### Attempt at using brms package instead #### 

### load in libraries ####
library(brms)
library(rstan)

#### set options ####
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#### load in data #### 
load("scratch/processed_data_nar_TA.rda")

# turn tibble into data.frame for map 
df <- as.data.frame(df_TA)

# tidy 
rm(df_TA)

########## Model 1 ########## 
# Just RT by theta for now  #
#############################
# not sure if this is how to set priors, 
# I *think* this is setting the prior for the mean parameter value of theta?
# not sure how to set any other priors, but I will read up

m1_rt_theta <- brm(rt ~ theta + (1 + theta|participant),
                   data = df, family = lognormal,
                   prior = c(set_prior("normal(0.55,1)", class = "b", coef = "theta")),
                   warmup = 1000, iter = 2000, chains = 4)

save(m1_rt_theta, file = "scratch/models/brm_m1")




