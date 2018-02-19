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

##### NB: retreiving priors ######
# if you want to see what priors # 
# can be set in your model, use: #
### get_priors(model formula)  ###

########## Model 1 ########## 
# Just RT by theta for now  #
#############################
# not sure if this is how to set priors, 
# I *think* this is setting the prior for the mean parameter value of theta?
# not sure how to set any other priors, but I will read up

m1_rt_theta <- brm(rt ~ theta + (1 + theta|participant),
                   data = df, family = lognormal,
                   prior = c(set_prior("normal(1,3)", class = "b", coef = "theta"),
                             set_prior("normal(0.55,1)", class = "Intercept"),# think this sets intercept?
                             set_prior("cauchy(0,1.5)", class = "sd")), 
                   warmup = 1000, iter = 2000, chains = 4)

save(m1_rt_theta, file = "scratch/models/brm_m1")




