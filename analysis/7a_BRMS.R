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
# added in the 'class = "cor"' prior, might help Rhat values?

m1_rt_theta <- brm(rt ~ theta + (1 + theta|participant),
                   data = df, family = lognormal,
                   prior = c(set_prior("normal(1,3)", class = "b", coef = "theta"),
                             set_prior("normal(0.55,1)", class = "Intercept"),
                             set_prior("cauchy(0,1.5)", class = "sd"),
                             set_prior("lkj(2)", class = "cor")), 
                   warmup = 1000, iter = 2000, chains = 4)

save(m1_rt_theta, file = "scratch/models/brm_m1")



######## Model 2 ########
# Adding in block_type  #
#########################
# no interactions
# fixed effect of block type
# no random effect of participant

m2_rt_theta_bt <- brm(rt ~ theta + block_type + (1 + theta|participant),
                      data = df, family = lognormal,
                      prior = c(set_prior("normal(1,3)", class = "b", coef = "theta"),
                                set_prior("normal(0.55,1)", class = "Intercept"),
                                set_prior("cauchy(0,1.5)", class = "sd"),
                                set_prior("lkj(2)", class = "cor"),
                                set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                set_prior("normal(0,1)", class = "b", coef = "block_typesinewave")),
                      warmup = 1000, iter = 2000, chains = 4)

save(m2_rt_theta_bt, file = "scratch/models/brm_m2")


####### Model 3 #######
# add in interactions #
## and random slopes ##
#######################

m3_rt_theta_bt <- brm(rt ~ (theta + block_type)^2 + (1 + theta|participant),
                      data = df, family = lognormal,
                      prior = c(set_prior("normal(1,3)", class = "b", coef = "theta"),
                                set_prior("normal(0.55,1)", class = "Intercept"),
                                set_prior("cauchy(0,1.5)", class = "sd"),
                                set_prior("lkj(2)", class = "cor"),
                                set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                set_prior("normal(0,1)", class = "b", coef = "block_typesinewave")),
                      warmup = 1000, iter = 2000, chains = 4)

save(m3_rt_theta_bt, file = "scratch/models/brm_m3")



########## Model 4##########
# Adding in random effects #
############################

m4_rt_theta_bt <- brm(rt ~ (theta + block_type)^2 + (1 + theta + block_type|participant),
                      data = df, family = lognormal,
                      prior = c(set_prior("normal(1,3)", class = "b", coef = "theta"),
                                set_prior("normal(0.55,1)", class = "Intercept"),
                                set_prior("cauchy(0,1.5)", class = "sd"),
                                set_prior("lkj(2)", class = "cor"),
                                set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                set_prior("normal(0,1)", class = "b", coef = "block_typesinewave"),
                                set_prior("normal(0,1)", class = "b", coef = "theta:block_typerandom"),
                                set_prior("normal(0,1)", class = "b", coef = "theta:block_typesinewave")),
                      warmup = 1000, iter = 2000, chains = 4, 
                      control = list(adapt_delta = 0.95))

save(m4_rt_theta_bt, file = "scratch/models/brm_m4")

# had issues with divergent samples, re-ren with control = list(adapt_delta = 0.95) added in
# this removed the divergent transitions so the chains can be trusted to have converged

####### Model 5 #######
##### Add in cprt #####
#######################
# we should create a centred prt, or scaled, or both?
# or maybe there's a way to tell the model that it isn't centred?
# May be centred already in the code automatically?

m5_rt_theta_bt_prt <- brm(rt ~ (theta + block_type)^2 + p_rt + (1 + theta + block_type|participant),
                          data = df, family = lognormal,
                          prior = c(set_prior("normal(1,1.5)", class = "b", coef = "theta"),
                                    set_prior("normal(0,1)", class = "b", coef = "p_rt"),
                                    set_prior("normal(0.55,1)", class = "Intercept"),
                                    set_prior("cauchy(0,1.5)", class = "sd"),
                                    set_prior("lkj(2)", class = "cor"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typesinewave"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typesinewave")),
                          warmup = 1000, iter = 2000, chains = 4, 
                          control = list(adapt_delta = 0.95))

save(m5_rt_theta_bt_prt, file = "scratch/models/brm_m5")


# divergent transitions reached with warning of max_treedepth reached
# should set this above 10 and run again
