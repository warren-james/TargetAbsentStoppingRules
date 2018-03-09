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
#   get_priors(model formula)    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########## Model 1 ########## 
# Just RT by theta for now  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#~~~~~~~~~~~~~~~~~~~~~~~#
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
#  and random slopes  #
#~~~~~~~~~~~~~~~~~~~~~#

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

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
#     Add in cprt     #
#~~~~~~~~~~~~~~~~~~~~~#
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
                          control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m5_rt_theta_bt_prt, file = "scratch/models/brm_m5")

# This fits much better than the previous model (m4)
###### Model 5.5 #######
# add in random effect #
#   for interaction    #
#~~~~~~~~~~~~~~~~~~~~~~#

m5_5_rt_theta_bt_prt <- brm(rt ~ (theta + block_type)^2 + p_rt + (1 + (theta + block_type)^2|participant),
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
                          control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m5_5_rt_theta_bt_prt, file = "scratch/models/brm_m5_5")

####### Model 6 #######
# add in p_rt as rand #
#~~~~~~~~~~~~~~~~~~~~~#

m6_rt_theta_bt_prt <- brm(rt ~ (theta + block_type)^2 + p_rt + (1 + theta + block_type + p_rt|participant),
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
                          control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m6_rt_theta_bt_prt, file = "scratch/models/brm_m6")

# m6 fits a little better than m5, though check with Alasdair to see what he thinks

####### Model 7 #######
# add in interaction  #
#   with block_type   #
#       and prt       #
#~~~~~~~~~~~~~~~~~~~~~#

m7_rt_theta_bt_prt <- brm(rt ~ theta + block_type + p_rt + (theta * block_type) + (p_rt * block_type) +
                            (1 + theta + block_type + p_rt|participant),
                          data = df, family = lognormal,
                          prior = c(set_prior("normal(1,1.5)", class = "b", coef = "theta"),
                                    set_prior("normal(0,1)", class = "b", coef = "p_rt"),
                                    set_prior("normal(0.55,1)", class = "Intercept"),
                                    set_prior("cauchy(0,1.5)", class = "sd"),
                                    set_prior("lkj(2)", class = "cor"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typesinewave"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typesinewave"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typerandom:p_rt"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typesinewave:p_rt")),
                          warmup = 1000, iter = 2000, chains = 4, 
                          control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m7_rt_theta_bt_prt, file = "scratch/models/brm_m7")

# this model doesn't really improve the fit beyond m6....
# so it looks like there is not much of an interaction between block type and 
# previous response time. However, adding in prt as a main effect (and random 
# by participant) does improve the fit... quite surprising really.

####### Model 8 #######
#     remove p_rt     #
# add in change theta #
#~~~~~~~~~~~~~~~~~~~~~#
# Instead of using prt, we are now using change in difficulty. 
# This should be a bit better than using just raw p_rt which 
# is going to reflect the change in difficulty anyway... but in
# a messy way when there is a mixture of this. Having said that, 
# we probably want to let this interact with theta more generally
# just so we know there is a difference when in the harder condition
# but little/no change occurs from trial to trial.
# For now though, it's just added as a fixed effect.

#### TO RUN MODEL 8 ####

m8_rt_theta_bt_cht <- brm(rt ~ (theta + block_type)^2 + change_th + (1 + theta + block_type|participant),
                          data = df, family = lognormal,
                          prior = c(set_prior("normal(1,1.5)", class = "b", coef = "theta"),
                                    set_prior("normal(0,1)", class = "b", coef = "change_th"),
                                    set_prior("normal(0.55,1)", class = "Intercept"),
                                    set_prior("cauchy(0,1.5)", class = "sd"),
                                    set_prior("lkj(2)", class = "cor"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typesinewave"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typesinewave")),
                          warmup = 1000, iter = 2000, chains = 4, 
                          control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m8_rt_theta_bt_cht, file = "scratch/models/brm_m8")

##### Model 8.5 #####
#  add interaction  #
# in random effects #
#~~~~~~~~~~~~~~~~~~~#

m8_5_rt_theta_bt_cht <- brm(rt ~ (theta + block_type)^2 + change_th +
                            (1 + theta + block_type + theta*block_type|participant),
                          data = df, family = lognormal,
                          prior = c(set_prior("normal(1,1.5)", class = "b", coef = "theta"),
                                    set_prior("normal(0,1)", class = "b", coef = "change_th"),
                                    set_prior("normal(0.55,1)", class = "Intercept"),
                                    set_prior("cauchy(0,1.5)", class = "sd"),
                                    set_prior("lkj(2)", class = "cor"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "block_typesinewave"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typerandom"),
                                    set_prior("normal(0,1)", class = "b", coef = "theta:block_typesinewave")),
                          warmup = 1000, iter = 2000, chains = 4, 
                          control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m8_5_rt_theta_bt_cht, file = "scratch/models/brm_m8_5")

########## Model 9 ##########
# interaction of block_type #  
#       and change_th       #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

m9_rt_theta_bt_cht <- brm(rt ~ (theta + block_type)^2 + block_type*change_th +
                              (1 + theta + block_type + theta*block_type|participant),
                            data = df, family = lognormal,
                            prior = c(set_prior("normal(1,1.5)", class = "b", coef = "theta"),
                                      set_prior("normal(0,1)", class = "b", coef = "change_th"),
                                      set_prior("normal(0.55,1)", class = "Intercept"),
                                      set_prior("cauchy(0,1.5)", class = "sd"),
                                      set_prior("lkj(2)", class = "cor"),
                                      set_prior("normal(0,1)", class = "b", coef = "block_typerandom"),
                                      set_prior("normal(0,1)", class = "b", coef = "block_typesinewave"),
                                      set_prior("normal(0,1)", class = "b", coef = "theta:block_typerandom"),
                                      set_prior("normal(0,1)", class = "b", coef = "theta:block_typesinewave"),
                                      set_prior("normal(0,1)", class = "b", coef = "block_typerandom:change_th"),
                                      set_prior("normal(0,1)", class = "b", coef = "block_typesinewave:change_th")),
                            warmup = 1000, iter = 2000, chains = 4, 
                            control = list(adapt_delta = 0.95, max_treedepth = 12))

save(m9_rt_theta_bt_cht, file = "scratch/models/brm_m9")

# This change doesn't look like it gives any clear explanation of what's going on..
# maybe because it's correlated with prt?

#### Model 10 ####
