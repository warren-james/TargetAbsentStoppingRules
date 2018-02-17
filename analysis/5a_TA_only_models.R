library(rethinking)
rstan_options(auto_write = TRUE)
#### TA judgement model ####
# load in data
load("scratch/processed_data_nar_TA.rda")

# turn tibble into data.frame for map 
df <- as.data.frame(df_TA)

# tidy 
rm(df_TA)

############# Model 1 ##############
# random intercepts by participant #
#        intercepts only           #
#      Use log-link for skew       #
####################################

m_ta_only_1 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <- a + a_p[participant], 
    # priors
    a ~ dnorm(1, 3),
    a_p[participant] ~ dnorm(0, sig_p),
    sigma ~ dcauchy(0, 3),
    sig_p ~ dcauchy(0, 3)
  ),
data = df)

# save the model 
save(m_ta_only_1, file = "scratch/models/m_ta_only_1")


########## Model 2 #########
# Add in random slopes for Theta #
# using similar notation to 13.22 in McElreath
############################

m_ta_only_2 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  a + a_p[participant] + (b_theta + b_theta_p[participant]) * theta, 

    # # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho),

    # fixed priors  
    a ~ dnorm(1, 3),
    b_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0, 3),
    sigma_p ~ dcauchy(0, 3),
    Rho ~ dlkjcorr(2)
  ),
  data = df,
  iter = 1000, warmup = 1000, chains = 3, cores = 3)

# save the model 
save(m_ta_only_2, file = "scratch/models/m_ta_only_2")

########## Model 3 #########
# addig in block type!
############################


m_ta_only_3 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A + B_theta + B_isra + B_issi,
    A <- a + a_p[participant],
    B_theta <- (b_theta + b_theta_p[participant]) * theta, 
    B_isra <- (b_isra + b_isra_p[participant]) * isra, 
    B_issi <- (b_issi + b_issi_p[participant]) * issi, 
    # # adaptive priors
    c(a_p, b_theta_p, b_isra, b_issi)[participant] ~ dmvnormNC(sigma_p, Rho),

    # fixed priors  
    a ~ dnorm(1, 3),
    b_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0, 3),
    sigma_p ~ dcauchy(0, 3),
    Rho ~ dlkjcorr(4)
  ),
  data = df,
  iter = 1000, warmup = 1000, chains = 3, cores = 3)

# save the model 
save(m_ta_only_3, file = "scratch/models/m_ta_only_3")

##### Attempts to add in block_type ####

##### temp #########
# with fixed effects for block_type
# I think that's what it is anyway, but I could be wrong
# This uses dummy variable coding and seems to work, so we might
# want to stick with this way of doing things

m_ta_only_temp <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A + B_theta + B_isra + B_issi,
    A <- a + a_p[participant],
    B_theta <- (b_theta + b_theta_p[participant]) * theta, 
    B_isra <- b_isra*isra, 
    B_issi <- b_issi*issi, 
    # # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho),
    
    # fixed priors  
    a ~ dnorm(1, 3),
    b_theta ~ dnorm(1,3),
    b_isra ~ dnorm(1,3),
    b_issi ~ dnorm(1,3),
    sigma ~ dcauchy(0, 3),
    sigma_p ~ dcauchy(0, 3),
    Rho ~ dlkjcorr(4)
  ),
  data = df,
  iter = 2000, warmup = 1000, chains = 3, cores = 3)

# save the model 
save(m_ta_only_temp, file = "scratch/models/m_ta_only_temp")






#### same as above but instead using coerce_index function ####
# model rerun, but I don't think it's right...
# make block_type_id 
# df$block_type_id <- coerce_index(df$block_type)
# "a" doesn't really mean anything in this model... 
# so we should probably ignore it 

# run model
m_ta_only_temp_2 <- map2stan(
   alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A + B_theta,
    A <- a + a_p[participant] + a_bt[block_type_id],
    B_theta <- (b_theta + b_theta_p[participant]) * theta, 
    
    # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho_p),
    a_bt[block_type_id] ~ dnorm(0, sigma_bt),
    
    # fixed priors  
    a ~ dnorm(1, 2),
    b_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0, 1.5),
    sigma_p ~ dcauchy(0, 1.5),
    sigma_bt ~ dcauchy(0,1.5),
    Rho_p ~ dlkjcorr(4)
  ),
  data = df,
  iter = 2000, warmup = 1000, chains = 3, cores = 3)

# save the file 
save(m_ta_only_temp_2, file = "scratch/models/m_ta_only_temp_2")


#### add in varying slopes for block_type ####
# This one probably won't work, it's using the index variable coding
# but that means that "a" becomes a bit meaningless

m_ta_only_temp_3 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A + B_theta,
    A <- a + a_p[participant] + a_bt[block_type_id],
    B_theta <- (b_theta + b_theta_p[participant] + b_theta_bt[block_type_id]) * theta, 
    
    # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho_p),
    c(a_bt, b_theta_bt)[block_type_id] ~ dmvnormNC(sigma_bt, Rho_bt),
    
    # fixed priors  
    a ~ dnorm(1, 2),
    b_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0, 1.5),
    sigma_p ~ dcauchy(0, 1.5),
    sigma_bt ~ dcauchy(0,1.5),
    Rho_p ~ dlkjcorr(4),
    Rho_bt ~ dlkjcorr(4)
  ),
  data = df,
  iter = 2000, warmup = 1000, chains = 3, cores = 3)


#### using dummy variables #### 
# This model has intercepts and slopes for block type
# These effects aren't by participant though

#### NB changes ####
# the priors for b_ra_theta and b_si_theta were as follows:
# b_ra_theta ~ dnorm(1,3)
# b_si_theta ~ dnorm(1,3)
# Same for their intercept values
# b_isra ~ dnorm(1,3)
# b_issi ~ dnorm(1,3)
# But this is dumb. The best assumption to make is that block_type 
# won't change anything (I know what I mean here) so the priors should
# reflect this. As such, these were all changed to be dnorm(0,1)

# Also, "a" can be a loosely informed prior based on Anna's data. 
# remember, we are operating on a log scale, so this should be accounted for
# Check 2_ for reasons for the prior for a

#### Needs to be rerun with new priors ####

m_ta_only_temp_4 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A  + B_theta + B_blk + B_blk_th,
    A <- a + a_p[participant],
    B_theta <- (b_theta + b_theta_p[participant]) * theta, 
    B_blk <- b_issi*issi + b_isra*isra,
    B_blk_th <- (b_si_theta*issi + b_ra_theta*isra) * theta,
    
    # # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho),
    
    # fixed priors  
    a ~ dnorm(0.55,1),
    b_theta ~ dnorm(1,3),
    b_isra ~ dnorm(0,1),
    b_issi ~ dnorm(0,1),
    b_ra_theta ~ dnorm(0,1),
    b_si_theta ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    sigma_p ~ dcauchy(0,1),
    Rho ~ dlkjcorr(4)
  ),
  data = df,
  iter = 2000, warmup = 1000, chains = 3, cores = 3)

save(m_ta_only_temp_4, file = "scratch/models/m_ta_only_temp_4")

#### with random effects #### 
# Same as above, except with effects for participants
# Not sure how to do this adaptive priors?

m_ta_only_temp_5 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A  + B_theta + B_blk + B_blk_th,
    A <- a + a_p[participant],
    B_theta <- (b_theta + b_theta_p[participant]) * theta, 
    B_blk <- (b_issi + b_issi_p[participant])*issi +
             (b_isra + b_isra_p[participant])*isra,
    B_blk_th <- ((b_si_theta + b_si_theta_p)*issi +
                 (b_ra_theta + b_ra_theta_p)*isra) * theta,
    
    # # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho),
    
    # fixed priors  
    a ~ dnorm(1, 3),
    b_theta ~ dnorm(1,3),
    b_isra ~ dnorm(1,3),
    b_issi ~ dnorm(1,3),
    b_ra_theta ~ dnorm(1,3),
    b_si_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0, 1),
    sigma_p ~ dcauchy(0, 1),
    Rho ~ dlkjcorr(4)
  ),
  data = df,
  iter = 2000, warmup = 1000, chains = 3, cores = 3)


#### add in varying slopes for block_type: Index variable coding ####
# attempt to use index variable coding to make things a bit more 
# straightforward

m_ta_only_temp_6 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <-  A + B_theta,
    A <- + a_bt[block_type_id] + a_p[participant],
    B_theta <- (b_theta_bt[block_type_id] + b_theta_p[participant]) * theta, 
    
    # adaptive priors
    c(a_p, b_theta_p)[participant] ~ dmvnormNC(sigma_p, Rho_p),
    
    # fixed?
    a_bt ~ dnorm(a, sigma_bt),
    b_theta_bt ~ dnorm(b_theta, sigma_b_theta),
    
    # fixed priors  
    a ~ dnorm(0.55, 1),
    b_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0,1.5)
    sigma_b_theta ~ dcauchy(0, 1.5),
    sigma_p ~ dcauchy(0, 1.5),
    sigma_bt ~ dcauchy(0,1.5),
    Rho_p ~ dlkjcorr(4)
  ),
  data = df,
  iter = 2000, warmup = 1000, chains = 3, cores = 3)


