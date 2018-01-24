library(rethinking)

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
# Add in a slope for Theta #
############################

m_ta_only_2 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma), 
    mu <- a + a_p[participant] + b_theta*theta, 
    # priors
    a ~ dnorm(1, 3),
    a_p[participant] ~ dnorm(0, sig_p),
    b_theta ~ dnorm(1,3),
    sigma ~ dcauchy(0, 3),
    sig_p ~ dcauchy(0, 3)
  ),
  data = df)

# save the model 
save(m_ta_only_2, file = "scratch/models/m_ta_only_2")

########## Model 3 #########
# Add in random slopes for Theta #
# using similar notation to 13.22 in McElreath
############################

m_ta_only_3 <- map2stan(
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
  data = df)

# save the model 
save(m_ta_only_3, file = "scratch/models/m_ta_only_3")