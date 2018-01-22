library(rethinking)

#### load in the data #### 
load("scratch/processed_data_nar.rda")

# make the tibble into a dataframe for map 
df <- as.data.frame(df_correct_only)

# tidy 
rm(df_correct_only)


######################### model 1 ################################
# Finally add random intercepts!
# 
# Start simple with intercepts only!
# 
# Use a log-link to account for skew!
# rt ~ exp(difficulty * targ_press)
# 
# Using informed priors
#################################################################

m_tp_diff_1 <- map2stan(
	alist(
	    rt ~ dlnorm(mu, sigma),
	    mu <- a + a_p[participant], 
	    # specify priors!
	    a ~ dnorm(1, 1),
	    a_p[participant] ~ dnorm(0, sig_p),
	    sigma ~ dcauchy(0, 1),
	    sig_p ~ dcauchy(0, 1)),
	data = df)

#### save models into a subfolder within scratch ####
save(m_tp_diff_1, file = "scratch/models/m_tp_diff_mixed_1")


######################### model 2 ################################
# Finally add random intercepts!
# 
# Add in slopes
# 
# Use a log-link to account for skew!
# rt ~ exp(difficulty * targ_press)
# 
# Using informed priors
#################################################################

m_tp_diff_2 <- map2stan(
	alist(
	    rt ~ dlnorm(mu, sigma),
	    mu <- a + a_p[participant] + b_diff*theta + b_tp*targ_pr + b_tp_diff*targ_pr*theta,  
	    # specify priors!
	    a ~ dnorm(1, 1),
	    a_p[participant] ~ dnorm(0, sig_p),
	    b_diff ~ dnorm(1, 1.5), 
	    b_tp ~ dnorm(-1, 1),
	    b_tp_diff ~ dnorm(-1, 1),
	    sigma ~ dcauchy(0, 1),
	    sig_p ~ dcauchy(0, 10)),
	data = df)

#### save models into a subfolder within scratch ####
save(m_tp_diff_2, file = "scratch/models/m_tp_diff_mixed_2")


#### maybe try a weaker prior for the intercept because it moves a lot ####
# also, should the intercept be allowed to go below 0?
# At the moment it shouldn't as we have only scaled the difficulty and not
# centred it... which we should do seeing as we have centred everything else

m_tp_diff_3 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma),
    mu <- a + a_p[participant] + b_diff*theta + b_tp*targ_pr + b_tp_diff*targ_pr*theta,  
    # specify priors!
    a ~ dnorm(1, 10),
    a_p[participant] ~ dnorm(0, sig_p),
    b_diff ~ dnorm(1, 1.5), 
    b_tp ~ dnorm(-1, 1),
    b_tp_diff ~ dnorm(-1, 1),
    sigma ~ dcauchy(0, 1),
    sig_p ~ dcauchy(0, 10)),
  data = df)

#### save models into a subfolder within scratch ####
save(m_tp_diff_3, file = "scratch/models/m_tp_diff_mixed_3")


#### tre the same again using centred diff data ####
m_tp_diff_4 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma),
    mu <- a + a_p[participant] + b_diff*theta_c + b_tp*targ_pr + b_tp_diff*targ_pr*theta_c,  
    # specify priors!
    a ~ dnorm(1, 10),
    a_p[participant] ~ dnorm(0, sig_p),
    b_diff ~ dnorm(1, 3), 
    b_tp ~ dnorm(-1, 3),
    b_tp_diff ~ dnorm(-1, 3),
    sigma ~ dcauchy(0, 10),
    sig_p ~ dcauchy(0, 10)),
  data = df)

#### save models into a subfolder within scratch ####
save(m_tp_diff_4, file = "scratch/models/m_tp_diff_mixed_4")


###########################################################
# Still some divergent sampling problems
# adding in a control = list(adapt_delta = 0.99)
###########################################################

m_tp_diff_5 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma),
    mu <- a + a_p[participant] + b_diff*theta_c + b_tp*targ_pr + b_tp_diff*targ_pr*theta_c,  
    # specify priors!
    a ~ dnorm(1, 10),
    a_p[participant] ~ dnorm(0, sig_p),
    b_diff ~ dnorm(1, 3), 
    b_tp ~ dnorm(-1, 3),
    b_tp_diff ~ dnorm(-1, 3),
    sigma ~ dcauchy(0, 10),
    sig_p ~ dcauchy(0, 10)),
  data = df, control = list(adapt_delta = 0.99))

#### save models into a subfolder within scratch ####
save(m_tp_diff_5, file = "scratch/models/m_tp_diff_mixed_5")


#############################
#### Try more iterations ####
#############################

m_tp_diff_6 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma),
    mu <- a + a_p[participant] + b_diff*theta_c + b_tp*targ_pr + b_tp_diff*targ_pr*theta_c,  
    # specify priors!
    a ~ dnorm(1, 10),
    a_p[participant] ~ dnorm(0, sig_p),
    b_diff ~ dnorm(1, 3), 
    b_tp ~ dnorm(-1, 3),
    b_tp_diff ~ dnorm(-1, 3),
    sigma ~ dcauchy(0, 10),
    sig_p ~ dcauchy(0, 10)),
  data = df, iter = 8000, warmup = 4000)

#### save models into a subfolder within scratch ####
save(m_tp_diff_6, file = "scratch/models/m_tp_diff_mixed_6")

#######################################
#### Maybe try with only TA trials ####
#######################################

# setup TA data 
df_TA <- df[df$targ_pr == 0,]

m_tp_diff_7 <- map2stan(
  alist(
    rt ~ dlnorm(mu, sigma),
    mu <- a + a_p[participant] + b_diff*theta_c, 
    # specify priors!
    a ~ dnorm(1, 10),
    a_p[participant] ~ dnorm(0, sig_p),
    b_diff ~ dnorm(1, 3),
    sigma ~ dcauchy(0, 10),
    sig_p ~ dcauchy(0, 10)),
  data = df_TA)

save(m_tp_diff_7, file = "scratch/models/m_tp_diff_mixed_7")
