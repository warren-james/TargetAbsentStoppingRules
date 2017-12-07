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
	    sig_p ~ dcauchy(0, 1)),
	data = df)

#### save models into a subfolder within scratch ####
save(m_tp_diff_2, file = "scratch/models/m_tp_diff_mixed_2")