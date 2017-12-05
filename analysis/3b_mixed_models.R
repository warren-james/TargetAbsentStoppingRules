library(rethinking)

#### load in the data #### 
load("scratch/processed_data_nar.rda")

# make the tibble into a dataframe for map 
df <- as.data.frame(df_correct_only)

# tidy 
rm(df_correct_only)


######################### model 5 ################################
# Finally add random intercepts!
# 
# Use a log-link to account for skew!
# rt ~ exp(difficulty * targ_press)
# 
# Using informed priors
#################################################################

m_tp_diff_5 <- map(
	alist(
	    rt ~ dlnorm(mu, sigma),
	    mu <- a + b_diff*theta + b_tp*targ_pr + b_tp_diff*targ_pr*theta, 
	    # specify priors!
	    a ~ dnorm(1, 1),
	    b_diff ~ dnorm(1, 1.5), 
	    b_tp ~ dnorm(-1, 1),
	    b_tp_diff ~ dnorm(-1, 1),
	    sigma ~ dcauchy(0, 1)),
	data = df, start = start_points)		

#### save models into a subfolder within scratch ####
save(m_tp_diff_5, file = "scratch/models/m_tp_diff_5")
