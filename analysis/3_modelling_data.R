library(rethinking)

#### load in the data #### 
load("scratch/processed_data_nar.rda")

# make the tibble into a dataframe for map 
df <- as.data.frame(df_correct_only)

# tidy 
rm(df_correct_only)

######################### model 1 ################################
# This model is a simple test, and clearly wrong!
# No random effects are present in this model
# Linear model, does not take skew of reaction times into account
# None the less, we can use it to check we know what we're doing!
# rt ~ difficulty * targ_press
# 
# Use dumb weak (0, 10) priors
#################################################################

m_tp_diff_1 <- map(
	alist(
	    rt ~ dnorm(mu, sigma),
	    mu <- a + b_diff*difficulty + b_tp*targ_pr, 
	    # specify priors!
	    a ~ dnorm(0, 10),
	    b_diff ~ dnorm(0, 10), 
	    b_tp ~ dnorm(0, 10), 
	    sigma ~ dcauchy(0, 1)),
	data = df)		

#### save models into a subfolder within scratch ####
save(m_tp_diff_1, file = "scratch/models/m_tp_diff_1")
