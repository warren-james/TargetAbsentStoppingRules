library(rethinking)
#library(tidyverse)

#### load in the data #### 
load("scratch/processed_data_nar.rda")

# make the tibble into a dataframe for map 
test <- as.data.frame(df_correct_only)

#### first model #### 
# Just the difficulty and targ_pres
# no random effects are present in this model
# in Alasdair's words, it's therefore "crap"
# rt ~ difficulty * targ_press

m_tp_diff_1 <- map(
  alist(
    rt ~ dnorm( mu , sigma ) ,
    mu <- a + b_diff*difficulty + b_tp*targ_pr , 
    a ~ dnorm( 1 , 10 ) ,
    b_diff ~ dnorm( 1 , 10 ) , 
    b_tp ~ dnorm( 1 , 10 ) , 
    sigma ~ dnorm( 0 , 1 )
  ) ,
data = test)



#### save models into a subfolder within scratch ####
save(m_tp_diff_1, file = "scratch/models/m_tp_diff_1")


