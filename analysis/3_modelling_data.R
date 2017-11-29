library(rethinking)
#library(tidyverse)

#### load in the data #### 
load("scratch/processed_data_nar.rda")

# make the tibble into a dataframe for map 
df_correct_only <- as.data.frame(df_correct_only)

#### first model #### 
# Just the difficulty and targ_pres
# rt ~ difficulty * targ_press + random effects

m_tp_diff_1 <- map(
  alist(
    rt ~ dnorm( mu , sigma ) ,
    mu <- a + b_diff*difficulty + b_tp*targ_pr , 
    a ~ dnorm( 1 , 10 ) ,
    b_diff ~ dnorm( 1 , 10 ) , 
    b_tp ~ dnorm( 1 , 10 ) , 
    sigma ~ dnorm( 0 , 1 )
  ) ,
data = df_correct_only)


