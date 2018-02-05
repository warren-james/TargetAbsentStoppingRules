# Script to summarise TA model with block_type fixed effects 
# this script needs a lot of work to get it to work properly
# Don't really want to commit it but I should so it's there

library(rethinking)
library(tidyverse)
library(scales)
library(ggthemes)

#### work on m_ta_only_temp ####
# load in raw data
load("scratch/processed_data_nar_TA.rda")

df <- as.data.frame(df_TA)

# tidy 
rm(df_TA)



#################################################################
# create functions 
#################################################################

# get 97% credible interval for regression lines
get_hpdi_region_from_samples <- function(m, post, ln = TRUE) {
  
  pred_data <- list(
    block_type = rep(c("a", "issi", "isra"), each = 2),
    theta =   rep(c(0.12, 0.88), length(unique(df$block_type))))#,
  #targ_pr = rep(c(0, 0, 1, 1), length(unique(df$participant))))
  
  mu <- link(m, data = pred_data)
  mu.PI <- apply(mu, 2, PI)
  
  pred_data$lower <- mu.PI[1,]
  pred_data$upper <- mu.PI[2,]
  
  #pred_data$targ_pr <- as.factor(pred_data$targ_pr)
  #levels(pred_data$targ_pr) <- c("absent", "present")
  
  if (ln == TRUE) { 
    pred_data$lower <- exp(pred_data$lower)
    pred_data$upper <- exp(pred_data$upper)
  }
  
  names(pred_data)[1] <- "participant"
  pred_data <- as.data.frame(pred_data)
  return(pred_data)
}


# facet plot mixed model
plot_model_mixed_facet <- function(pred_lines, model_lines, title_text, lt) {
  plt <-  ggplot()  
  # add model fit
  plt <- plt + geom_ribbon(data = model_lines, 
                           aes(x = theta, ymin = lower, ymax = upper))#, fill = targ_pr))
  # add empirical data points
  plt <- plt + geom_jitter(data = df, 
                           aes(x = theta, y = rt),
                           shape = 3, alpha = 0.2, show.legend = FALSE) 
  # spec theme
  plt <- plt + scale_x_continuous("search difficulty", 
                                  limits = c(0, 1), expand = c(0, 0))
  plt <- plt + scale_y_continuous(name = "reaction time", trans = log2_trans())
  plt <- plt + ggtitle(title_text)
  plt <- plt + theme_bw()
  plt <- plt + facet_wrap( ~ participant)
  ggsave("scratch/random_incpt_facet.pdf", width = 10, height = 10)
}



##################################################################################################
# load in model 
load("scratch/models/m_ta_only_temp")

# extract samples 
post <- extract.samples(m_ta_only_temp)

#### everything below this kind of works a little bit... but not really ####
# get 95% HDPI for block_type
intercepts_bl <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a), quantile, probs = c(0.025, 0.975)))))
intercepts_si <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a + post$b_issi), quantile, probs = c(0.025, 0.975)))))
intercepts_ra <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a + post$b_isra), quantile, probs = c(0.025, 0.975)))))

# Get exp before creating data set 
# blocked
intercepts_bl[,1] <- exp(intercepts_bl[,1] + mean(post$a))
intercepts_bl[,2] <- exp(intercepts_bl[,2] + mean(post$a))
# random
intercepts_ra[,1] <- exp(intercepts_ra[,1] + mean(post$a + post$b_isra))
intercepts_ra[,2] <- exp(intercepts_ra[,2] + mean(post$a + post$b_isra))
#sinewave
intercepts_si[,1] <- exp(intercepts_si[,1] + mean(post$a + post$b_issi))
intercepts_si[,2] <- exp(intercepts_si[,2] + mean(post$a + post$b_issi))


intercepts_block_type <- rbind(intercepts_ra, intercepts_bl, intercepts_si)
names(intercepts_block_type) <- c("lower", "upper")
intercepts_block_type$block_type <- c("random","blocked","sinewave")

# 1 = ra, 2 = bl, 3 = si
# tidy 
rm(intercepts_bl, intercepts_ra, intercepts_si)

# display range of intercepts, again this is probably not 100% correct 
# (but good enough for now?)
theta <- mean(post$sigma_p)
mu <- mean(post$a)

model_lines <- get_hpdi_region_from_samples(m_ta_only_temp, post2, TRUE)

#### Try a dumb way ####
# this gives the estimates for the intercepts in the different block_types
mu.block <- post$a
mu.sine <- post$a + post$b_issi
mu.rand <- post$a + post$b_isra
mu.theta <- post$b_theta

#precis(data.frame(mu.block,mu.sine, mu.rand)) 


# this should be a manual way of doing the link part to get the HPDI's
temp <- list(block_type = rep(c("Blocked", "Sine Wave", "Random"), each = 2),
                      theta =   rep(c(0.12, 0.88), length(1:3)))

#### this way is very long winded... using link must be quicker but I can't figure that out
# small theta
temp$lower[temp$block_type == "Blocked" & temp$theta == 0.12] <- (exp(HPDI(mu.block))*exp(0.12))[1]
temp$upper[temp$block_type == "Blocked" & temp$theta == 0.12] <- (exp(HPDI(mu.block))*exp(0.12))[2]

temp$lower[temp$block_type == "Sine Wave" & temp$theta == 0.12] <- (exp(HPDI(mu.sine))*exp(0.12))[1]
temp$upper[temp$block_type == "Sine Wave" & temp$theta == 0.12] <- (exp(HPDI(mu.sine))*exp(0.12))[2]

temp$lower[temp$block_type == "Random" & temp$theta == 0.12] <- (exp(HPDI(mu.rand))*exp(0.12))[1]
temp$upper[temp$block_type == "Random" & temp$theta == 0.12] <- (exp(HPDI(mu.rand))*exp(0.12))[2]

# big theta
temp$lower[temp$block_type == "Blocked" & temp$theta == 0.88] <- (exp(HPDI(mu.block))*exp(0.88))[1]
temp$upper[temp$block_type == "Blocked" & temp$theta == 0.88] <- (exp(HPDI(mu.block))*exp(0.88))[2]

temp$lower[temp$block_type == "Sine Wave" & temp$theta == 0.88] <- (exp(HPDI(mu.sine))*exp(0.88))[1]
temp$upper[temp$block_type == "Sine Wave" & temp$theta == 0.88] <- (exp(HPDI(mu.sine))*exp(0.88))[2]

temp$lower[temp$block_type == "Random" & temp$theta == 0.88] <- (exp(HPDI(mu.rand))*exp(0.88))[1]
temp$upper[temp$block_type == "Random" & temp$theta == 0.88] <- (exp(HPDI(mu.rand))*exp(0.88))[2]

model_lines <- temp
model_lines$avg <- (temp$lower + temp$upper)/2

plt <- ggplot(model_lines)
plt <- plt + geom_line(aes(y = avg,
                           x = theta,
                           colour = block_type))
plt <- plt + geom_ribbon(data = model_lines, 
                         aes(x = theta,
                             ymin = lower,
                             ymax = upper,
                             fill = block_type),
                         alpha = 0.3)
plt <- plt + ggtitle("Model output for Block Type treated as a fixed effect") +
             labs(x = "Theta",
                  y = "RT",
                  colour = "Block Type",
                  fill = "Block Type")

##################################################################################################
#### Version with varying slopes for block_type ####
# Don't think it works this way #
# load in model 
load("scratch/models/m_ta_only_temp_4")

# extract samples 
post <- extract.samples(m_ta_only_temp_4)

#### everything below this kind of works a little bit... but not really ####
# get 95% HDPI for block_type
intercepts_bl <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a), quantile, probs = c(0.025, 0.975)))))
intercepts_si <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a + post$b_issi), quantile, probs = c(0.025, 0.975)))))
intercepts_ra <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a + post$b_isra), quantile, probs = c(0.025, 0.975)))))

# Get exp before creating data set 
# blocked
intercepts_bl[,1] <- exp(intercepts_bl[,1] + mean(post$a))
intercepts_bl[,2] <- exp(intercepts_bl[,2] + mean(post$a))
# random
intercepts_ra[,1] <- exp(intercepts_ra[,1] + mean(post$a + post$b_isra))
intercepts_ra[,2] <- exp(intercepts_ra[,2] + mean(post$a + post$b_isra))
#sinewave
intercepts_si[,1] <- exp(intercepts_si[,1] + mean(post$a + post$b_issi))
intercepts_si[,2] <- exp(intercepts_si[,2] + mean(post$a + post$b_issi))


intercepts_block_type <- rbind(intercepts_ra, intercepts_bl, intercepts_si)
names(intercepts_block_type) <- c("lower", "upper")
intercepts_block_type$block_type <- c("random","blocked","sinewave")

# 1 = ra, 2 = bl, 3 = si
# tidy 
rm(intercepts_bl, intercepts_ra, intercepts_si)

#### Try a dumb way ####
# this gives the estimates for the intercepts in the different block_types
mu.block <- post$a
mu.sine <- post$a + post$b_issi
mu.rand <- post$a + post$b_isra
mu.theta_bl <- post$b_theta
mu.theta_ra <- post$b_theta + post$b_ra_theta
mu.theta_si <- post$b_theta + post$b_si_theta



#precis(data.frame(mu.block,mu.sine, mu.rand)) 


# this should be a manual way of doing the link part to get the HPDI's
temp <- list(block_type = rep(c("Blocked", "Sine Wave", "Random"), each = 2),
             theta =   rep(c(0.12, 0.88), length(1:3)))

#### this way is very long winded... using link must be quicker but I can't figure that out
# small theta
temp$lower[temp$block_type == "Blocked" & temp$theta == 0.12] <- (exp(HPDI(mu.block))*exp(0.12))[1]
temp$upper[temp$block_type == "Blocked" & temp$theta == 0.12] <- (exp(HPDI(mu.block))*exp(0.12))[2]

temp$lower[temp$block_type == "Sine Wave" & temp$theta == 0.12] <- (exp(HPDI(mu.sine))*exp(0.12))[1]
temp$upper[temp$block_type == "Sine Wave" & temp$theta == 0.12] <- (exp(HPDI(mu.sine))*exp(0.12))[2]

temp$lower[temp$block_type == "Random" & temp$theta == 0.12] <- (exp(HPDI(mu.rand))*exp(0.12))[1]
temp$upper[temp$block_type == "Random" & temp$theta == 0.12] <- (exp(HPDI(mu.rand))*exp(0.12))[2]

# big theta
temp$lower[temp$block_type == "Blocked" & temp$theta == 0.88] <- (exp(HPDI(mu.block))*exp(0.88))[1]
temp$upper[temp$block_type == "Blocked" & temp$theta == 0.88] <- (exp(HPDI(mu.block))*exp(0.88))[2]

temp$lower[temp$block_type == "Sine Wave" & temp$theta == 0.88] <- (exp(HPDI(mu.sine))*exp(0.88))[1]
temp$upper[temp$block_type == "Sine Wave" & temp$theta == 0.88] <- (exp(HPDI(mu.sine))*exp(0.88))[2]

temp$lower[temp$block_type == "Random" & temp$theta == 0.88] <- (exp(HPDI(mu.rand))*exp(0.88))[1]
temp$upper[temp$block_type == "Random" & temp$theta == 0.88] <- (exp(HPDI(mu.rand))*exp(0.88))[2]

model_lines <- temp
model_lines$avg <- (temp$lower + temp$upper)/2

plt <- ggplot(model_lines)
plt <- plt + geom_line(aes(y = avg,
                           x = theta,
                           colour = block_type))
plt <- plt + geom_ribbon(data = model_lines, 
                         aes(x = theta,
                             ymin = lower,
                             ymax = upper,
                             fill = block_type),
                         alpha = 0.3)
plt <- plt + ggtitle("Model output for Block Type treated as a fixed effect") +
  labs(x = "Theta",
       y = "RT",
       colour = "Block Type",
       fill = "Block Type")




