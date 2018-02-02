library(rethinking)
library(tidyverse)
library(scales)
library(ggthemes)

#################################################################
# create functions 
#################################################################

# get 97% credible interval for regression lines
get_hpdi_region_from_samples <- function(m, post, ln = TRUE) {
  
  pred_data <- list(
    participant = rep(1:length(unique(df$participant)), each = 4),
    theta =   rep(c(0.12, 0.88, 0.12, 0.88), length(unique(df$participant))))#,
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
  plt <- plt + scale_y_continuous(name = "reaction time", trans = log2_trans())#, limits = c(0.5, 32))
  #plt <- plt + coord_cartesian(ylim = c(0,60), xlim = c(0,1))
  #plt <- plt + scale_fill_discrete(name = "target present")
  plt <- plt + ggtitle(title_text)
  plt <- plt + theme_bw()
  plt <- plt + facet_wrap( ~ participant)
  ggsave("scratch/random_incpt_facet.pdf", width = 10, height = 10)
}

# to "fix" the way rethinking orders numbers
sequence <- c(1,10,11,12,13,14,15,16,17,18,19,2,20,21,22,23,24,25,26,27,
              28,29,3,30,31,32,33,34,35,36,37,38,39,4,40,41,42,43,44,45,
              46,47,48,49,5,50,6,7,8,9)

# load in data 
load("scratch/processed_data_nar_TA.rda")

# turn tibble into data.frame for map 
df <- as.data.frame(df_TA)

#################################################################
# model 1 - getting started
#################################################################

load("scratch/models/m_ta_only_1")
precis(m_ta_only_1)

# extract samples from model
post <- extract.samples(m_ta_only_1)

# get 95% HPDI for each person 
#  this is a horrible hack, what's a nicer way of doing this?
intercepts_p <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a_p), quantile, probs = c(0.025, 0.975)))))
names(intercepts_p) = c("lower", "upper")
intercepts_p$participant = c(sequence)
# this is a little incorrect... should do everything with samples, then estimate mean, ec
# will fix later!
intercepts_p$lower <- exp(intercepts_p$lower + mean(post$a))
intercepts_p$upper <- exp(intercepts_p$upper + mean(post$a))


# display range of intercepts, again this is probably not 100% correct 
# (but good enough for now?)
theta <- mean(post$sig_p)
mu <- mean(post$a)
participant_range <- c(
  qlnorm(mu, theta, 0.025), qlnorm(mu, theta, 0.975))

model_lines <- get_hpdi_region_from_samples(m_ta_only_1, post, TRUE)

# alter the participant variable in model_lines so it's right
model_lines$participant <- rep(sequence, each=4)

plot_model_mixed_facet(pred_lines, model_lines, "random intercepts", TRUE)


#################################################################
# model 2 - getting started
#################################################################

load("scratch/models/m_ta_only_2")
precis(m_ta_only_2)

# extract samples from model
post <- extract.samples(m_ta_only_2)

# get 95% HPDI for each person 
#  this is a horrible hack, what's a nicer way of doing this?
intercepts_p <- as.tibble(t(as.tibble(lapply(as.data.frame(post$a_p), quantile, probs = c(0.025, 0.975)))))
names(intercepts_p) = c("lower", "upper")
intercepts_p$participant = 1:nrow(intercepts_p)
# this is a little incorrect... should do everything with samples, then estimate mean, ec
# will fix later!
intercepts_p$lower <- exp(intercepts_p$lower + mean(post$a))
intercepts_p$upper <- exp(intercepts_p$upper + mean(post$a))


# display range of intercepts, again this is probably not 100% correct 
# (but good enough for now?)
theta <- mean(post$sig_p)
mu <- mean(post$a)
participant_range <- c(
  qlnorm(mu, theta, 0.025), qlnorm(mu, theta, 0.975))

model_lines <- get_hpdi_region_from_samples(m_ta_only_2, post, TRUE)

# alter the participant variable in model_lines so it's right
model_lines$participant <- rep(sequence, each=4)

plot_model_mixed_facet(pred_lines, model_lines, "random intercepts", TRUE)




########################################################################################
# below doesn't work just yet 
#### For model with varying intercepts of block type ####
df <- df_TA

load("scratch/models/m_ta_only_temp_2")
precis(m_ta_only_temp_2)

# extract samples from model
post <- extract.samples(m_ta_only_temp_2)

# get 97% credible interval for regression lines
get_hpdi_region_from_samples <- function(m, post, ln = TRUE) {
  
  pred_data <- list(
    participant = rep(1:length(unique(df$participant)), each = 6),
    theta =   rep(c(0.12, 0.88, 0.12, 0.88, 0.12, 0.88), length(unique(df$participant))),
    block_type_id = rep(c(1,1,2,2,3,3), length(unique(df$participant))))
  
  mu <- link(m, data = pred_data)
  mu.PI <- apply(as.data.frame(mu), 2, PI) # making it a data.frame seems to help?
  
  pred_data$lower <- mu.PI[1,]
  pred_data$upper <- mu.PI[2,]
  
  pred_data$block_type_id <- as.factor(pred_data$block_type_id)
  levels(pred_data$block_type_id) <- c("blocked", "random", "sinewave")
  
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
                           aes(x = theta,
                               ymin = lower,
                               ymax = upper,
                               fill = block_type_id),
                           alpha = 0.5)
  # add empirical data points
  plt <- plt + geom_jitter(data = df, 
                           aes(x = theta,
                               y = rt, 
                               colour = block_type_id),
                           shape = 3, alpha = 0.3, show.legend = FALSE) 
  # spec theme
  plt <- plt + scale_x_continuous("search difficulty", 
                                  limits = c(0, 1), expand = c(0, 0))
  plt <- plt + scale_y_continuous(name = "reaction time", trans = log2_trans())#, limits = c(0.5, 32))
  #plt <- plt + coord_cartesian(ylim = c(0,60), xlim = c(0,1))
  #plt <- plt + scale_fill_discrete(name = "target present")
  plt <- plt + ggtitle(title_text)
  plt <- plt + theme_bw()
  plt <- plt + facet_wrap( ~ participant)
  ggsave("scratch/random_incpt_facet.pdf", width = 10, height = 10)
}

model_lines <- get_hpdi_region_from_samples(m_ta_only_temp_2, post, TRUE)

# alter the participant variable in model_lines so it's right
model_lines$participant <- rep(sequence, each=6)

# prep data for plot 
df$block_type_id <- as.factor(df$block_type_id)

# make the plot
plot_model_mixed_facet(pred_lines, model_lines, "random intercepts", TRUE)

