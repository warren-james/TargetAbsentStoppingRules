library(rethinking)
library(tidyverse)
library(scales)
library(ggthemes)

#################################################################
# create functions 
#################################################################

# show random intercepts


#################################################################

# load in data 
load("scratch/processed_data_nar.rda")

#################################################################
# model 1 - getting started
#################################################################

load("scratch/models/m_tp_diff_mixed_1")
precis(m_tp_diff_1)

# extract samples from model
post <- extract.samples(m_tp_diff_1)

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

plt <- ggplot()
plt <- plt + geom_ribbon(aes(
		x = 0:44,
		ymin = participant_range[1], ymax = participant_range[2]),
	alpha = 0.6, fill = "orangered3")
plt <- plt + geom_ribbon(aes(
		x = 0:44,
		ymin = exp(quantile(post$a, 0.025)),
		ymax = exp(quantile(post$a, 0.975))), 
	alpha = 1.0, fill = "steelblue1")
plt <- plt + geom_errorbar(
	data = intercepts_p, 
	aes(x = participant, ymin = lower, ymax = upper))
plt <- plt + scale_x_continuous("participant", expand = c(0,0))
plt <- plt + scale_y_continuous(
	"reaction times (seconds)", 
	trans = log2_trans(),
	limits = c(1, 6))
plt <- plt + theme_bw() + scale_fill_ptol()
plt



#################################################################
# model 2 - getting started
# now we have some slopes... plotting is getting more complicated
#################################################################

load("scratch/models/m_tp_diff_mixed_2")
precis(m_tp_diff_2)

# extract samples from model
post <- extract.samples(m_tp_diff_2)


