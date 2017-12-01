library(rethinking)
library(tidyverse)
library(scales)

get_hpdi_region_from_samples <- function(post) {
	#### get 97% CIs ####
	# get 97% credible interval for regression lines
	model_lines <- rbind(
		data.frame(
			theta = c(0, 1),
			lower = sapply(c(0,1), 
				function(x) {y = quantile(post$a + post$b_diff * x, 0.015)}),
			upper = sapply(c(0,1), 
				function(x) {y = quantile(post$a + post$b_diff * x, 0.985)}),
			targ_pr = "absent"),
		data.frame(
			theta = c(0, 1),
			lower = sapply(c(0,1), 
				function(x) {y = quantile(post$a + post$b_tp + post$b_diff * x, 0.015)}),
			upper = sapply(c(0,1), 
				function(x) {y = quantile(post$a + post$b_tp + post$b_diff * x, 0.985)}),
			targ_pr = "present")
		)

	return(model_lines)
}

get_prediction_region_from_samples <- function(post, m) {
	# sample a load of points from the model so we can get a feel for what it predicts
	pred_ta = sim(m, data = list(theta = seq(0,1, 0.1), targ_pr = 0))
	pred_tp = sim(m, data = list(theta = seq(0,1, 0.1), targ_pr = 1))

	# get 97% HDPI
	ta_PI = apply(pred_ta, 2, PI, prob = 0.97)
	tp_PI = apply(pred_tp, 2, PI, prob = 0.97)

	pred_lines = data.frame(
		theta = rep(seq(0,1,0.1),2),
		targ_pr = rep(c("absent", "present"), each = 11),
		rbind(t(ta_PI), t(tp_PI)))
	names(pred_lines)[3:4] = c("lower", "upper")

	return(pred_lines)		
}


#### load in data ####
load("scratch/processed_data_nar.rda")

# load model
load("scratch/models/m_tp_diff_1")

# load  first model
precis(m_tp_diff_1)

#### MODEL 1 ####
#### extract samples from model ####
post <- extract.samples(m_tp_diff_1)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)


model_lines <- get_hpdi_region_from_samples(post)
pred_lines <- get_prediction_region_from_samples(post, m_tp_diff_1)


#### plot predictions ####
plt <-  ggplot()
plt <- plt + geom_jitter(data = df_correct_only, 
	aes(x = theta, y = rt, colour = as.factor(targ_pr))) 
# add model fit
plt <- plt + geom_ribbon(data= model_lines, aes(x = s_theta, ymin = lower, ymax = upper, fill = targ_pr))
# add prediction range
plt <- plt + geom_ribbon(data = pred_lines, aes(
	x = pred_lines$s_theta, 
	ymin = pred_lines$lower,
	ymax = pred_lines$upper,
	fill = pred_lines$targ),
	alpha = 0.5)
# spec theme
plt <- plt + scale_x_continuous("search difficulty", limits = c(0, 1))
# plt <- plt + scale_y_continuous("reaction time", limits = c(0, 10))
plt <- plt + theme_bw()
plt


#### MODEL 2 ####


# load model
load("scratch/models/m_tp_diff_2")

# load  first model
precis(m_tp_diff_2)

#### MODEL 1 ####
#### extract samples from model ####
post <- extract.samples(m_tp_diff_2)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)



model_lines <- get_hpdi_region_from_samples(post)
pred_lines <- get_prediction_region_from_samples(post, m_tp_diff_2)


#### plot predictions ####
plt <-  ggplot()
plt <- plt + geom_jitter(data = df_correct_only, 
	aes(x = theta, y = rt, colour = as.factor(targ_pr)), alpha =.7) 
# add model fit
plt <- plt + geom_ribbon(data = model_lines, aes(
	x = theta, 
	ymin = lower, 
	ymax = upper, 
	fill = targ_pr))
# add prediction range
plt <- plt + geom_ribbon(data = pred_lines, aes(
	x = pred_lines$theta, 
	ymin = pred_lines$lower,
	ymax = pred_lines$upper,
	fill = pred_lines$targ),
	alpha = 0.25)
# spec theme
plt <- plt + scale_x_continuous("search difficulty", limits = c(0, 1))
plt <- plt + scale_y_continuous("reaction time", trans = log_trans())
plt <- plt + theme_bw()
plt

