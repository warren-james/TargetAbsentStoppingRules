library(rethinking)
library(tidyverse)
library(scales)

#################################################################
# create functions 
#################################################################

# function that calls everything else!
process_model <- function(m, ln = TRUE, title_text = "") {

	precis(m)

	# extract samples from model
	post <- extract.samples(m)

	# plot posterior distributions for parameters!
	dens(post$a)
	dens(post$b_diff)
	dens(post$b_tp)

	# get HDPIs for regression lines and predictions
	model_lines <- get_hpdi_region_from_samples(m, post, ln)
	pred_lines <- get_prediction_region_from_samples(m)

	# plot predictions
	plot_model_simple(pred_lines, model_lines, title_text, ln)
} 

# get 97% credible interval for regression lines
get_hpdi_region_from_samples <- function(m, post, ln = TRUE) {
	
	pred_data <- data.frame(
		theta = rep(c(0,1), 2),
		targ_pr = rep(0:1, each = 2))

	mu <- link(m, data = pred_data)
	mu.PI <- apply(mu, 2, PI)

	pred_data$lower <- mu.PI[1,]
	pred_data$upper <- mu.PI[2,]

	pred_data$targ_pr <- as.factor(pred_data$targ_pr)
	levels(pred_data$targ_pr) <- c("absent", "present")

	if (ln == TRUE) { 
		pred_data$lower <- exp(pred_data$lower)
		pred_data$upper <- exp(pred_data$upper)
	}


	return(pred_data)
}

# sample a load of points from the model so we can get a feel for what it predicts
get_prediction_region_from_samples <- function(m) {
	pred_ta = sim(m, data = list(theta = c(0, 1), targ_pr = 0))
	pred_tp = sim(m, data = list(theta = c(0, 1), targ_pr = 1))

	# get 97% HDPI
	ta_PI = apply(pred_ta, 2, PI, prob = 0.97)
	tp_PI = apply(pred_tp, 2, PI, prob = 0.97)

	pred_lines = data.frame(
		theta = rep(c(0,1), 2),
		targ_pr = rep(c("absent", "present"), each = 2),
		rbind(t(ta_PI), t(tp_PI)))
	names(pred_lines)[3:4] = c("lower", "upper")

	pred_lines$targ_pr <- as.factor(pred_lines$targ_pr)
	levels(pred_lines$targ_pr) <- c("absent", "present")
	return(pred_lines)		
}

# plot model against empirical data
# simple rt ~ theta for ta and tp
plot_model_simple <- function(pred_lines, model_lines, title_text, lt) {
	plt <-  ggplot()	
	# add prediction range
	plt <- plt + geom_ribbon(data = pred_lines, 
		aes(
		x = theta, 
		ymin = lower,
		ymax = upper,
		fill = targ_pr),
		alpha = 0.5)
	# add model fit
	plt <- plt + geom_ribbon(data = model_lines, 
		aes(x = theta, ymin = lower, ymax = upper, fill = targ_pr))
	# add empirical data points
	plt <- plt + geom_jitter(data = df_correct_only, 
		aes(x = theta, y = rt, colour = as.factor(targ_pr)),
		shape = 3, alpha = 0.2, show.legend = FALSE) 
	# spec theme
	plt <- plt + scale_x_continuous("search difficulty", 
		limits = c(0, 1), expand = c(0, 0))
	if (lt == TRUE) {
		plt = plt + scale_y_continuous("reaction time", trans = log2_trans())
	}
	plt <- plt + scale_fill_discrete(name = "target present")
	plt <- plt + ggtitle(title_text)
	plt <- plt + theme_bw()

	plt
}
#################################################################

# load in data 
load("scratch/processed_data_nar.rda")

#################################################################
# plot simple models
#################################################################


# model 1 - getting started
load("scratch/models/m_tp_diff_1")
process_model(m_tp_diff_1, FALSE, "very wrong")


# model 2 - using log-normal distribution
load("scratch/models/m_tp_diff_2")
process_model(m_tp_diff_2, TRUE, "use log-normal distribution")


# model 3 - adding an interaction
load("scratch/models/m_tp_diff_3")
process_model(m_tp_diff_3, TRUE, "allow interaction")

#################################################################
# compare these simple models using IC
#################################################################

compare(m_tp_diff_3, m_tp_diff_2)

#################################################################
# model 4 - thinking about our priors
#################################################################

load("scratch/models/m_tp_diff_4")
process_model(m_tp_diff_4, TRUE, "switch to better priors")
