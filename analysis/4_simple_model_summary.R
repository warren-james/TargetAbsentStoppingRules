library(rethinking)
library(tidyverse)
library(scales)

#################################################################
# create functions 
#################################################################

# get 97% credible interval for regression lines
get_hpdi_region_from_samples <- function(post, ln) {
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
				function(x) {y = quantile(
					post$a + post$b_tp + (post$b_diff + post$b_tp_diff) * x, 
					0.015)}),
			upper = sapply(c(0,1), 
				function(x) {y = quantile(
					post$a + post$b_tp + (post$b_diff + post$b_tp_diff) * x, 
					0.985)}),
			targ_pr = "present")
		)
	# if we're using a log-normal model, we have to transform regression lines!
	if (ln == TRUE) {
		model_lines$lower <- exp(model_lines$lower)
		model_lines$upper <- exp(model_lines$upper)
	}

	return(model_lines)
}

# sample a load of points from the model so we can get a feel for what it predicts
get_prediction_region_from_samples <- function(post, m) {
	pred_ta = sim(m, data = list(theta = seq(0,1, 0.1), targ_pr = 0))
	pred_tp = sim(m, data = list(theta = seq(0,1, 0.1), targ_pr = 1))

	# get 97% HDPI
	ta_PI = apply(pred_ta, 2, PI, prob = 0.97)
	tp_PI = apply(pred_tp, 2, PI, prob = 0.97)

	pred_lines = data.frame(
		theta = rep(seq(0,1,0.1), 2),
		targ_pr = rep(c("absent", "present"), each = 11),
		rbind(t(ta_PI), t(tp_PI)))
	names(pred_lines)[3:4] = c("lower", "upper")

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
	plt <- plt + scale_x_continuous("search difficulty", limits = c(0, 1))
	plt <- plt + scale_y_continuous("reaction time")
	if (lt == TRUE) {
		plt = plt + scale_y_continuous(trans = log2_trans())
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
# model 1 - getting started
#################################################################

load("scratch/models/m_tp_diff_1")
precis(m_tp_diff_1)

# extract samples from model
post <- extract.samples(m_tp_diff_1)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)

# get HDPIs for regression lines and predictions
model_lines <- get_hpdi_region_from_samples(post, ln = FALSE)
pred_lines <- get_prediction_region_from_samples(post, m_tp_diff_1)

# plot predictions
plot_model_simple(pred_lines, model_lines, 'normal and crap', FALSE)

#################################################################
# model 2 - using log-normal distribution
#################################################################

load("scratch/models/m_tp_diff_2")
precis(m_tp_diff_2)

#### extract samples from model ####
post <- extract.samples(m_tp_diff_2)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)

model_lines <- get_hpdi_region_from_samples(post, ln = TRUE)
pred_lines <- get_prediction_region_from_samples(post, m_tp_diff_2)

# plot predictions
plot_model_simple(pred_lines, model_lines, 'log-normal and crap', TRUE)


#################################################################
# model 3 - adding an interaction
#################################################################

load("scratch/models/m_tp_diff_3")
precis(m_tp_diff_3)

# extract samples from model
post <- extract.samples(m_tp_diff_3)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)

# get HDPIs for regression lines and predictions
model_lines <- get_hpdi_region_from_samples(post, ln = TRUE)
pred_lines <- get_prediction_region_from_samples(post, m_tp_diff_3)

# plot predictions
plot_model_simple(pred_lines, model_lines, 'log-normal with interaction, still crap', TRUE)

#################################################################
# compare these simple models using IC
#################################################################

compare(m_tp_diff_3, m_tp_diff_2)

#################################################################
# model 4 - thinking about our priors
#################################################################

load("scratch/models/m_tp_diff_4")
precis(m_tp_diff_4)

# extract samples from model
post <- extract.samples(m_tp_diff_4)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)

# get HDPIs for regression lines and predictions
model_lines <- get_hpdi_region_from_samples(post, ln = TRUE)
pred_lines <- get_prediction_region_from_samples(post, m_tp_diff_4)

# plot predictions
plot_model_simple(pred_lines, model_lines, 'log-normal with interaction, still crap, better priors', TRUE)
