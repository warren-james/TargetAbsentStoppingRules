library(rethinking)
library(tidyverse)

# load in data
load("scratch/processed_data_nar.rda")

# load first model
load("scratch/models/m_tp_diff_1")

# load  first model
precis(m_tp_diff_1)

# extract samples from model
post <- extract.samples(m_tp_diff_1)

# plot posterior distributions for parameters!
dens(post$a)
dens(post$b_diff)
dens(post$b_tp)

# plot best fit lines with data
plt <- ggplot(df_correct_only, 
	aes(x = difficulty, y = rt, colour = as.factor(targ_pr)))
plt <- plt + geom_jitter() 
# add model fit
plt <- plt + geom_abline(intercept = mean(post$a), slope = mean(post$b_diff), colour = "red")
plt <- plt + geom_abline(intercept = mean(post$a + post$b_tp) , slope = mean(post$b_diff), colour = "blue")
# spec theme
plt <- plt + scale_x_continuous(limits = c(0, pi))
plt <- plt + theme_bw()
plt

# get 97% credible interval for regression lines
model_lines <- rbind(
	data.frame(
		difficulty = c(0, 1),
		lower = sapply(c(0,1), 
			function(x) {y = quantile(post$a + post$b_diff * x, 0.015)}),
		upper = sapply(c(0,1), 
			function(x) {y = quantile(post$a + post$b_diff * x, 0.985)}),
		targ_pr = "absent"),
	data.frame(
		difficulty = c(0, 1),
		lower = sapply(c(0,1), 
			function(x) {y = quantile(post$a + post$b_tp + post$b_diff * x, 0.015)}),
		upper = sapply(c(0,1), 
			function(x) {y = quantile(post$a + post$b_tp + post$b_diff * x, 0.985)}),
		targ_pr = "present")
	)

# sample a load of points from the model so we can get a feel for what it predicts
pred_ta = sim(m_tp_diff_1, data = list(difficulty = seq(0,1, 0.1), targ_pr = 0))
pred_tp = sim(m_tp_diff_1, data = list(difficulty = seq(0,1, 0.1), targ_pr = 1))

# get 97% HDPI
ta_PI = apply(pred_ta, 2, PI, prob = 0.97)
tp_PI = apply(pred_tp, 2, PI, prob = 0.97)

pred_lines = data.frame(
	difficulty = rep(seq(0,1,0.1),2),
	targ_pr = rep(c("absent", "present"), each = 11),
	rbind(t(ta_PI), t(tp_PI)))
names(pred_lines)[3:4] = c("lower", "upper")


plt <-  ggplot()
plt <- plt + geom_jitter(data = df_correct_only, 
	aes(x = difficulty, y = rt, colour = as.factor(targ_pr))) 
# add model fit
plt <- plt + geom_ribbon(data= model_lines, aes(x = difficulty, ymin = lower, ymax = upper, fill = targ_pr))
# add prediction range
plt <- plt + geom_ribbon(data = pred_lines, aes(
	x = pred_lines$difficulty, 
	ymin = pred_lines$lower,
	ymax = pred_lines$upper,
	fill = pred_lines$targ),
	alpha = 0.5)
# spec theme
plt <- plt + scale_x_continuous("search difficulty", limits = c(0, pi))
# plt <- plt + scale_y_continuous("reaction time", limits = c(0, 10))
plt <- plt + theme_bw()
plt
