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

# plot range of fits

# for ta, lower line is
model_lines <- rbind(
	data.frame(
	x = c(0, 1),
	lower = sapply(c(0,1), 
		function(x) {y = quantile(post$a + post$b_diff * x, 0.05)}),
	upper = sapply(c(0,1), 
		function(x) {y = quantile(post$a + post$b_diff * x, 0.95)}),
	targ = "absent"),
	data.frame(
	x = c(0, 1),
	lower = sapply(c(0,1), 
		function(x) {y = quantile(post$a + post$b_tp + post$b_diff * x, 0.05)}),
	upper = sapply(c(0,1), 
		function(x) {y = quantile(post$a + post$b_tp + post$b_diff * x, 0.95)}),
	targ = "present"))



plt <- ggplot(model_lines, aes(x = x, fill = targ))
# add model fit
plt <- plt + geom_ribbon(aes(ymin = lower, ymax = upper))

# spec theme
plt <- plt + scale_x_continuous("search difficulty", limits = c(0, pi))
plt <- plt + scale_y_continuous("reaction time", limits = c(0, 10))
plt <- plt + theme_bw()
plt
