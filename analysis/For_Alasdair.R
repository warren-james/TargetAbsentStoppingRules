# Make some datafiles and plots for Alasdair 
library(tidyverse)

# load data 
load("scratch/processed_data.rda")

# process data 
df <- df %>% 
  mutate(targ_pr = ifelse(targ_pr == 1, "Present", "Absent")) %>% 
  group_by(participant, block) %>%
  mutate(p_targ_pr = lag(targ_pr)) %>%
  drop_na() %>% 
  filter(site == "Aberdeen") 

# plots 
df %>%
  filter(targ_pr == "Absent",
         rt <=10,
         rt >= .3,
         p_rt <= 10,
         p_rt >= .3) %>%
  ggplot(aes(rt, p_rt,
             colour = block_type)) +
  geom_point(alpha = .1) +
  geom_smooth(method = glm) +
  facet_wrap(~difficulty)

df %>% 
  filter(rt <= 10,
         difficulty < 2.1) %>%
  group_by(participant, block_type, difficulty, targ_pr) %>% 
  summarise(rt = median(rt)) %>% 
  ungroup() %>% 
  mutate(targ_pr = as.factor(targ_pr),
         difficulty = round(difficulty, digits = 3)) %>%
  ggplot(aes(rt, colour = targ_pr)) +
  geom_density(alpha = .3) + 
  facet_grid(block_type ~ difficulty)
  
df %>% 
  filter(rt <= 10,
         p_rt <= 10,
         rt >= .3,
         p_rt >= .3,
         correct == 1) %>%
  mutate(rt_change = rt-p_rt,
         present = ifelse(targ_pr == 1, "Present", "Absent"),
         p_targ_pr = paste("prev_", p_targ_pr)) %>% 
  ggplot(aes(rt_change,
             colour = block_type,
             fill = block_type)) + 
  geom_density(alpha = .3) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  # facet_wrap(~targ_pr) + 
  facet_grid(p_targ_pr ~ targ_pr) +
  see::scale_color_flat() + 
  see::scale_fill_flat()

  
df %>% 
  drop_na() %>%
  filter(rt <= 10) %>%
  mutate(p_correct = as.factor(p_correct)) %>%
  ggplot(aes(rt,
             colour = p_correct,
             fill = p_correct)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  facet_grid(block_type ~ targ_pr)
  
# df %>% 
#   filter(block_type != "blocked",
#          rt <= 10,
#          rt >= .3) %>%
#   mutate(diff_change = difficulty - p_difficulty) %>% 
#   ggplot(aes(diff_change, rt,
#              colour = block_type)) + 
#   geom_point() + 
#   facet_grid(block_type ~ targ_pr)
  




    