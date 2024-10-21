library(tidyverse)
setwd("~/madison/phd/Semesters/03_Fall2024/BMI881/Homework1")

data <- read_csv('data/feigin2014_table1_mortality.csv')
data

data %>% 
  group_by(year, age_group) %>% 
  summarise(across(where(is.numeric), ~mean(.))) %>% 
  ungroup %>% 
  ggplot(aes(x = factor(year), y = mortality_rate)) + 
  geom_line(aes(color= age_group, group = age_group))

data %>% 
  mutate(age_group = factor(age_group, levels = c('all', '<75', '>=75'))) %>% 
  ggplot(aes(x = factor(year), y = mortality_rate)) + 
  geom_ribbon(aes(ymin = interval_low, ymax=interval_high, group = income_group, fill = income_group), alpha = 0.35, show.legend = F) + 
  geom_line(aes(color= income_group, group = income_group), size=1, show.legend = F) + 
  geom_point(aes(color = income_group), size = 2.5)+
  facet_wrap(~age_group, scales = 'free') + 
  theme_bw() + 
  xlab(NULL) + 
  ylab('Mortality Rate') + 
  scale_color_discrete('Income Group', labels = c("All", "High", 'Low and Middle'))
  

ggsave('Figure1.jpg', width = 2500, height = 1250, units = 'px')
