# Clean slate
rm(list = ls())

#Set working directory
setwd("~/Documents/sirpi/training /jan2018_datascience_workshop/code")

# Load libraries
library(tidyverse)

mtcars  %>% 
  group_by(cyl,am)  %>%
  summarise(mean_mpg = mpg %>% mean(),
            count = n()) %>%
  View()

ggplot() +
  geom_jitter(data = mtcars,
              aes(x = factor(cyl), 
                  y = mpg, 
                  color = factor(cyl)), 
              position = position_jitter(w = 0.1, h = 0),
              alpha = 0.4)  + 
  geom_point(data = mtcars  %>% 
               group_by(cyl) %>% 
               summarise(mean_mpg = mean(mpg)),
             aes(x = factor(cyl), 
                 y = mean_mpg,
                 color = factor(cyl)), 
             size = 10, alpha = 0.1) +
  geom_text(data = mtcars  %>% 
              group_by(cyl) %>% 
              summarise(mean_mpg = mean(mpg)), 
            aes(x = factor(cyl), 
                y = mean_mpg, 
                label = mean_mpg %>% round(1), 
                color = factor(cyl)),
            size = 5) +
  labs(x = "No. of cylinders",
       title = "Miles per gallon vs cylinders",
       y = "Miles per gallon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(legend.position = "none") -> p1

"data_with_stats.pdf" %>% ggsave(p1, width = 8, height = 11, units = "in")