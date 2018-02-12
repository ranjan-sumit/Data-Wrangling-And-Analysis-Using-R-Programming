# Scatterplot exercise
# 2 - variable plot (numeric vs numeric)

# Look for high performers, low performers, average performers, and possibly 1 CEO of the company
# Look for trends, correlations
# Identify bounds of the variables
# Give fixed size and alpha to check overplotting!
# Basic scatter plot
#Salary
rm(list = ls()) 
setwd("E:/SIRPI Workshop/Day 1")

library(tidyverse)

"salary_data.csv" %>% 
  read_csv() %>% 
  ggplot(aes(x = experience, y = salary)) + 
  geom_point() 

# Add titles and change theme
"salary_data.csv" %>% 
  read_csv() %>%
  ggplot(aes(x = experience, y = salary)) + 
  geom_point() + 
  labs(title = "Employee Salary vs Experience",
       x = "Experience in years",
       y = "Salary in lakhs per annum") +
  theme_minimal()

# Add Transparency and point size
"salary_data.csv" %>% 
  read_csv() %>% 
  ggplot(aes(x = experience,
             y = salary))  +
  geom_point(size = 5, 
             alpha = 0.5) + 
  labs(title = "Employee Salary vs Experience",
       x = "Experience in years",
       y = "Salary in lakhs per annum") +
  theme_minimal()