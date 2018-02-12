# Housekeeping
rm(list = ls())
setwd("~/Documents/sirpi/training /jan2018_datascience_workshop/code")
library(tidyverse)

View(iris)

iris %>% 
  View()

iris %>% 
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width)) + 
  geom_point(size = 4)
iris %>% 
  str()
iris %>% 
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width, 
             colour = Species)) + 
  geom_point(size = 2)

# Scatterplot exercise
# 2 - variable plot (numeric vs numeric)

# Look for high performers, low performers, average performers, and possibly 1 CEO of the company
# Look for trends, correlations
# Identify bounds of the variables
# Give fixed size and alpha to check overplotting!

# Basic scatter plot
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

# View and explore your data set
mtcars %>% 
  View()

# Converting rowname into a column 
mtcars %>% 
  rownames_to_column() %>% 
  rename(carname = rowname) -> mycars

# Display in console
mycars

# Top 6 lines
mycars %>% 
  head()

# Bottom 6 lines
mycars %>% 
  tail()


# Filter rows
mycars %>% 
  filter(gear == 3) %>%
  View()


mycars %>% 
  filter(gear == 3 & mpg > 15) %>%
  View()

# Select columns
mycars %>% 
  select(mpg) %>%
  View()

# Select multiple columns 
mycars %>% 
  select(carname, cyl, gear) %>%
  View()

mycars %>% 
  select(starts_with("mp")) %>%
  View()

variable <- c("gear","hp","disp")
mycars %>% 
  select(one_of(variable)) %>%
  View()

mycars %>% 
  select(c(1,3,5:8))

mycars %>% 
  select(-c(3,5:8))


mycars %>% 
  arrange(mpg) %>% 
  View()

mycars %>% 
  arrange(desc(mpg)) %>% 
  View()

# Which car has the best mileage among four cyl cars ?
mycars %>% 
  filter(cyl == 4) %>%
  arrange(desc(mpg)) %>% 
  head(n=1) 

mycars %>%
  filter(cyl == 4 & mpg == max(mpg))

# Best and worst 4 cyl car in terms of mileage  
mycars %>% 
  filter(cyl == 4) %>%
  filter(mpg == max(mpg) | mpg == min(mpg))

# Plot columns
mycars %>% 
  ggplot(aes(x = am,
             y = mpg)) + 
  geom_point()

# Make x- axis categorical
mycars %>% 
  ggplot(aes(x = factor(am), 
             y = mpg)) + 
  geom_point()

# Change order of factors levels
mycars %>% 
  ggplot(aes(x = factor(am,
                        levels = c("1","0")), 
             y = mpg)) + 
  geom_point()

# Change labels
mycars %>% 
  ggplot(aes(x = factor(am,
                        levels = c("1","0"),
                        labels = c("manual","auto")), 
             y = mpg)) + 
  geom_point()

# Modify axes labels point sizes
mycars %>% 
  ggplot(aes(x = factor(am,
                        levels = c("1","0"),
                        labels = c("manual","auto")), 
             y = mpg)) + 
  geom_point(size = 3) + 
  labs(title = "Miles per gallon vs Transmission",
       x = "Transmission",
       y  = "Miles per gallon",
       subtitle = "From mtcars dataset",
       caption = "1 gallon = 3.8 litres")

# Add jitterring
mycars %>% 
  ggplot(aes(x = factor(am,
                        levels = c("1","0"),
                        labels = c("manual","auto")), 
             y = mpg)) + 
  geom_jitter(size = 3, position = position_jitter(w = 0.1, h = 0)) + 
  labs(title = "Miles per gallon vs Transmission",
       x = "Transmission",
       y  = "Miles per gallon",
       subtitle = "From mtcars dataset",
       caption = "1 gallon = 3.8 litres")

# Add a box plot
mycars %>% 
  ggplot(aes(x = factor(am,
                        levels = c("1","0"),
                        labels = c("manual","auto")), 
             y = mpg)) + 
  geom_boxplot() + 
  geom_jitter(size = 3, position = position_jitter(w = 0.1, h = 0)) + 
  labs(title = "Miles per gallon vs Transmission",
       x = "Transmission",
       y  = "Miles per gallon",
       subtitle = "From mtcars dataset",
       caption = "1 gallon = 3.8 litres")


# Add colors
mycars %>% 
  ggplot(aes(x = factor(am,
                        levels = c("1","0"),
                        labels = c("manual","auto")), 
             y = mpg)) + 
  geom_boxplot() + 
  geom_jitter(size = 3, 
              position = position_jitter(w = 0.1, h = 0), 
              aes(colour = cyl %>% factor())) + 
  labs(title = "Miles per gallon vs Transmission",
       x = "Transmission",
       y  = "Miles per gallon",
       subtitle = "From mtcars dataset",
       caption = "1 gallon = 3.8 litres",
       colour = "Cylinders")

# Acc due to gravity (tidy data)

# Grouping and statistics & faceting

# Functional Programming and avoiding for loops 

# Animations


