#IRIS

rm(list = ls()) 
setwd("E:/SIRPI Workshop/Day 1")

library(tidyverse)

View(iris) 

iris %>%
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point(size=4)

iris %>%
  str()
iris %>%
  ggplot(aes(x= Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point(size=4)
