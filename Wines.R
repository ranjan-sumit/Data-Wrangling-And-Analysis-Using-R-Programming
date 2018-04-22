#Setting up directory

setwd("C:/Users/sumit/Downloads/Case_Onsite_Modeling_Wine")

Wines <- read.csv("wine_dataset.csv")

#Checking the data

dim(Wines)

head(Wines)

tail(Wines)

names(Wines)

summary(Wines)

str(Wines)

#Installing packages
#install.packages("ggplot2")
#install.packages("GGally")

#importing Library
library(ggplot2)
library(GGally)

#Observations from the Summary :

#Mean residual sugar level is 5.4 g/l, but there is a sample of very sweet wine with 65.8 g/l (an outlier). Mean free sulfur dioxide is 30.5 ppm. Max value is 289 which is quite high as 75% is 41 ppm. PH of wine is within range from 2.7 till 4, mean 3.2. There is no basic wines in this dataset (no high pH levels). Alcohol: lightest wine is 8%, strongest is 14.9. Minimum quality mark is 3, mean 5.8, highest is 9.

#Understand the Distribution of Single Variables

#Quality of Wine

summary(Wines$quality)

table(Wines$quality)

ggpairs(Wines)

qplot(x = quality, data = Wines)

summary(Wines$quality)

#Transforming Quality from an Integer to a Factor
Wines$quality <- factor(Wines$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

Wines$rating <- ifelse(Wines$quality < 5, 'bad', ifelse(
  Wines$quality < 8, 'average', 'good'))

Wines$rating <- ordered(Wines$rating,
                      levels = c('bad', 'average', 'good'))

summary(Wines$rating)

#Univariate Analysis

#install.packages("gridExtra")

library(gridExtra)

p1 <- qplot(x = pH, data = Wines)
p2 <- qplot(x = density, data = Wines)
p3 <- qplot(x = citric_acid, data = Wines)
p4 <- qplot(x = fixed_acidity, data = Wines)
p5 <- qplot(x = total_sulfur_dioxide, data = Wines)
p6 <- qplot(x = citric_acid, data = Wines)
p7 <- qplot(x = free_sulfur_dioxide, data = Wines)
p8 <- qplot(x = volatile_acidity, data = Wines)
p9 <- qplot(x = sulphates, data = Wines)
p10 <- qplot(x = residual_sugar, data = Wines)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)


#Bivariate analysis

#Analysis on the acidic concentration of the alchohol

p1 <- ggplot(aes(x = rating, y = pH), data = Wines) + geom_boxplot() + stat_summary(fun.y = "mean", 
                                                                                  geom = "point", 
                                                                                  color = "red", 
                                                                                  shape = 8, 
                                                                                  size = 4)

p2 <- ggplot(aes(x = quality, y = pH), data = Wines) + geom_boxplot() + stat_summary(fun.y = "mean", 
                                                                                   geom = "point", 
                                                                                   color = "red", 
                                                                                   shape = 8, 
                                                                                   size = 4)

p1

p2

#Fixed Acidity Analysis

ggplot(data = Wines, aes(x = quality, y = fixed_acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Analysis of Volatile Acidity

ggplot(data=Wines, aes(x = quality, y = volatile_acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

# Analysis of citric acid

ggplot(data=Wines, aes(x=quality, y=citric_acid)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,2)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

ggplot(data=Wines, aes(x=rating, y=citric_acid)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,2)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

# Analysis on residual sugar

ggplot(data=Wines, aes(x=quality, y=residual_sugar)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0.75,3)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

# Analysis on chlorides

ggplot(data=Wines, aes(x=quality, y=chlorides)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .25,color = 'blue') +
  scale_y_continuous(lim = c(0,0.20))

stat_summary(fun.y = "mean", 
             geom = "point", 
             color = "red", 
             shape = 8, 
             size = 9)

#Analysis of sulphates

ggplot(data=Wines, aes(x=quality, y=sulphates)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0.25,1)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Analysis of Alcohol

ggplot(data=Wines, aes(x=quality, y=alcohol)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Analysis of Chlorides

ggplot(data=Wines, aes(x=quality, y=chlorides)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0, 0.2)) + 
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Analysis of Sulpher_dioxide

ggplot(data=Wines, aes(x=quality, y=free_sulfur_dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,40)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Analysis of total_Sulpher_dioxide

ggplot(data=Wines, aes(x=quality, y=total_sulfur_dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,150)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Analysis of density

ggplot(data=Wines, aes(x=quality, y=density)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

#Multivariate Analysis of the data

#density with alchohol with quality a

ggplot(data = Wines,
       aes(y = density, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

# Volatile Acidity and alchohol density ratio

ggplot(aes(y = volatile_acidity, x = alcohol/density),
       data = Wines) +
  labs(y = 'Volatile Acidity', x = 'Alchohol/Density Ratio',
       title = 'Volatile Acidity and Alcohol Density ratio plot') +
  geom_point(aes(color = rating), position = 'jitter',
             alpha = 0.5) +
  scale_color_brewer(type = 'qual') 

#Sulphates vs alcohol density ratio

ggplot(aes(y = sulphates, x = alcohol/density),
       data = Wines) +
  labs(y = 'Sulphates', x = 'Alcohol/Density Ratio',
       title = 'Sulphates vs Alcohol Density ratio') +
  geom_point(aes(color = rating), position = 'jitter',
             alpha = 0.5) +
  scale_color_brewer(type = 'qual') 

#Citric Acid vs alcohol density ratio

ggplot(aes(y = citric.acid, x = alcohol/density),
       data = wqa) +
  labs(y = 'Citric Acid', x = 'Alcohol/Density Ratio',
       title = 'Citric Acid vs Alcohol Density ratio') +
  geom_point(aes(color = rating), position = 'jitter',
             alpha = 0.5) +
  scale_color_brewer(type = 'qual') 

