# Load the package RCurl
library(RCurl)
# Import the HANES data set from GitHub; break the string into two for readability
# (Please note this readability aspect very carefully)
URL_text_1 <- "https://raw.githubusercontent.com/kannan-kasthuri/kannan-kasthuri.github.io"
URL_text_2 <- "/master/Datasets/HANES/NYC_HANES_DIAB.csv"
# Paste it to constitute a single URL 
URL <- paste(URL_text_1,URL_text_2, sep="")
HANES <- read.csv(text=getURL(URL))
# Rename the GENDER factor for identification
HANES$GENDER <- factor(HANES$GENDER, labels=c("M","F"))
# Rename the AGEGROUP factor for identification
HANES$AGEGROUP <- factor(HANES$AGEGROUP, labels=c("20-39","40-59","60+"))
# Rename the HSQ_1 factor for identification
HANES$HSQ_1 <- factor(HANES$HSQ_1, labels=c("Excellent","Very Good","Good", "Fair", "Poor"))
# Rename the DX_DBTS as a factor
HANES$DX_DBTS <- factor(HANES$DX_DBTS, labels=c("DIAB","DIAB NO_DX","NO DIAB"))
# Omit all NA from the data frame
HANES <- na.omit(HANES)
# Observe the structure
str(HANES)
# Load the tidyverse library
library(tidyverse)
# Make a ggplot
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR)))

# Make a ggplot with asthetic color for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR), color=DX_DBTS))

# Make a ggplot with asthetic size for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR), size=AGEGROUP))

# Make a ggplot with asthetic alpha for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR), alpha=DX_DBTS))

# Make a ggplot with asthetic shape for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR), shape=DX_DBTS))

# Make a ggplot with asthetic shape for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR)), color="blue")

# Make a ggplot with facets
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR))) + 
  facet_wrap(~ DX_DBTS, nrow = 2)

# Make a ggplot with facet grid - GENDER vs DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR))) + 
  facet_grid(GENDER ~ DX_DBTS)
# Make a ggplot with facet grid - AGEGROUP vs DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR))) + 
  facet_grid(AGEGROUP ~ DX_DBTS)

# Plot smooth and point geom in the same plot 
ggplot(data = HANES) + 
  geom_smooth(mapping = aes(x = UCREATININE, y = UALBUMIN, color=GENDER)) + 
  xlim(c(0,100)) + ylim(c(0,10)) +
  geom_point(mapping = aes(x = UCREATININE, y = UALBUMIN, color=GENDER)
             
            