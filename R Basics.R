#Cars

mtcars %>%
  View()


#Coverting rowname into a column

mtcars %>%
  rownames_to_column() %>% 
  rename(carname = rowname) -> mycars

#Display in console
mycars

# Top 6 lines
mycars %>%
  head()

#Bottom 6 lines

mycars %>%
  tail()

#Filter rows
mycars %>%
filter(gear == 3) %>%
  View()

mycars %>% 
  filter(gear == 3 & mpg > 15) %>%
  View()

#Select column
mycars %>%
  select(mpg) %>%
  View()

#Selecting multiple columns
mycars %>%
  select(carname,cyl,gear) %>%
  View()

mycars %>%
  select(starts_with("mp")) %>%
  View()

Variable <- c("gear","hp","dslp")

mycars %>%
  select(c(1,4,5:8))

mycars %>%
  select(-c(4,5,6))

mycars %>%
  View()
 
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

