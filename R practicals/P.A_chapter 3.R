#     Intro to R - Day 3  ################################
#   Intro to R - Day 3       #
#   In-class exercise        # 
#   Author: Praise Adeyemo   #
#   Date: 7 October, 2024    #
#################################

setwd("~/Documents/UofG PhD Masterfile/KRS 2024/Intro_to_R/Day 3") #to set the working directory for the exercise
library(readr) #calling the readr package
tick_data <- read_csv("tick_data.csv") #defining the data
View(tick_data) #to view data set
names(tick_data) #to check the variable names in the data
#data has 9 variables - species, sex, day, month, year, recap, wt_with_bag, bag_wt and ticks
dim(tick_data) #to check number of rows and columns
#data has 10597 observations/rows and 9 variables/columns
str(tick_data) #to understand full features of the data
#'species' 'sex' variables are factors/categorical data, while other variables seem to be covariates. I didn't explore the data in full to see the specific types of data

library(dplyr) #calling this package to aid data manipulation
tibble(tick_data) #to construct a dataframe
tick_data %>%
  group_by(species) #to find the number of birds of each species that were trapped
#there are 10 species of birds in the data
(tibble.print_max = Inf) #to show all the species beyond the first ten rows
length(tick_data)
count(tick_data)

#Question 1: Exploring the data
#to summarise the number of birds trapped for each species
tick_data %>%
  group_by(species) %>% summarise(
  n = length(species))
#there are 10 species in the dataset - blackbird, blue tit, chaffinch, coal tit, dunnock, goldfinch, great tut, greenfinch, robin and siskin

#to view the number of years in the data
tick_data %>%
  group_by(year) %>% summarise(
    n = length(year))
#birds data were collected for 3 years - 2009, 2010 and 2011

#to summarise the number of birds trapped for each species per year
tick_data %>%
  group_by(species,year) %>% summarise(
    n = length(species))
options(tibble.print_max = Inf) #add this line to show the table beyond the first ten rows, then run all the codes from line 41-44; where Inf means Infinity


#Question 2: Adding and re-defining variables
#a) Adding a new variable
library(dplyr)
library(tidyr)
library(readr)
tick_data$bird_wt <- (tick_data$wt_with_bag - tick_data$bag_wt) #to create a new variable/column called bird weight 'bird_wt'. Bird weight = wt_with_bag - bag_wt
names(tick_data) #to see if new column or variable has been added to data

#b) creating a new date variable
tick_data$date <- paste(tick_data$day, tick_data$month, tick_data$year, collapse = NULL, recycle0 = FALSE) #to create a new variable/column called date, collapsing three rows (day,  month, year) into one string
View(tick_data) #dataset now has a new column for date
library(lubridate) #to call this package for use
tick_data <- mutate(tick_data, date = dmy(tick_data$date)) #to show clarity in the date

#c) recording an existing variable e.g. changing a numeric value into a factor with levels
library(dplyr)
tick_data$Recap <- (recode(tick_data$recap,'1'="yes",'0'="no")) #to recode the variable 'recap' from having 1s and 0s to yes or no. A new column has been created with the name 'Recap'
tick_data$Recap <- as.factor(tick_data$Recap) #to tell R to treat the new variable 'Recap' as a factor with levels


#Question 3: Sorting
#a) to sort the columns using the arrange() function
tick_data %>% 
  arrange(tick_data$date, tick_data$species) #to simultaneously sort the data first by date and then by species
options(tibble.print_max = 10) #added this to shorten the rows displayed in the console

#b) sorting using the order() function
head(order(tick_data$date)) 
tick_data <- tick_data[order(tick_data$date),]
tick_data <- tick_data[rev(order(tick_data$date)),] #changing the order from increasing to decreasing
#need to reverse the data to be chronological
head(order(tick_data$date)) 
tick_data <- tick_data[order(tick_data$date),]


#Question 4: Subsetting data
library(dplyr)
tick_data$tick_season <- (tick_data$ticks)


