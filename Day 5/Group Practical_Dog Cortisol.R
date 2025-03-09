##########
#title: Practical 5: Dog cortisol
#author: Praise Adeyemo
#date: 29th October 2024
#######
 
setwd("~/Documents/UofG PhD Masterfile/KRS 2024/Intro_to_R/Day 5")

library(readr)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(tidyr)
library(lubridate)

DogCortisol <- read.csv("ASWEL - dog cort data/Dog cort.csv")
View(DogCortisol)
glimpse(DogCortisol)
#data contains six variables; 'Week' 'Treatment' and 'Sex' variables are 
##categorical data; 'Cortisol' and 'Time' are co-variates or continuous data
#all the categorical data have 2 levels each

DogCortisol <- na.omit(DogCortisol) #to remove observation with NA
DogCortisol

#changing categorical data into factors to show their levels
DogCortisol$Treatment <- as.factor(DogCortisol$Treatment)
DogCortisol$Week <- as.factor(DogCortisol$Week)
DogCortisol$Sex <- as.factor(DogCortisol$Sex)

#Research question: What is the association between treatment on the cortisol 
#levels of the dogs?
#H0: There is no association between treatment on cortisol levels
#Ha: There is an association between treatment on cortisol levels
ggplot(data = DogCortisol, aes(x = Week, y = Cortisol)) + 
  geom_boxplot()

ggplot(data = DogCortisol, aes(x = Treatment, y = Cortisol)) + 
  geom_boxplot()

ggplot(data = DogCortisol, aes(x = Sex, y = Cortisol)) + 
  geom_boxplot()

#Research question: What is the association between treatment and sex of dog on the cortisol levels of the dogs?
#H0: There is no association between treatment and sex on cortisol levels
#Ha: There is an association between treatment and sex on cortisol levels

ggplot(data = DogCortisol, aes(x = Time, y = Cortisol)) +
  geom_point() #scatterplot for covariates
#it's difficult to explain the relationship between time and cortisol;

ggplot(DogCortisol, aes(x = Treatment, y = Cortisol,
                   fill = Sex)) +
  geom_bar(stat = 'identity', position = 'dodge')
#to see how the sex of the dogs differ based on treatment type
#treatment type 2 increased the cortisol levels in sex 2 more than the others


Model1<-lm(Cortisol~Treatment, data=DogCortisol) #to see the which of the 2 treatments
#has an effect on the cortisol
anova(Model1)
summary(Model1)
#treatment explains the cortisol levels in the dogs because the residuals are lower
#than the treatment variable
#Treatment 2 explains the response variable

Model2<-lm(Cortisol~Sex, data=DogCortisol) #to see the effect of sex on cortisol
anova(Model2)
summary(Model2)
#sex does not explain cortisol

Model3<-lm(Cortisol~Week, data=DogCortisol) #to see the effect of sex on cortisol
anova(Model3)
#week does not explain cortisol at all!

Model4<-lm(Cortisol~Time, data=DogCortisol) #to see the effect of sex on cortisol
anova(Model4)
#time does not explain cortisol at all

Model5<-lm(Cortisol~Treatment + Treatment:Sex, data=DogCortisol)
anova(Model5)
summary(Model5)

Model6<-lm(Cortisol~Treatment + Treatment:Week, data=DogCortisol)
anova(Model6)
summary(Model6)

Model7<-lm(Cortisol~Treatment + Week + Sex, data=DogCortisol)
anova(Model7)
summary(Model7)

Model8<-lm(Cortisol~Treatment + Treatment:Time, data=DogCortisol)
summary(Model8)

Model9<-lm(Cortisol~ Treatment:Week + Treatment + Week, data=DogCortisol)
summary(Model9)

Model10<-lm(Cortisol~Treatment:Sex + Treatment:Week + Treatment:Time + Treatment + Sex + Week + Time, data=DogCortisol)
summary(Model10)


ggplot(DogCortisol, aes(x = Treatment, y = Cortisol,
                        fill = Week)) +
  geom_bar(stat = 'identity', position = 'dodge')


#Treatment:Sex
DogCortisol %>% 
  filter(!is.na(Sex) & !is.na(Treatment) & !is.na(Cortisol)) %>%
  ggplot(aes(y = Cortisol, x = Treatment, fill = Sex)) +
  geom_boxplot() +
  labs(y = "Cortisol", x = "Treatment", fill = "Sex")

#Treatment:Week
DogCortisol %>% 
  filter(!is.na(Week) & !is.na(Treatment) & !is.na(Cortisol)) %>%
  ggplot(aes(y = Cortisol, x = Treatment, fill = Week)) +
  geom_boxplot() +
  labs(y = "Cortisol", x = "Treatment", fill = "Week")



library(lmtest)
lrtest(Model10,Model9)




