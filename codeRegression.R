library(tidyverse)
library(dplyr)
library(brms)
library(rstan)
example(stan_model,run.dontrun=TRUE,verbose=TRUE)
# Read In Data
df <- read_csv("E:/PT/gss.csv")
# abstract key features
feature <- df %>% select(age,age_at_first_birth, sex,  education,  average_hours_worked, self_rated_health,  income_respondent, occupation)
# Overview
glimpse(feature)

#Clean data
#Remove rows contains NA
feature %>% drop_na()
dim(feature)

#fitting regression model
set.seed(4)
brm1 <-  brm( age_at_first_birth ~ 1/age+sex+education+average_hours_worked+self_rated_health+income_respondent+occupation,
              
          data=feature, 
             
          brmsfamily("gaussian"),
      
          chains = 4, 
         
          cores = getOption("mc.cores", 1),
             
          iter = 3000,
       
          warmup = 1500) 

summary(brm1)
plot(brm1)