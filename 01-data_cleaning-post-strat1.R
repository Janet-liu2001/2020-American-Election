
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/Janet/Downloads/censa data")
reduced_data_cen <- read_dta("usa_00001.dta")

# Add the labels
reduced_data_cen <- labelled::to_factor(reduced_data_cen)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_cen <- 
  reduced_data_cen %>% 
  select(hhincome,
         race,
         educ,
         sex, 
         age)
         

##clean data
reduced_data_cen$age<-as.numeric(reduced_data_cen$age)
reduced_data_cen<-reduced_data_cen %>% filter(age>=18)
#special NA
reduced_data_cen<-reduced_data_cen %>% 
  filter(hhincome!=9999999)


#hhincome
reduced_data_cen$hhincome<-as.numeric(reduced_data_cen$hhincome)
max(reduced_data_cen$hhincome)
min(reduced_data_cen$hhincome)
reduced_data_cen<-reduced_data_cen %>% filter(hhincome>=0)
summary(reduced_data_cen$hhincome)

#race
reduced_data_cen$race = ifelse(reduced_data_cen$race=="other asian or pacific islander","Other Asian or Pacific Island",
                                     ifelse(reduced_data_cen$race=="american indian or alaska native","American Indian or Alaska Native",
                                            ifelse(reduced_data_cen$race=="black/african american/negro","Black/African American/Negro",
                                                   ifelse(reduced_data_cen$race=="chinese","Chinese",
                                                          ifelse(reduced_data_cen$race=="japanese","Japanese",
                                                                 ifelse(reduced_data_cen$race=="white","White","Some other race"))))))

reduced_data_cen$race <- as.factor(reduced_data_cen$race)
summary(reduced_data_cen$race)
length(unique(reduced_data_cen$race))#7

#educ
reduced_data_cen$educ = ifelse(reduced_data_cen$educ=="grade 5, 6, 7, or 8" | reduced_data_cen$educ=="n/a or no schooling" | reduced_data_cen$educ=="grade 11"| reduced_data_cen$educ=="grade 10"| reduced_data_cen$educ=="grade 9","Middel School or less",
                                ifelse(reduced_data_cen$educ=="grade 12" | reduced_data_cen$educ=="1 year of college","High School",
                                       ifelse(reduced_data_cen$educ=="5+ years of college","5+ in collage","4 or less in collage")))


reduced_data_cen$educ <- as.factor(reduced_data_cen$educ)
summary(reduced_data_cen$educ)
length(unique(reduced_data_cen$educ))#4
#sex
reduced_data_cen$sex = ifelse(reduced_data_cen$sex=="female","Female","Male")
reduced_data_cen$sex <- as.factor(reduced_data_cen$sex)
summary(reduced_data_cen$sex)
length(unique(reduced_data_cen$sex))#2
#age
unique(reduced_data_cen$age)
reduced_data_cen$age<-as.numeric(reduced_data_cen$age)
reduced_data_cen$age = ifelse(reduced_data_cen$age <=20,"<=20",
                          ifelse(reduced_data_cen$age>20 & reduced_data_cen$age<=30,"21-30",
                                 ifelse(reduced_data_cen$age>30 & reduced_data_cen$age<=40,"31-40",
                                        ifelse(reduced_data_cen$age>40 & reduced_data_cen$age<=50,"41-50",
                                               ifelse(reduced_data_cen$age>50 & reduced_data_cen$age<=60,"51-60",
                                                      ifelse(reduced_data_cen$age>60 & reduced_data_cen$age<=70,"61-70",
                                                             ifelse(reduced_data_cen$age>70 & reduced_data_cen$age<=80,"71-80",
                                                                    ifelse(reduced_data_cen$age>80 & reduced_data_cen$age<=90,"81-90",">90"))))))))

reduced_data_cen$age <- as.factor(reduced_data_cen$age)
summary(reduced_data_cen$age)
length(unique(reduced_data_cen$age))#9



#Drop NAs
reduced_data_cen<-na.omit(reduced_data_cen)


#rename
library(janitor)
reduced_data_cen <- reduced_data_cen %>% 
  clean_names() %>% 
  rename(Age_group=age, 
         Sex=sex,
         HHIncome=hhincome,
         Race=race,
         Edu=educ)



# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data_cen, "cleancensus_data.csv")
         