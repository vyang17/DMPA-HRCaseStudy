
# load in required libraries
library(tidyverse)
library(GGally)
library(caret)

# set working directory
setwd("C:/Users/Victor/Project/hr")

# read in data
generalData = read.csv(file = "./src/general_data.csv", header = T)
employeeSurveyData = read.csv(file = "./src/employee_survey_data.csv", header = T)
managerSurveyData = read.csv(file = "./src/manager_survey_data.csv", header = T)

# join employee and manager survey data to general data by EmployeeID
df = generalData %>% 
  left_join(employeeSurveyData,  by = "EmployeeID") %>% 
  left_join(managerSurveyData, by = "EmployeeID")

# summary
summary(df)

# check if df is complete
all(complete.cases(df))
which(!complete.cases(df)) %>% length()

# How does monthly income relate to an employee's job satisfaction?
df %>% ggplot(aes(x = as.factor(JobSatisfaction), y = MonthlyIncome)) + 
  geom_boxplot()

# Does a person's marital status affect how long they stay with the company?
df %>% ggplot(aes(x = as.factor(MaritalStatus), y = YearsAtCompany)) +
  geom_boxplot()

# feature engineering
# create age groups
df %>% mutate(
      age18_24 = as.factor(ifelse(age >= 18 & age <= 24, 1, 0)),
      age25_34 = as.factor(ifelse(age >= 25 & age <= 34, 1, 0)),
      age35_44 = as.factor(ifelse(age >= 35 & age <= 44, 1, 0)),
      age45_54 = as.factor(ifelse(age >= 45 & age <= 54, 1, 0)),
      age55plus = as.factor(ifelse(age >= 55, 1, 0))
      )

# age vs attrition
df %>% ggplot(aes(x = as.factor(Attrition), y = Age)) +
  geom_boxplot()

# Job Satisfaction
df %>% ggplot(aes(x = JobSatisfaction)) + 
  geom_histogram()

# for(i in colnames(df)) {
#   df %>% ggplot(
#   
# }



cordf = df %>% select_if(is.numeric) %>% drop_na() %>% cor()
corrplot(cordf)
                         