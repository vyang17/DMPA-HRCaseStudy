
# load in required libraries
library(tidyverse)
library(GGally)
library(caret)
library(missForest)

# set working directory
#setwd("C:/Users/Victor/Project/hr")

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

# look at columns containing missing values; determine if factor or int to prep for missForest
colnames(df)[c(15,20,25:27)]

### Impute missing values using missForest
##
# converted missing variables to factors before missForest
set.seed(6)
df.imp = 
  df %>%
  mutate_at(c(15,20,25:27), .funs = as.factor) %>% 
  missForest(variablewise = TRUE)

#check imputation error
df.imp$OOBerror

# assign imposed dataframe to dfFinal
dfFinal = df.imp$ximp
all(complete.cases(dfFinal))
summary(dfFinal)

# missForest without converting missing variables to factors
# set.seed(6)
# df.imp.noConvert = missForest(df, variablewise = TRUE)

### ggpairs
ggpairs(dfFinal)



### Analysis Idea Questions
##
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


# correlation plot
cordf = df %>% select_if(is.numeric) %>% drop_na() %>% cor()
corrplot(cordf)
                         