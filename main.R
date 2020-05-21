
## load in required libraries
library(tidyverse)
library(GGally)
library(caret)
library(missForest)

source(file = "header.R")

## set working directory
# setwd("C:/Users/Victor/Project/hr")

## read in data
generalData = read.csv(file = "./src/general_data.csv", header = T)
employeeSurveyData = read.csv(file = "./src/employee_survey_data.csv", header = T)
managerSurveyData = read.csv(file = "./src/manager_survey_data.csv", header = T)

## join employee and manager survey data to general data by EmployeeID
df = generalData %>% 
  left_join(employeeSurveyData,  by = "EmployeeID") %>% 
  left_join(managerSurveyData, by = "EmployeeID")

## summary
summary(df)

## check if df is complete
all(complete.cases(df))
which(!complete.cases(df)) %>% length()

## look at columns containing missing values; determine if factor or int to prep for missForest
colnames(df)[c(15,20,25:27)]

### Impute missing values using missForest
## uncomment to run, otherwise load from file

## converted missing variables to factors before missForest
# set.seed(6)
# df.imp = 
#   df %>%
#   mutate_at(c(15,20,25:27), .funs = as.factor) %>% 
#   missForest(variablewise = TRUE)

# save and load df.imp
# saveRDS(df.imp, file = "df-imp.rds")
df.imp = readRDS(file = "df-imp.rds")

## check imputation error
df.imp$OOBerror

## assign imposed dataframe to dfFinal
dfFinal = df.imp$ximp
all(complete.cases(dfFinal))
summary(dfFinal)

## missForest without converting missing variables to factors
# set.seed(6)
# df.imp.noConvert = missForest(df, variablewise = TRUE)

### convert data to correct data types, rearrange attrition to first column, deselect some columns
dfFinal = 
  dfFinal %>%
  select(Attrition, everything(), -c(EmployeeCount, EmployeeID, StandardHours, Over18, JobRole)) %>%
  mutate(
    Education = as.factor(Education),
    JobLevel = as.factor(JobLevel),
    StockOptionLevel = as.factor(StockOptionLevel),
    TotalWorkingYears = as.integer(TotalWorkingYears),
    JobInvolvement = as.factor(JobInvolvement),
    PerformanceRating = as.factor(PerformanceRating),
    NumCompaniesWorked = as.integer(NumCompaniesWorked),
    TotalWorkingYears = as.integer(TotalWorkingYears))

dfFinal %>% glimpse()

### ggpairs
## only run if necessary, look at Rplot03.jpeg
# ggpairs(dfFinal)

## Testing types of plots
dfFinal %>% 
  ggplot(aes(x = JobSatisfaction, fill = Attrition)) + 
  geom_bar()

dfFinal %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) + 
  geom_histogram(bins = 30)

## Iteratively create ggplots
yVar = "Attrition"
ggPlotPlease(data = dfFinal, yVar = yVar)


yVar2 = "JobSatisfaction"
ggPlotPlease(data = dfFinal, yVar = yVar2)








### Analysis Idea Questions
##
## How does monthly income relate to an employee's job satisfaction?
df %>% ggplot(aes(x = as.factor(JobSatisfaction), y = MonthlyIncome)) + 
  geom_boxplot()

## Does a person's marital status affect how long they stay with the company?
df %>% ggplot(aes(x = as.factor(MaritalStatus), y = YearsAtCompany)) +
  geom_boxplot()

## feature engineering
## create age groups
df %>% mutate(
  age18_24 = as.factor(ifelse(Age >= 18 & Age <= 24, 1, 0)),
  age25_34 = as.factor(ifelse(Age >= 25 & Age <= 34, 1, 0)),
  age35_44 = as.factor(ifelse(Age >= 35 & Age <= 44, 1, 0)),
  age45_54 = as.factor(ifelse(Age >= 45 & Age <= 54, 1, 0)),
  age55plus = as.factor(ifelse(Age >= 55, 1, 0))
)

## age vs attrition
df %>% ggplot(aes(x = as.factor(Attrition), y = Age)) +
  geom_boxplot()

## Job Satisfaction
df %>% ggplot(aes(x = JobSatisfaction)) + 
  geom_histogram()



## correlation plot
# cordf = dfFinal %>% select_if(is.numeric) %>% drop_na() %>% cor()
# corrplot(cordf)
