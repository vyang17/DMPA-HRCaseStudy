
## load in required libraries
library(tidyverse)
library(GGally)
library(caret)
library(missForest)
library(arules)

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

## create train and testing 
in_train = createDataPartition(y = dfFinal$Attrition, 
                               p = 0.8, 
                               list = FALSE)

training = dfFinal[in_train, ]
testing = dfFinal[-in_train, ]

## logistic regression: full model
lrModel = train(Attrition ~ .,
                family = "binomial",
                data = training,
                method = "glm",
                trControl = trainControl(method = "cv",
                                         number = 10
                                         )
                )

summary(lrModel)
predictionsLR = predict(lrModel, newdata = testing)
confusionMatrix(predictionsLR, testing$Attrition)


## random forest
rfModel = readRDS(file = "rfModel.rds")
# rfModel = train(Attrition ~ .,
#                 data = training,
#                 method = "rf",
#                 trControl = trainControl(method = "cv",
#                                          number = 10
#                                          )
#                 )
# saveRDS(rfModel, file = "rfModel.rds")


summary(rfModel)
predictionsRF = predict(rfModel, newdata = testing)
confusionMatrix(predictionsRF, testing$Attrition)

plot(varImp(rfModel))


## benchmark and dotplot
results = resamples(list(lrModel = lrModel,
                         rfModel = rfModel))

results
summary(results)

dotplot(results)


## feature engineering
# create age groups
# create overOneYear since last promotion variable
dfFinal2 = 
  dfFinal %>%
  mutate(
    age18_24 = as.factor(ifelse(Age >= 18 & Age <= 24, 1, 0)),
    age25_29 = as.factor(ifelse(Age >= 25 & Age <= 29, 1, 0)),
    age30_34 = as.factor(ifelse(Age >= 30 & Age <= 34, 1, 0)),
    age35_39 = as.factor(ifelse(Age >= 35 & Age <= 39, 1, 0)),
    age40_44 = as.factor(ifelse(Age >= 40 & Age <= 44, 1, 0)),
    age45_49 = as.factor(ifelse(Age >= 45 & Age <= 49, 1, 0)),
    age50_54 = as.factor(ifelse(Age >= 50 & Age <= 54, 1, 0)),
    age55plus = as.factor(ifelse(Age >= 55, 1, 0)),
    overOneYear = as.factor(ifelse(YearsSinceLastPromotion >= 2, 1, 0))
  )


# split TotalWorkingYears into quartiles
dfFinal2$TWYQuartiles = discretize(dfFinal2$TotalWorkingYears,
                                   method = "frequency",
                                   breaks = 4,
                                   labels = c("TWY_Q1", "TWY_Q2", "TWY_Q3", "TWY_Q4"))

TWY = model.matrix( ~ TWYQuartiles - 1, data = dfFinal2)
dfFinal2 = cbind(dfFinal2, TWY)

# split MonthlyIncome into quartiles
dfFinal2$IncomeThirds = discretize(dfFinal2$MonthlyIncome,
                                   method = "frequency",
                                   breaks = 3,
                                   labels = c("IncomeLow", "IncomeMed", "IncomeHigh"))

IncomeBrackets = model.matrix( ~ IncomeThirds - 1, data = dfFinal2)
dfFinal2 = cbind(dfFinal2, IncomeBrackets)

nzv(dfFinal2)

dfFinal2 = dfFinal2 %>%
  select(-c(Age, age55plus, YearsSinceLastPromotion, MonthlyIncome, IncomeThirds, TotalWorkingYears, TWYQuartiles))


## Run models again with new engineered features

## create train and testing 
in_train2 = createDataPartition(y = dfFinal2$Attrition, 
                               p = 0.8, 
                               list = FALSE)

training2 = dfFinal2[in_train2, ]
testing2 = dfFinal2[-in_train2, ]

## logistic regression: full model
lrModel2 = train(Attrition ~ .,
                family = "binomial",
                data = training2,
                method = "glm",
                trControl = trainControl(method = "cv",
                                         number = 10
                )
)

summary(lrModel2)
predictionsLR2 = predict(lrModel2, newdata = testing2)
confusionMatrix(predictionsLR2, testing2$Attrition)


## random forest
rfModel2 = readRDS(file = "rfModel2.rds")
# rfModel2 = train(Attrition ~ .,
#                 data = training2,
#                 method = "rf",
#                 trControl = trainControl(method = "cv",
#                                          number = 10
#                                          )
#                 )
# saveRDS(rfModel2, file = "rfModel2.rds")


summary(rfModel2)
predictionsRF2 = predict(rfModel2, newdata = testing2)
confusionMatrix(predictionsRF2, testing2$Attrition)

plot(varImp(rfModel2))


## benchmark and dotplot
results2 = resamples(
  list(
    lrModel1 = lrModel,
    rfModel1 = rfModel,
    lrModel2 = lrModel2,
    rfModel2 = rfModel2))

results2
summary(results2)

dotplot(results2)






















### Analysis Idea Questions
##
## How does monthly income relate to an employee's job satisfaction?
df %>% ggplot(aes(x = as.factor(JobSatisfaction), y = MonthlyIncome)) + 
  geom_boxplot()

## Does a person's marital status affect how long they stay with the company?
df %>% ggplot(aes(x = as.factor(MaritalStatus), y = YearsAtCompany)) +
  geom_boxplot()

## age vs attrition
df %>% ggplot(aes(x = as.factor(Attrition), y = Age)) +
  geom_boxplot()

df %>% ggplot(aes(x = Age, fill = as.factor(Attrition))) +
  geom_histogram()

## Job Satisfaction
df %>% ggplot(aes(x = JobSatisfaction)) + 
  geom_histogram()



## correlation plot
# cordf = dfFinal %>% select_if(is.numeric) %>% drop_na() %>% cor()
# corrplot(cordf)
