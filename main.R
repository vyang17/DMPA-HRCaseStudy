## disable scientific notation
options(scipen=999)

## load in required libraries
library(tidyverse)
library(GGally)
library(caret)
library(missForest)
library(arules)
library(scales)

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
# dfFinal %>% 
#   ggplot(aes(x = JobSatisfaction, fill = Attrition)) + 
#   geom_bar()
# 
# dfFinal %>%
#   ggplot(aes(x = YearsAtCompany, fill = Attrition)) + 
#   geom_histogram(bins = 30)

## Iteratively create ggplots
ggPlotPlease(data = dfFinal, yVar = "Attrition")

# yVar2 = "JobSatisfaction"
# ggPlotPlease(data = dfFinal, yVar = yVar2)

## create train and testing
set.seed(144)
in_train = createDataPartition(y = dfFinal$Attrition, 
                               p = 0.8, 
                               list = FALSE)

training = dfFinal[in_train, ]
testing = dfFinal[-in_train, ]

## generate single decision tree, bagging, logistic regression, random forest models
modelList = list()
modelList[[1]] = generateModels(training)

## single tree: model 1 summary
summary(modelList[[1]]$tree)
predictionsTree = predict(modelList[[1]]$tree, newdata = testing)
confusionMatrix(predictionsTree, testing$Attrition)

## bagging: model 1 summary
summary(modelList[[1]]$bagged)
predictionsBagged = predict(modelList[[1]]$bagged, newdata = testing)
confusionMatrix(predictionsBagged, testing$Attrition)

## logistic regression: model 1 summary
summary(modelList[[1]]$lr)
predictionsLR = predict(modelList[[1]]$lr, newdata = testing)
confusionMatrix(predictionsLR, testing$Attrition)

## random forest: model 1 summary
summary(modelList[[1]]$rf)
predictionsRF = predict(modelList[[1]]$rf, newdata = testing)
confusionMatrix(predictionsRF, testing$Attrition)

plot(varImp(modelList[[1]]$rf))

## benchmark and dotplot
results = resamples(list(treeModel = modelList[[1]]$tree,
                         baggedModel = modelList[[1]]$bagged,
                         lrModel = modelList[[1]]$lr,
                         rfModel = modelList[[1]]$rf))

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

# split MonthlyIncome into thirds
dfFinal2$IncomeThirds = discretize(dfFinal2$MonthlyIncome,
                                   method = "frequency",
                                   breaks = 3,
                                   labels = c("IncomeLow", "IncomeMed", "IncomeHigh"))

IncomeBrackets = model.matrix( ~ IncomeThirds - 1, data = dfFinal2)
dfFinal2 = cbind(dfFinal2, IncomeBrackets)

## investigate any near zero variance columns: age55plus
nzv(dfFinal2)

## remove 1 in c encoded/discretized variables and nzv variables
dfFinal2 = dfFinal2 %>%
  select(-c(Age, age55plus, YearsSinceLastPromotion, MonthlyIncome, IncomeThirds, TotalWorkingYears, TWYQuartiles))


## Run models again with new engineered features
## create train and testing 
set.seed(144)
in_train2 = createDataPartition(y = dfFinal2$Attrition, 
                               p = 0.8, 
                               list = FALSE)

training2 = dfFinal2[in_train2, ]
testing2 = dfFinal2[-in_train2, ]

## generate single decision tree, bagging, logistic regression, random forest models 
modelList[[2]] = generateModels(training2, modelNumber = 2)

## single tree: model 2 summary
summary(modelList[[2]]$tree)
predictionsTree2 = predict(modelList[[2]]$tree, newdata = testing2)
confusionMatrix(predictionsTree2, testing2$Attrition)

## bagging: model 2 summary
summary(modelList[[2]]$bagged)
predictionsBagged2 = predict(modelList[[2]]$bagged, newdata = testing2)
confusionMatrix(predictionsBagged2, testing2$Attrition)

## logistic regression: model 2 summary
summary(modelList[[2]]$lr)
predictionsLR2 = predict(modelList[[2]]$lr, newdata = testing2)
confusionMatrix(predictionsLR2, testing2$Attrition)

## random forest: model 2 summary
summary(modelList[[2]]$rf)
predictionsRF2 = predict(modelList[[2]]$rf, newdata = testing2)
confusionMatrix(predictionsRF2, testing2$Attrition)

plot(varImp(modelList[[2]]$rf))

## benchmark and dotplot
results2 = resamples(list(treeModel = modelList[[2]]$tree,
                         baggedModel = modelList[[2]]$bagged,
                         lrModel = modelList[[2]]$lr,
                         rfModel = modelList[[2]]$rf))

results2
summary(results2)

dotplot(results2)


## Association Rule Mining
dfFinal3 = dfFinal

## character vector that stores names of numeric columns in dfFinal; used to determine which columns will be discretized
numericCols = 
  dfFinal3 %>% 
  select_if(is.numeric) %>% 
  names()

## initialize tmp df to house discretized data
tmpdf = as_tibble(matrix(NA, nrow = nrow(dfFinal3), ncol = length(numericCols)))

## Assign column names corresponding to correct discretized columns
names(tmpdf) = paste0(numericCols, "_Group")

## iterate through numeric columns in dfFinal3 and discretize them
for( i in 1:length(numericCols)) {
  tmpdf[,i] = discretize(dfFinal3[,numericCols[i]],
                         method = "frequency",
                         breaks = 4)
}


# for( i in 1:length(numericCols)) {
#   if(numericCols[i] != "Age") {
#     tmpdf[,i] = discretize(dfFinal3[,numericCols[i]],
#                            method = "frequency",
#                            breaks = 4)
#   } else {
#     tmpdf[,i] = discretize(dfFinal3[,numericCols[i]],
#                            method = "interval",
#                            breaks = 8,
#                            labels = c("Age_G1", "Age_G2", "Age_G3", "Age_G4", "Age_G5", "Age_G6", "Age_G7", "Age_G8"))
#   }
# }

## remove numeric columns from dfFinal and combine with discretized columns
dfFinal3 = 
  dfFinal %>%
  select(-numericCols) %>%
  cbind(tmpdf)

# make transactions
hrTrans = as(dfFinal3, 'transactions')
inspect(head(hrTrans))
summary(hrTrans)

itemFrequencyPlot(hrTrans, topN = 10, cex = 0.70)

# rules 1: sup = 0.01, conf = 0.60
rules = apriori(hrTrans,
                parameter = list(sup = 0.01, conf = 0.60, target = "rules"))

inspect(head(rules))
summary(rules)

# filter rules to Attrition = "Yes"
attritionYes = subset(rules, subset = rhs %in% "Attrition=Yes" & lift > 1)

inspect(head(attritionYes, by = "lift", n = 10))
summary(attritionYes)

# filter rules to Attrition = "No"
attritionNo = subset(rules, subset = rhs %in% "Attrition=No" & lift > 1)

inspect(head(attritionNo, by = "lift", n = 10))
summary(attritionNo)


### Random Additional Usefulness for presentation
dfFinal %>%
  ggplot(aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot()


df4 = 
  dfFinal %>%
  mutate(Age_Group = discretize(dfFinal$Age,
                                method = "interval",
                                breaks = 8,
                                labels = c("Age_G1", "Age_G2", "Age_G3", "Age_G4", "Age_G5", "Age_G6", "Age_G7", "Age_G8"))) %>%
  group_by(Age_Group) %>% 
  summarise(
    nTotal = n(),
    nLeft = sum(Attrition == "Yes"),
    attrPct = round((nLeft / nTotal) * 100, 1))

round(attr(df4$Age_Group, "discretized:breaks"))

avgAttrRate = round(mean(dfFinal$Attrition == "Yes") * 100, 1)
df4 %>%
  ggplot(aes(x = Age_Group, y = attrPct)) +
  geom_col(fill = "#F8766D") +
  geom_text(aes(label = paste0(attrPct, "%")), 
            vjust = 1.3,
            size = 4.8) +
  geom_hline(yintercept = avgAttrRate, 
             color = "red", 
             size = 1.2) +
  geom_text(aes(0,
                avgAttrRate,
                label = paste0(avgAttrRate, "%"), 
                hjust = -0.1,
                vjust = -0.6)) +
  labs(title = "Attrition Rate of different Age Groups", 
       x = "Age Group", 
       y = "Attrition Rate (%)") +
  ylim(0, 60) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

dfFinal3 %>%
  ggplot(aes(x = YearsAtCompany_Group, fill = Attrition)) + 
  geom_bar() +
  theme_minimal()

dfFinal3 %>%
  ggplot(aes(x = TotalWorkingYears_Group, fill = Attrition)) + 
  geom_bar()

dfFinal3 %>%
  ggplot(aes(x = BusinessTravel, fill = Attrition)) + 
  geom_bar()

dfBT = 
  dfFinal3 %>%
    group_by(BusinessTravel) %>%
    summarise(
      nTotal = n(),
      nLeft = sum(Attrition == "Yes"),
      attrPct = round((nLeft / nTotal) * 100, 1))

dfBT %>%
  ggplot(aes(x = BusinessTravel, y = attrPct)) +
  geom_col(fill = "#F8766D") +
  geom_text(aes(label = paste0(attrPct, "%")), 
            vjust = 1.3,
            size = 4.8) +
  geom_hline(yintercept = avgAttrRate, 
             color = "red", 
             size = 1.2) +
  geom_text(aes(0,
                avgAttrRate,
                label = paste0(avgAttrRate, "%"), 
                hjust = -0.1,
                vjust = -0.6)) +
  labs(title = "Attrition Rate of different Business Travel Frequency", 
       x = "Business Travel Frequency", 
       y = "Attrition Rate (%)") +
  ylim(0, 60) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )


## benchmark and dotplot for ALL models
resultsAll = resamples(list(treeModel1 = modelList[[1]]$tree,
                          treeModel2 = modelList[[2]]$tree,
                          baggedModel1 = modelList[[1]]$bagged,
                          baggedModel2 = modelList[[2]]$bagged,
                          lrModel1 = modelList[[1]]$lr,
                          lrModel2 = modelList[[2]]$lr,
                          rfModel1 = modelList[[1]]$rf,
                          rfModel2 = modelList[[2]]$rf))

resultsAll
summary(resultsAll)

dotplot(resultsAll)
