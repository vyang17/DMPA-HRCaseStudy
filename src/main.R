library(tidyverse)

generalData = read.csv(file = "general_data.csv", header = T)
employeeSurveyData = read.csv(file = "employee_survey_data.csv", header = T)
managerSurveyData = read.csv(file = "manager_survey_data.csv", header = T)

df = generalData %>% 
  left_join(employeeSurveyData,  by = "EmployeeID") %>% 
  left_join(managerSurveyData, by = "EmployeeID")

