
ggPlotPlease = function(data, yVar) {

   xVar = 
      dfFinal %>% 
      select(-c(yVar)) %>% 
      names()
   
   for(i in xVar) {
      plt = 
         data %>%
         ggplot(mapping = aes_string(x = i, fill = yVar)) +
         labs(title = str_c("Distribution of ", i),
              subtitle = str_c("grouped by ", yVar))
      
      if(is.numeric(i)) {
         print(plt + geom_histogram(bins = 30))
      } else {
         print(plt + geom_bar())
      }
   }
}


# generate single decision tree, bagging, logistic regression, random forest models
generateModels = function(trainingData, modelNumber = 1, runRF = FALSE, predY = "Attrition") {

   tmplist = list()
   
   # single decision tree model
   print("Generating single tree model...")
   tmplist[[1]] = train(Attrition ~ .,
                        data = trainingData,
                        method = "rpart",
                        trControl = trainControl(method = "cv",
                                                 number = 10
                                                 )
                        )
   print("Tree model completed.")
      
   # bagged model
   print("Generating bagging model...")
   tmplist[[2]] = train(Attrition ~ .,
                       data = trainingData,
                       method = "treebag",
                       trControl = trainControl(method = "cv",
                                                number = 10
                                                )
                       )
   print("Bagged model completed.")
   
   # logistic regression model
   print("Generating logistic regression model...")
   tmplist[[3]] = train(Attrition ~ .,
                   family = "binomial",
                   data = trainingData,
                   method = "glm",
                   trControl = trainControl(method = "cv",
                                            number = 10
                   )
   )
   print("Logistic regression model completed.")
   
   # random forest model
   if(runRF) {
      print("Generating random forest model...")
      tmplist[[4]] = train(Attrition ~ .,
                           data = trainingData,
                           method = "rf",
                           trControl = trainControl(method = "cv",
                                                    number = 10
                           )
      )
      print("Random Forest model completed.")
      
      saveRDS(tmplist[[4]], file = paste0("rfModel", modelNumber, ".rds"))
      print(paste0("Random Forest model saved to ./", paste0("rfModel", modelNumber, ".rds")))
      
   } else {
      print("Loaded random forest model from file.")
      tmplist[[4]] = readRDS(file = paste0("rfModel", modelNumber, ".rds"))
   }
   
   names(tmplist) = c("tree", "bagged", "lr", "rf")
   
   return(tmplist)
}
