
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
