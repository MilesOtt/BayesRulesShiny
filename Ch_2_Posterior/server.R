library(shiny)
library(tidyverse)


server<-function(input, output){
  output$plot<-renderPlot({
    # Define possible articles
    article <- data.frame(type = c("real", "fake"))
    # Define the prior model
    prior <- c(0.6, 0.4)
    article_sim <- sample_n(article,
                            size = input$num, weight = prior, replace = TRUE)
    ggplot(article_sim, aes(x=type))+
      geom_bar()
    
  })
  
  
}
