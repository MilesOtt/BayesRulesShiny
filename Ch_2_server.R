library(shiny)
library(tidyverse)
titlePanel("Section 2.1.5: P Simulation")

server<-function(input, output){
  output$plot<-renderPlot({
    # Define possible articles
    article <- data.frame(type = c("real", "fake"))
    # Define the prior model
    prior <- c(0.6, 0.4)
    article_sim <- sample_n(article,
                            size = input$sum, weight = prior, replace = TRUE)
    hist(article_sim)
    
  })
  
}