library(shiny)
library(tidyverse)

server<-function(input, output){
  output$plot<-renderPlot({
    # Define possible articles
    article <- data.frame(type = c("real", "fake"))
    # Define the prior model
    prior <- c(0.6, 0.4)
    article_sim <- sample_n(article,
      size = input$num,weight = prior, replace = TRUE)
    article_sim<-article_sim %>% 
      count(type) %>% 
      group_by(type) %>% 
      mutate(prop=n/input$num)
    ggplot(article_sim, aes(x=type, y=prop))+
      geom_col()+
      labs(title = "Posterior Distribution", x="Type", y="Percentage")+
      theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14
                                   ,face="bold"),
           plot.title = element_text(size=22))
    
  })
  
  
}
