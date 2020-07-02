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
    #First Plot
    ggplot(article_sim, aes(x=type))+
      geom_bar()+
      labs(title = "Prior Distribution", x="Type", y="Count")+
      theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14
                                   ,face="bold"),
           plot.title = element_text(size=22))
})
    
    
    
output$plot2<-renderPlot({
   article <- data.frame(type = c("real", "fake"))
   # Define the prior model
   prior <- c(0.6, 0.4)
   article_sim <- sample_n(article,
                           size = input$num,weight = prior, replace = TRUE)
   #Likelihood 
   article_sim <- article_sim %>% 
     mutate(likelihood = 
              case_when(type == "fake" ~ 0.28,type == "real" ~ 0.02 ))
   #Creting Proportions
   data <- c("no", "yes")
   # Simulate exclamation point usage
   set.seed(3)
   article_sim <- article_sim %>%
     group_by(1:n()) %>%
     mutate(usage = sample(data, size = 1,
                           prob = c(1 - likelihood, likelihood))) 
   
   ggplot(article_sim, aes(x = usage)) + geom_bar() +
     facet_wrap(~ type)    +
     labs(title = "Likelihood Distribution", x="Usage", y="Count")+
     theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14
                                   ,face="bold"),
           plot.title = element_text(size=22))
   
 })
 
 
 
 
 
 
 output$plot3<-renderPlot({

   article <- data.frame(type = c("real", "fake"))
   # Define the prior model
   prior <- c(0.6, 0.4)
   article_sim <- sample_n(article,
                           size = input$num,weight = prior, replace = TRUE)
   #Likelihood 
   article_sim <- article_sim %>% 
     mutate(likelihood = 
              case_when(type == "fake" ~ 0.28,type == "real" ~ 0.02 ))
   #Creting Proportions
  
   
   data <- c("no", "yes")
   set.seed(3)
   article_sim <- article_sim %>%
     group_by(1:n()) %>%
     mutate(usage = sample(data, size = 1,
                           prob = c(1 - likelihood, likelihood))) 
   
  
   ggplot(article_sim, aes(x = type)) + 
     geom_bar() +
     facet_wrap(~ usage)+
     labs(title = "Posterior Distribution", x="Type", y="Count")+
     theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14
                                   ,face="bold"),
           plot.title = element_text(size=22))
 })

  
  
}
