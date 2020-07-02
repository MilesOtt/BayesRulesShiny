library(shiny)
library(tidyverse)

server<-function(input, output){
  output$plot<-renderPlot({
    # Define possible win probabilities
    chess <- data.frame(pi = c(0.2, 0.5, 0.8))
    # Define prior model
    prior <- c(0.10, 0.25, 0.65)
    # Simulate 10000 values of pi from the prior
    set.seed(84735)
    chess_sim <- sample_n(chess,
                          size = input$num, weight = prior, replace = TRUE)
    chess_sim <- chess_sim %>%
      mutate(x = rbinom(input$num, size = 6, prob = pi))
    ggplot(chess_sim, aes(x = pi)) + 
      geom_bar()+
      labs(title = "Prior Distribution", x="Pi", y="Count")+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14
                                    ,face="bold"),
            plot.title = element_text(size=22))
  })
  
  
  
  output$plot2<-renderPlot({
    # Define possible win probabilities
    chess <- data.frame(pi = c(0.2, 0.5, 0.8))
    # Define prior model
    prior <- c(0.10, 0.25, 0.65)
    # Simulate 10000 values of pi from the prior
    set.seed(84735)
    chess_sim <- sample_n(chess,
                          size = input$num, weight = prior, replace = TRUE)
    chess_sim <- chess_sim %>%
      mutate(x = rbinom(input$num, size = 6, prob = pi))
    ggplot(chess_sim, aes(x = x)) + stat_count(aes(y = ..prop..)) + facet_wrap(~ pi)+
      labs(title = "Win Outcomes for each Pi", x="Win Outcomes", y="Count")+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14
                                    ,face="bold"),
            plot.title = element_text(size=22))
    
  })
  
  
  
  
  
  
  output$plot3<-renderPlot({
    
    # Define possible win probabilities
    chess <- data.frame(pi = c(0.2, 0.5, 0.8))
    # Define prior model
    prior <- c(0.10, 0.25, 0.65)
    # Simulate 10000 values of pi from the prior
    set.seed(84735)
    chess_sim <- sample_n(chess,
                          size = input$num, weight = prior, replace = TRUE)
    chess_sim <- chess_sim %>%
      mutate(x = rbinom(input$num, size = 6, prob = pi))
    # Focus on simulations with x = 1
    win_one <- chess_sim %>% filter(x == 1)
    # Plot the posterior approximation
    ggplot(win_one, aes(x = pi)) + geom_bar()+
      labs(title = "Posterior Distribution", x="Pi", y="Count")+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14
                                    ,face="bold"),
            plot.title = element_text(size=22))
  })
  
  
  
}
