library(shiny)
library(tidyverse)

server<-function(input, output){
  data<-reactive({
    validate(
      need(input$p1+input$p2+input$p3==1, "Please make sure your inputs add to 1!")
    )
   c(input$p1,input$p2,input$p3)
  })
  observeEvent(list(input$num, input$p1, input$p2, input$p3, input$range),{


  output$plot<-renderPlot({
    # Define possible win probabilities
    chess <- data.frame(pi = c(0.2, 0.5, 0.8))
    # Define prior model
    prior <- c(data())
   
    # Simulate 10000 values of pi from the prior
    set.seed(84735)
    chess_sim <- sample_n(chess,
                          size = input$num, weight = prior, replace = TRUE)
    chess_sim <- chess_sim %>%
      mutate(x = rbinom(input$num, size = 6, prob = pi))
    #Creating prop
    chess_sim_prop<-chess_sim %>% 
      group_by(pi) %>% 
      count(pi) %>% 
      mutate(prop=n/input$num)
      
    
    ggplot(chess_sim_prop, aes(x = pi, y=prop)) + 
      geom_col()+
      labs(title = "Prior Distribution", x="Pi", y="Prop")+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14
                                    ,face="bold"),
            plot.title = element_text(size=22))+
      scale_x_continuous(breaks = c(0.25, 0.5, 0.75),labels = c("0.2", "0.5", "0.8"))
    #Change the x-axis
  })
  
  
  
  output$plot2<-renderPlot({
    # Define possible win probabilities
    chess <- data.frame(pi = c(0.2, 0.5, 0.8))
    # Define prior mode
    prior <- c(data())
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

    prior <- c(data())

    # Simulate 10000 values of pi from the prior
    set.seed(84735)
    chess_sim <- sample_n(chess,
                          size = input$num, weight = prior, replace = TRUE)
    chess_sim <- chess_sim %>%
      mutate(x = rbinom(input$num, size = 6, prob = pi))
    
    win <- chess_sim %>%
      filter(x==input$range) %>% 
      group_by(pi) %>% 
      count(pi) %>% 
      mutate(prop=n/input$num)
    
  #Creating prop
      
    # Plot the posterior approximation
    ggplot(win, aes(x = pi, y=prop)) +
      geom_col()+
      labs(title = "Posterior Distribution Given The Number \n of Won Games", x="Pi", y="Prop")+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14
                                    ,face="bold"),
            plot.title = element_text(size=22))+
      scale_x_continuous(breaks = c(0.25, 0.5, 0.75),labels = c("0.2", "0.5", "0.8"))
  })
  
  })
  
}
