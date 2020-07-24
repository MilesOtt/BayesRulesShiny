library(shiny)
library(tidyverse)
plot_beta <- function(alpha, beta, mean = FALSE, mode = FALSE){
  
  
  p <- ggplot(data = data.frame(x = c(0, 1)),
              aes(x)) +
    stat_function(fun = dbeta,
                  n = 101,
                  args = list(shape1 = alpha,
                              shape2=beta)) +
    labs(x = expression(pi),
         y = expression(paste("f(",pi,")")))+
    
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14
                                  ,face="bold"),
          plot.title = element_text(size=22))
  
  
  if (mean == TRUE & mode == FALSE){
    mean <- alpha / (alpha + beta)
    
    p <- p +
      geom_segment(aes(x = mean, y = 0, 
                       xend = mean, 
                       yend = dbeta(mean, alpha, beta),
                       linetype = "mean")) +
      scale_linetype_manual(values = c(mean = "solid")) +
      theme(legend.title = element_blank())
  }
  
  if (mean == FALSE & mode == TRUE){
    mode <- (alpha - 1)/(alpha + beta - 2)
    
    p <- p +
      geom_segment(aes(x = mode, y = 0, 
                       xend = mode, 
                       yend = dbeta(mode, alpha, beta), 
                       linetype = "mode"))+
      scale_linetype_manual(values = c(mode = "dashed")) +
      theme(legend.title = element_blank())
    
    
  }
  
  if (mean == TRUE & mode == TRUE){
    mean <- alpha / (alpha + beta)
    mode <- (alpha - 1)/(alpha + beta - 2)
    
    
    p <- p +
      geom_segment(aes(x = mean, y = 0, 
                       xend = mean, 
                       yend = dbeta(mean, alpha, beta),
                       linetype = "mean")) +
      geom_segment(aes(x = mode, y = 0, 
                       xend = mode, 
                       yend = dbeta(mode, alpha, beta), 
                       linetype = "mode"))+
      scale_linetype_manual(values = c(mean = "solid", mode = "dashed")) +
      theme(legend.title = element_blank())
  }
  p
}



server<-function(input, output){
  observeEvent(list(input$alpha, input$beta, input$x, input$n, input$mean, input$mode),{
    a=as.integer(input$alpha)
    b=as.integer(input$beta)
    output$plot_beta<-renderPlot({
    plot_beta(a,b, input$mean, input$mode)
    })

      })

}
