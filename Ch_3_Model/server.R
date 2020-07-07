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
         y = expression(paste("f(",pi,")")))
  
  
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
plot_beta_binomial <- function (alpha,
                               beta,
                               x = NULL,
                               n = NULL,
                               prior = TRUE,
                               likelihood = TRUE,
                               posterior = TRUE){
  if (is.null(x) | is.null(n))
    warning("To visualize the posterior,
            specify data x and n")
  
  g <- ggplot(NULL, aes(x = c(0, 1))) +
    labs(x = expression(pi),
         y = "density",
         "The Beta Binomial Model of Michelle's Campaign ") +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"),
          plot.title = element_text(size=22), 
          legend.text = element_text(size=16))+
    scale_fill_manual("",
                      
                      values = c(prior = "gold1",
                                 
                                 likelihood = "cyan2",
                                 posterior = "cyan4"),
                      breaks = c("prior",
                                 "(scaled) likelihood",
                                 "posterior"))
  if (prior == TRUE) {
    g <- g +
      stat_function(fun = dbeta,
                    args = list(shape1 = alpha,
                                shape2 = beta)) +
      stat_function(fun = dbeta,
                    args = list(shape1 = alpha,
                                shape2 = beta),
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "prior"))
  }
  
  if (!is.null(x) & !is.null(n)) {
    alpha_post <- alpha + x
    beta_post <- beta + n - x
    x_data <- x
    like_scaled <- function(x) {
      like_fun <- function(x) {
        dbinom(x = x_data, size = n, prob = x)
      }
      scale_c <- integrate(like_fun, lower = 0, upper = 1)[[1]]
      like_fun(x)/scale_c
    }
  }
  if (!is.null(x) & !is.null(n) & (likelihood != FALSE)) {
    g <- g +
      stat_function(fun = like_scaled) +
      stat_function(fun = like_scaled,
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "(scaled) likelihood"))
  }
  if (!is.null(x) & !is.null(n) & posterior == TRUE) {
    g <- g +
      stat_function(fun = dbeta,
                    args = list(shape1 = alpha_post,
                                shape2 = beta_post)) +
      stat_function(fun = dbeta,
                    args = list(shape1 = alpha_post,
                                shape2 = beta_post),
                    geom = "area", alpha = 0.5,
                    aes(fill = "posterior"))
    
  }
  g
  
} # end of function`

plot_binomial_likelihood <-function(x, 
                                    n, 
                                    mle = FALSE){
  
  g <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
    stat_function(fun = dbinom, args = list(x = x, size = n)) +
    labs(x = expression(pi),
         y = expression(paste("L(",pi,"|(X=", x, "))")))
  
  
  
  if (mle == TRUE){
    
    max <- x/n
    
    success <- x # the line segment does not work since x is an argument in ggplot
    
    g <- g +
      
      geom_segment(aes(x = max, 
                       xend = max, 
                       y = 0, 
                       yend = dbinom(success, n, max)),
                   color = "cyan4") +
      theme(legend.position = "none") 
    
    
  }
  
  g
  
}# end of function


server<-function(input, output){
  observeEvent(list(input$alpha, input$beta, input$x, input$n, input$mean, input$mode),{
    a=as.integer(input$alpha)
    b=as.integer(input$beta)
    output$plot<-renderPlot({
    plot_beta(a,b, input$mean, input$mode)
    })
  output$plot2<-renderPlot({
    plot_binomial_likelihood(input$x, input$n, mle=TRUE)
      
    })
  output$plot3<-renderPlot({
    plot_beta_binomial(a,b,input$x, input$n)
  })
      })

}
