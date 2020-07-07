library(shiny)
library(tidyverse)
lot_beta_binomial <- function (alpha,
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
server<-function(input, output){
  observeEvent(list(input$alpha, input$beta, input$x, input$n),{
    if (input$x>input$n){
        shinyjs::disable('x');
    }else{
      shinyjs::enable('x')
      output$plot<-renderPlot({
        a=as.integer(input$alpha)
        b=as.integer(input$beta)
        lot_beta_binomial(a,b,x=input$x,n=input$n)
      })
    }
    
  
  })
}
