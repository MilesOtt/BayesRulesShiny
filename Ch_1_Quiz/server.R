library(shiny)
library(tidyverse)
library(shinyjs)

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
#-----------------------------------------------------------


plot_beta_binomial <- function (alpha,
                                beta,
                                x = NULL,
                                n = NULL,
                                title_name=NULL,
                                prior = TRUE,
                                likelihood = TRUE,
                                posterior = TRUE
                                ){
  if (is.null(x) | is.null(n))
    warning("To visualize the posterior,
            specify data x and n")
  #MORE GRAPHING
  
  g <- ggplot(NULL, aes(x = c(0, 1))) +
 
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"),
          plot.title = element_text(size=22), 
          legend.text = element_text(size=16))+
    scale_fill_manual("",
                      values = c(prior = "gold1",
                                 likelihood = "cyan2",
                                 posterior = "cyan4"),
                      breaks = c("prior",
                                 "likelihood",
                                 "posterior"),
                      labels=c(
                        "prior",
                        "(scaled) likelihood", 
                        "posterior"
                      ))+
    labs(x = expression(pi),
         y = "density")
  #GRAPH 
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
  if (!is.null(x) & !is.null(n) & !is.null(title_name)) {
    title_name=as.character(title_name)
    g <- g +
      labs(title=paste(title_name))
  }
  #GRAPHING
  if (!is.null(x) & !is.null(n) & (likelihood != FALSE)) {
    g <- g +
      stat_function(fun = like_scaled) +
      stat_function(fun = like_scaled,
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "likelihood"))
  }
  #GRAPHHIN
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

#-------------------------------------------------------

plot_beta_binomial_day1 <- function (alpha,
                                beta,
                                x = NULL,
                                n = NULL,
                                prior = TRUE,
                                likelihood = TRUE,
                                posterior = TRUE){
  if (is.null(x) | is.null(n))
    warning("To visualize the posterior,
            specify data x and n")
  #MORE GRAPHING
  
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
                                 "likelihood",
                                 "posterior"),
                      labels=c(
                        "prior",
                        "(scaled) likelihood", 
                        "posterior"
                      ))+
    labs(title = "First Day Beta Binomial Model")
  #GRAPH 
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
  
  #GRAPHING
  if (!is.null(x) & !is.null(n) & (likelihood != FALSE)) {
    g <- g +
      stat_function(fun = like_scaled) +
      stat_function(fun = like_scaled,
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "likelihood"))
  }
  #GRAPHHIN
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

#-------------------------------------------------------

plot_beta_binomial_day2 <- function (alpha,
                                     beta,
                                     x = NULL,
                                     n = NULL,
                                     prior = TRUE,
                                     likelihood = TRUE,
                                     posterior = TRUE,
                                     title_name=TRUE){
  if (is.null(x) | is.null(n))
    warning("To visualize the posterior,
            specify data x and n")
  #MORE GRAPHING
  
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
                                 "likelihood",
                                 "posterior"),
                      
                      labels=c("prior (posterior: day1)", 
                               "(scaled) likelihood", 
                               "posterior: day 2"))
  #GRAPH 
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
  
  #
  
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
  
  #GRAPHING
  if (!is.null(x) & !is.null(n) & (likelihood != FALSE)) {
    g <- g +
      stat_function(fun = like_scaled) +
      stat_function(fun = like_scaled,
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "likelihood"))
  }
  #GRAPHHIN
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
  if (!is.null(x) & !is.null(n) & posterior == TRUE & title_name==TRUE) {
    g <- g +
     labs(title=paste("Second Day Updated Beta Prior: \n alpha = ",alpha, "beta = ", beta))
    
    
  }
  g
  
}# end of function`

#----------------------------------------------------------


plot_beta_binomial_day3 <- function (alpha,
                                     beta,
                                     x = NULL,
                                     n = NULL,
                                     prior = TRUE,
                                     likelihood = TRUE,
                                     posterior = TRUE,
                                     title_name = TRUE){
  if (is.null(x) | is.null(n))
    warning("To visualize the posterior,
            specify data x and n")
  #MORE GRAPHING
  
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
                                 "likelihood",
                                 "posterior"),
                      
                      labels=c("prior (posterior: day2)", 
                               "(scaled) likelihood", 
                               "posterior: day 3"))
  #GRAPH 
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
  
  #
  
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
  
  #GRAPHING
  if (!is.null(x) & !is.null(n) & (likelihood != FALSE)) {
    g <- g +
      stat_function(fun = like_scaled) +
      stat_function(fun = like_scaled,
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "likelihood"))
  }
  #GRAPHHIN
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
  
  if (!is.null(x) & !is.null(n) & posterior == TRUE & title_name==TRUE) {
    x="title_name"
    g <- g +
      labs(title=paste("Third Day Updated Beta Prior: \n alpha = ",alpha, "beta = ", beta))
    
    
  }
  g
  
} # end of function`

#------------------------------------------------------

plot_gamma_poisson <- function (shape, rate,
                                sum_x = NULL,
                                n = NULL,
                                title_name=NULL,
                                prior = TRUE,
                                likelihood = TRUE,
                                posterior = TRUE){
  
  if (is.null(sum_x) | is.null(n))
    warning("To visualize the posterior,
            specify information about the data: sum_x and n")
  
  
  
  x_min <- min(qgamma(1e-05, shape, rate),
               qgamma(1e-05,
                      shape + sum_x,
                      rate + n),
               qgamma(1e-05, sum_x+1, n))
  x_max <- max(qgamma(0.99999, shape, rate),
               qgamma(0.99999, shape + sum_x,
                      rate + n),
               qgamma(0.99999, sum_x+1, n))
  g <- ggplot(NULL, aes(x = c(x_min, x_max))) +
    labs(x = expression(lambda),
         y = "density") +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"),
          plot.title = element_text(size=22), 
          legend.text = element_text(size=16))+
    scale_fill_manual("",
                      values = c(prior = "gold1",
                                 `(scaled) likelihood` = "cyan2",
                                 posterior = "cyan4"),
                      breaks = c("prior",
                                 "(scaled) likelihood",
                                 "posterior"))
  if (!is.null(sum_x) & !is.null(n) & !is.null(title_name)) {
    title_name=as.character(title_name)
    g <- g +
      labs(title=paste(title_name))
  }
  
  if (prior == TRUE) {
    g <- g + stat_function(fun = dgamma,
                           args = list(shape = shape,
                                       rate = rate)) +
      stat_function(fun = dgamma,
                    args = list(shape = shape,
                                rate = rate),
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "prior"))
  }

  
  if (!is.null(sum_x) & !is.null(n)) {
    shape_post <- shape + sum_x
    rate_post <- rate + n
    like_scaled <- function(x) {
      dgamma(x, shape = sum_x + 1, rate = n)
    }
  }
  if (!is.null(sum_x) & !is.null(n) & (likelihood != FALSE)) {
    g <- g +
      stat_function(fun = like_scaled) +
      stat_function(fun = like_scaled,
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "(scaled) likelihood"))
  }
  if (!is.null(sum_x) & !is.null(n) & posterior == TRUE) {
    g <- g +
      stat_function(fun = dgamma,
                    args = list(shape = shape_post,
                                rate = rate_post)) +
      stat_function(fun = dgamma,
                    args = list(shape = shape_post,
                                rate = rate_post),
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "posterior"))
  }
  
  
  g
}



server<-function(input, output, session) {
 output$answer<-renderPrint({ 
   req(input$q1)
   req(input$q2)
   req(input$q3)
   req(input$q4)
   asInt1=as.integer(input$q1)
     asInt2=as.integer(input$q2)
     asInt3=as.integer(input$q3)
     asInt4=as.integer(input$q4)
     x=asInt1+asInt2+asInt3+asInt4
   if(x<=5&x>=4){
      "Your current thinking are fairly frequentist!";
    } else if(x>=9){
      "According to your answers, you already think like a Bayesian!";
    } else if(x<=8 |x>=6){
      "You don't take sides; you shared ideas from frequentists and Bayesians";
    }
    })

 
 #CHAPTER 2 POSTERIOR SIMULATION

 data_1<-reactive({
   validate(
     need(input$c2p1<=1, "Make sure your inputs are less than 1!")
   )
   input$c2p1

 })

 data_2<-reactive({
   validate(
     need(input$c2p2<=1, "Make sure your inputs are less than 1!")
   )
   input$c2p2

 })

 observeEvent(list(input$c2p2, input$c2p1, input$c2num),{
   output$plot<-renderPlot({
     # Define possible articles
     article <- data.frame(type = c("real", "fake"))
     # Define the prior model
     prior <- c(0.6, 0.4)
     article_sim <- sample_n(article,
                             size = input$c2num,weight = prior, replace = TRUE)
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
                             size = input$c2num,weight = prior, replace = TRUE)
     #Likelihood
     article_sim <- article_sim %>%
       mutate(likelihood =
                case_when(type == "fake" ~ data_1(),type == "real" ~ data_2() ))
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
                             size = input$c2num,weight = prior, replace = TRUE)
     #Likelihood
     article_sim <- article_sim %>%
       mutate(likelihood =
                case_when(type == "fake" ~  data_1(),type == "real" ~ data_2() ))
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

 })

 #Chapter 3 Beta Model

   observeEvent(list(input$alpha, input$beta, input$x, input$n, input$mean, input$mode),{
     a=as.integer(input$alpha)
     b=as.integer(input$beta)


     output$plot_b<-renderPlot({
       plot_beta(a,b, input$mean, input$mode)
     })

   })

   #Chapter 3 Beta Binomial Model

   observeEvent(list(input$c3alpha, input$c3beta, input$c3x, input$c3n),{
     if (input$c3x>input$c3n){
       shinyjs::disable('c3x');
     }else{
       shinyjs::enable('c3x')
       output$plot_bb<-renderPlot({
         c3a=as.integer(input$c3alpha)
         c3b=as.integer(input$c3beta)
         plot_beta_binomial(c3a,c3b,x=input$c3x,n=input$c3n, "The Beta Binomial Model of Michelle's Campaign" )
       })
     }
     
     #Chapter 4 Sequential Bayesian Analysis
     
     observeEvent(list(input$ch4_alpha, input$ch4_beta, input$ch4_x1, input$ch4_n1,
                       input$ch4_x2 ,input$ch4_n2,input$ch4_x3, input$ch4_n3),{
                         
                         warn_ch4p1<-reactive({
                           validate(
                             need(input$ch4_x1<=input$ch4_n1, "Make sure number of successes is less than or equal to the number of trials!")
                           )
                           return(input$ch4_x1)
                           
                         })
                         
                         warn_ch4p2<-reactive({
                           validate(
                             need(input$ch4_x1<=input$ch4_n1, "Make sure number of successes is less than or equal to the number of trials!")
                           )
                           return(input$ch4_n1)
                           
                         })
                         
                         warn_ch4p1_day2<-reactive({
                           validate(
                             need(input$ch4_x2<=input$ch4_n2 ,"Make sure number of successes is less than or equal to the number of trials!")
                           )
                           return(input$ch4_x2)
                           
                         })
                         
                         warn_ch4p2_day2<-reactive({
                           validate(
                             need(input$ch4_x2<=input$ch4_n2, "Make sure number of successes is less than or equal to the number of trials!")
                           )
                           return(input$ch4_n2)
                           
                         })
                         warn_ch4p1_day3<-reactive({
                           validate(
                             need(input$ch4_x3<=input$ch4_n3, "Make sure number of successes is less than or equal to the number of trials!")
                           )
                           return(input$ch4_x3)
                           
                         })
                         
                         warn_ch4p2_day3<-reactive({
                           validate(
                             need(input$ch4_x3<=input$ch4_n3, "Make sure number of successes is less than or equal to the number of trials!")
                           )
                           return(input$ch4_n3)
                           
                         })
       output$plot_ch41<-renderPlot({
       ch4_alpha=as.integer(input$ch4_alpha)
       ch4_beta=as.integer(input$ch4_beta)
       plot_beta_binomial_day1(ch4_alpha, ch4_beta, warn_ch4p1(), warn_ch4p2())
       })
       
       output$plot_ch42<-renderPlot({
         ch4_alpha=as.integer(input$ch4_alpha)
         ch4_beta=as.integer(input$ch4_beta)
         alpha_prior <- ch4_alpha + input$ch4_x1
         beta_prior <- ch4_beta + input$ch4_n1 - input$ch4_x1
         plot_beta_binomial_day2(alpha_prior, beta_prior, warn_ch4p1_day2(), warn_ch4p2_day2())
       })
       output$plot_ch43<-renderPlot({
         ch4_alpha=as.integer(input$ch4_alpha)
         ch4_beta=as.integer(input$ch4_beta)
         alpha_prior <- ch4_alpha + input$ch4_x1
         beta_prior <- ch4_beta + input$ch4_n1 - input$ch4_x1
         alpha_prior2 <- alpha_prior + input$ch4_x2
         beta_prior2 <- beta_prior + input$ch4_n2 - input$ch4_x2
         plot_beta_binomial_day3(alpha_prior2, beta_prior2, warn_ch4p1_day3(), warn_ch4p2_day3())
       })
       })
     
  #Chapter 4: Balancing Bayesian Models
     
     observeEvent(list(input$balance_samplen, input$balance_alpha, input$balance_beta
                       ),{
       output$balance_bayesian<-renderPlot({
         sample_n=as.integer(input$balance_samplen)
         x=0.6*sample_n
         plot_beta_binomial(input$balance_alpha, input$balance_beta, x, sample_n,"Our Beta Binomial Model")
       })
     })
#Chapter 5: Gamma-Poisson
     
     observeEvent(list(input$gamma_alpha,input$gamma_beta, input$poi_n, input$poi_xn),{
       output$gamma_poisson<-renderPlot({
         alpha<-as.integer(input$gamma_alpha)
         beta<-as.integer(input$gamma_beta)
         shape<-alpha+input$poi_n
         rate<-beta+input$poi_n
         plot_gamma_poisson(shape, rate, input$poi_n, input$poi_xn, "Our Gamma-Poisson Model")
       })
       })
     
#Chapter 5: Normal-Normal
     
     observeEvent(list(input$normal_mean, input$normal_sd, input$normal_samplesize, 
                       input$normal_samplemean),{
          output$normal_normal<-renderPlot({
            mean<-as.integer(input$normal_sd)
            sd<-as.integer(input$normal_sd)
          })
          })
     })

}






