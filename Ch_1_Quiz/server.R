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




#--------------------------------------------------------------
plot_normal_normal<-function(mean,
                             prior_sd,
                             sample_mean=NULL,
                             sample_n=NULL,
                             sd=NULL, 
                             title_name=NULL,
                             prior =TRUE,
                             likelihood= TRUE,
                             posterior=TRUE){
  
  if (is.null(sample_mean) | is.null(sample_n) | is.null(sd))
    warning("To visualize the posterior,
            specify information about the data: sample mean, sample size, and standard deviation")
  
  x <- c(mean - 5*prior_sd, mean +5*prior_sd)
  g<-ggplot(data = data.frame(x = x),
            aes(x)) +
    stat_function(fun = dnorm, n = 1001, 
                  args = list(mean = mean, sd = prior_sd)) +
    labs(x = expression(theta),
         y = expression(paste("f(",theta,")")))+
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
  if (!is.null(sample_mean) & !is.null(sample_n) & !is.null(sd) & !is.null(title_name)) {
    title_name=as.character(title_name)
    g <- g +
      labs(title=paste(title_name))
  }
  
  
  if (prior == TRUE) {
    g <- g + stat_function(fun = dnorm,
                           args = list(mean = mean,
                                       sd = prior_sd)) +
      stat_function(fun = dnorm,
                    args = list(mean=mean,
                                sd = prior_sd),
                    geom = "area",
                    alpha=0.5,
                    aes(fill = "prior"))
  }
  

  
  
  #--------------------------------
  
  if (!is.null(sample_mean) & !is.null(sample_n) & !is.null(sd) & (likelihood != FALSE)){
    sd2<-(sd/sqrt(sample_n))
    
    g <- g + 
      stat_function(fun = dnorm, 
                    args = list(mean = sample_mean, sd= sd2))+
      stat_function(fun = dnorm,
                    args = list(mean = sample_mean, sd= sd2),
                    geom = "area",
                    alpha =0.5,
                    aes(fill = "(scaled) likelihood"))
    
  }
  
  #------------------------------
  
  if (!is.null(sample_mean) & !is.null(sample_n) & !is.null(sd) & posterior == TRUE) {
    
    mean_post<-((mean*sd2^2)+(sample_mean*prior_sd^2))/(prior_sd^2+(sd2^2))
    sd_post<-sqrt(sd^2 *sd2^2/(sd^2 +sd2^2))
    g <- g +
      stat_function(fun = dnorm,
                    args = list(mean = mean_post,
                                sd = sd_post)) +
      stat_function(fun = dnorm,
                    args = list(mean = mean_post,
                                sd = sd_post),
                    geom = "area",
                    alpha = 0.5,
                    aes(fill = "posterior"))
  }
  
  
  g
  
}
#--------------------------
# sim_beta_binomial<-function(alpha,
#                             beta,
#                             x,
#                             n,
#                             chains = 4,
#                             iter=1000){
# 
#   beta_bin_model <- "
#      data {
#   real<lower=0> alpha;
#   real<lower=0> beta;
#   int<lower=1> n;
#   int<lower=0, upper=n> x;
# }
# 
# parameters {
# real<lower=0, upper=1> pi;
# }
# 
# model {
# x ~ binomial(n, pi);
# pi ~ beta(alpha, beta);
# }
# "
# stan(
#   model_code = beta_bin_model,
#   data = list(x = x, n = n, alpha = alpha, beta = beta),
#   chains = chains, iter = iter)
# }



one_mh_iteration <- function(sigma, current){
  proposal <- rnorm(1, mean = current, sd = sigma)

  if(proposal < 0) {alpha <- 0}
  else {
    proposal_plaus <- dgamma(proposal,1,1) * dpois(0,proposal) 
    current_plaus <- dgamma(current,1,1) * dpois(0,current)
    alpha <- min(1, proposal_plaus / current_plaus)
    }
  next_stop <- sample(c(proposal, current), 
                      size = 1, 
                      prob = c(alpha, 1-alpha))
  # Return the results
  return(data.frame(proposal, alpha, next_stop))
  
}


mh_tour<- function(N, sigma){
  # 1. Start the chain at location 1 current <- 1
  # 2. Initialize the simulation
  lambda <- rep(0, N)
  # 3. Simulate N Markov chain stops
  for(i in 1:N){
    # Simulate one iteration
    sim <- one_mh_iteration(sigma = sigma, current = 1)
    # Record next location
    lambda[i] <- sim$next_stop
    # Reset the current location
    current <- sim$next_stop 
    }
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), lambda)) 
}
#--------------------------------------------------------------

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
     
   })
     
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
         alpha<-as.numeric(input$gamma_alpha)
         beta<-as.numeric(input$gamma_beta)
         shape<-alpha+input$poi_xn
         rate<-beta+input$poi_n
         plot_gamma_poisson(shape, rate, input$poi_n, input$poi_xn, "Our Gamma-Poisson Model")
       })
       })
     
#Chapter 5: Normal-Normal
     
     observeEvent(list(input$nmean, input$snd, input$nsample_n, 
                       input$nsample_mean, input$npop_sd),{
                        
          output$normal_normal<-renderPlot({
            mean<-as.numeric(input$nmean)
            sd<-as.numeric(input$nsd)
            plot_normal_normal(mean, sd, input$nsample_mean, input$nsample_n,
                               input$npop_sd, "Our Normal-Normal Model")
          })
       

                       })
     

#Chapter 6: Grid Approximation
    observeEvent(list(input$g6_alpha, input$g6_beta, input$g6_lambda, input$g6_grid),{
      alpha<-as.numeric(input$g6_alpha)
      beta<-as.numeric(input$g6_beta) 
      
      output$grid_p1<-renderPlot({
        shape<-alpha+input$g6_lambda
        rate<-beta+1
        plot_gamma_poisson(shape, rate, input$g6_lambda,1, "Our Gamma Prior and Poisson Likelihood")
      
      })
      
      output$grid_p2<-renderPlot({
        shape<-alpha+input$g6_lambda
        rate<-beta+1
        # Step 1: Define a grid of 501 lambda values
        lambda_grid <- seq(from = 0, to = input$g6_grid, length = 501) 
        grid_data <- data.frame(lambda_grid)
        # Step 2: Evaluate the prior & likelihood at each lambda
        grid_data <- grid_data %>%
          mutate(prior = dgamma(lambda_grid, alpha, beta)) %>% 
          mutate(likelihood = dpois(5, lambda_grid))
        # Step 3: Approximate the posterior
        grid_data <- grid_data %>%
          mutate(unnormalized = likelihood * prior) %>% 
          mutate(posterior = unnormalized / sum(unnormalized))
        # Set the seed
        set.seed(84735)
        # Step 4: sample from the discretized posterior
        post_sample <- sample_n(grid_data, size = 10000, weight = posterior, replace = TRUE)
        # Histogram of the grid simulation with posterior pdf
        ggplot(post_sample, aes(x = lambda_grid)) + geom_histogram(aes(y = ..density..), color = "white") +
          stat_function(fun = dgamma, args = list(shape,rate )) + lims(x = c(0, input$g6_grid))+
          
          theme(axis.text=element_text(size=16),
                axis.title=element_text(size=16,face="bold"),
                plot.title = element_text(size=22), 
                legend.text = element_text(size=16))+
          labs(title="Posterior Estimation Using Grid Approximation")
        
    
     
      
      })
    })
    
    #Chapter 7: Metropolis-Hastings
    
    
     observeEvent( input$mcmc_sd,{
       
       
       output$mcmc_trace_plot<-renderPlot({
         current<-1
         set.seed(4)
         proposal <- rnorm(1, mean = current, sd = input$mcmc_sd)
         proposal_plaus <- dgamma(proposal,1,2) * dpois(0,proposal)
         current_plaus <- dgamma(current,1,2) * dpois(0,current)
         alpha <- min(1, proposal_plaus / current_plaus)
         next_stop <- sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
         
       mh_simulation_1 <- mh_tour(N = 5000, sigma = input$mcmc_sd)
       
       ggplot(mh_simulation_1, aes(x = iteration, y = lambda)) + geom_line()+
         labs(title = "Tour")
       })
       
      output$mcmc_iteration<-renderPlot({
        
        current<-1
        set.seed(4)
        proposal <- rnorm(1, mean = current, sd = input$mcmc_sd)
        proposal_plaus <- dgamma(proposal,1,2) * dpois(0,proposal)
        current_plaus <- dgamma(current,1,2) * dpois(0,current)
        alpha <- min(1, proposal_plaus / current_plaus)
        next_stop <- sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
        
        mh_simulation_1 <- mh_tour(N = 5000, sigma = input$mcmc_sd)
       
       ggplot(mh_simulation_1, aes(x = lambda)) + geom_histogram(color = "white")+
         labs(title = "Iteration")
  
       
       })
       
       
       
     })
     

          
}






