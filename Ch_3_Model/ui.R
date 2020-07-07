library(shiny)
library(tidyverse)
ui<-fluidPage(
  useShinyjs(),
  titlePanel(
    h1("Chapter 3: Getting to the Basics of the Beta Binomial Bayesian Model", align = "center")
  ),
  # Sidebar to demonstrate various slider options ----
  
  sidebarPanel(
    "To start, we need to tune the Beta prior which includes two parameters, 
    alpha and beta. Choose alpha and beta values from below and see how that alters
    prior distribution.",
    radioButtons("alpha", "Alpha", choices=c(1,3, 5, 7,20)),
    radioButtons("beta", "Beta", choices=c(1, 3, 5, 7, 20)),
    "Now let's build our binomial likelihood. In order to do so, we need to look at the 
    number of samples and out of that number, the amount of success.",
    
    sliderInput("n", "Number of Random Samples", value=2, min=1, max=10),
    sliderInput("x", "Number of Successes", value=2, min=1, max=10),
    radioButtons("mean", "Show the Mean ", choices = c("No mean"=FALSE, "Mean"=TRUE)),
    radioButtons("mode", "Show the Mode ", choices = c("No mode"=FALSE, "Mode"=TRUE)),
    #sliderInput("pi", "Pi", value=0, min=0, max=1),
    style = "position:fixed;width:inherit;"),
  
  
  
  mainPanel(
    div(
      strong("The goal of this app is to get you a bit more familiarized with the 
             Beta Binomial model which will be discussed throughout chapter 3. In the exercise, 
             you can play around with the prior data, sample size (n), and number of
             successes (x) to have a more literal sense of what is happening"),
      style = "padding-right: 5%; padding-left: 5%"
      ),
    "Discuss Beta Prior Model",
    div(
      plotOutput('plot'),
      style = "padding-right: 10%; padding-left: 10%"),
    "Discuss Binomial Likelihood Model", 
    div(
      plotOutput('plot2'),
      style = "padding-right: 10%; padding-left: 10%"),
    "Discuss Beta Binomial Bayesian Model",
    div(
      plotOutput('plot3'),
      style = "padding-right: 10%; padding-left: 10%"),
    div(
      br(),
      br(),
      p("This application was made by Elaona Lemoto in 
        supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
        is a textbook for undergarduates learning Bayesian statistics. "),
      style = "padding-right: 5%; padding-left: 5%")
    )
    )
