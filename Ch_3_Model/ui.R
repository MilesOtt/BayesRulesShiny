library(shiny)
library(tidyverse)
ui<-fluidPage(
  useShinyjs(),
  titlePanel(
    h1("Chapter 3: Getting to Know the Beta Model", align = "center")
  ),
  # Sidebar to demonstrate various slider options ----
  
  sidebarPanel(
    "To start, we need to tune the Beta prior which includes two parameters, 
    alpha and beta. Choose alpha and beta values from below and see how that alters
    prior distribution.",
    radioButtons("alpha", "Alpha", choices=c(1,3, 5, 7,20)),
    radioButtons("beta", "Beta", choices=c(1, 3, 5, 7, 20)),
    "Below, you can choose whether to see the mean and mode on the graph. 
    Before checking, try and see whether you can place it yourself!",

    radioButtons("mean", "Show the Mean ", choices = c("No mean"=FALSE, "Mean"=TRUE)),
    radioButtons("mode", "Show the Mode ", choices = c("No mode"=FALSE, "Mode"=TRUE)),
    #sliderInput("pi", "Pi", value=0, min=0, max=1),
    style = "position:fixed;width:inherit;"),
  
  
  
  mainPanel(
    div(
      strong("The goal of this app is to get you a bit more familiarized with the 
             beta model which will be discussed throughout chapter 3. In this exercise, 
             you can play around with the beta model's two parameters, alpha and beta. 
             You can also choose whether to have the mean and mode depicted on the graph."),
      style = "padding-right: 5%; padding-left: 5%"
      ),
    div(
      plotOutput('plot'),
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
