library(shiny)
library(shinyjs)
library(tidyverse)
set.seed(84735)



ui<-fluidPage(
  useShinyjs(),
  titlePanel(
    h1("Chapter 3: The Beta-Binomial Bayesian Model of Michelle's Presidential
       Campaign", align = "center")
  ),
  # Sidebar to demonstrate various slider options ----
  
  sidebarPanel(
    "The Alpha and Beta choices below are the parameters for the prior distribution. They 
    must be greater than 0 and for simplicities sake, we chose values for each 
    parameter so you can see how they might alter the prior and posterior distributions",
    radioButtons("alpha", "Alpha", choices=c(45, 55, 65, 75)),
    radioButtons("beta", "Beta", choices=c(45, 55, 65,75)),
    "Now the sliders here are to gain more insight on pi, better known
    as Michelle's election support. In order to do so, we need to conduct
    polls (n) and record the number of pollers interested in voting
    for Michelle (X). Remember that number of pollers must be greater than or equal to 
    Pollers in support of Michelle. ",
    sliderInput("n", "Number of Respondents to the Poll", value=50, min=1, max=100),
    sliderInput("x", "Number of Respondents in Support of Michelle", value=30, min=1, max=100),
    #sliderInput("pi", "Pi", value=0, min=0, max=1),
    style = "position:fixed;width:inherit;"),
  
  
  
  mainPanel(
    div(
      strong("In section 2.2, we learned that Michelle decided to run for presidency
             in the next election and won the Iowa caucus. In recent developments, we 
learned that she secured her political party's nomination and as campaign manager, you want
to conduct more polls in order to gain more insight on voters and prepare for election season.
             Play around with the inputs to your right and see how prior data, number
 of respondents to the polls,
             and number of number of respondents in support of Michelle, alters the 
             perceived support for Michelle's candidacy in the upcoming election. 
             "),
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
