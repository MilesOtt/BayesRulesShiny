library(shiny)
set.seed(84735)



ui<-fluidPage(
  titlePanel(
    h1("Chapter 3: The Beta-Binomial Bayesian Model of Michelle's Presidential
       Campaign", align = "center")
  ),
  # Sidebar to demonstrate various slider options ----
  sidebarPanel(
    
    sliderInput("nset", "Number of Pollers", value=0, min=1, max=10000),
    h6("The slider above changes the number of random samples based 
       on our weights for real and fake articles." 
    ),
    sliderInput("x", "Pollers in support of Michelle", value=0, min=1, max=10000),
    sliderInput("pi", "Pi", value=0, min=0, max=1),
    style = "position:fixed;width:inherit;"),
  
  
  mainPanel(
    div(
      strong("In section 2.2, we learned that Michelle decided to run for presidency
             in the next election and won the Iowa caucus. In recent developments, we 
learned that she secured her political party's nomination and as campaign manager, want
to conduct more polls in order to prepare for election season. 
             party's nomination"),
      style = "padding-right: 5%; padding-left: 5%"
      ),
    div(
      plotOutput('plot'),
      style = "padding-right: 5%; padding-left: 5%"
    ),
    p("This application was made by Elaona Lemoto in 
      supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
      is a textbook for undergarduates learning Bayesian statistics. "))
    )
