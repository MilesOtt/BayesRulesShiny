library(shiny)
set.seed(84735)



ui<-fluidPage(
  titlePanel(
    h1("Chapter 2.3.6: More Posterior Simulation with Kasparov's Chess Skills", align = "center")
  ),
  # Sidebar to demonstrate various slider options ----
  sidebarPanel(
    sliderInput("num", "Number of Random Samples ", value=0, min=1, max=10000),
    h6("The slider above changes the number of random samples based 
       on our weights for real and fake articles." 
    ),
    numericInput("p1", "f(0.2) = ", value=0.1,min=0,max=1), 
    numericInput("p2", "f(0.6) = ",value=0.25, min=0,max=1), 
    numericInput("p3", "f(0.8) = ", value=0.65, min=0,max=1), 
    h6("The numeric inputs above can change the prior weights for each value of pi. 
       These inputs range from 0 to 1 and MUST add up to 1. "),
    sliderInput("range", "Number of Won Games", min=0, max=6, value=c(0,6)),
    h6("Change the slider above to change the posterior distribution given the number of games 
       Kasparov won."),
    submitButton("Submit"), 
    #Create a widget to input what the priors are and their weights 
    style = "position:fixed;width:inherit;"),
  
  
  mainPanel(
    div(
      strong("In this acitvity, we are interested in supplementing the Bayesian analysis of 
             Kasparov's chess skills highlighted through the in-text R code. Recall from 2.3.1
             where we introduce Gary Kasparov (chess champion) and his six-game chess match against
             IBM supercomputer Deep Blue. We defined our prior model to be pi (the probability of 
             him defeating Deep Blue) and as usual with prior models, we identified the values that
            pi can take as; 0.2, 0.6, and 0.8 well, as the assigned prior weight to each value; 
0.10 for 0.2, 0.25 for 0.6, and 0.65 for 0.8. As the assigned weights are default, you can alter the prior 
weights from the numeric inputs to the left. Remember that the weights must add up to 1.
             "),
      style = "padding-right: 5%; padding-left: 5%"
      ),
    br(),
    div(
      p("We first want to simulate random samples of the six-game series. Therefore, we created 
        a dataframe for pi called 'chess' and a separate dataframe for prior weights called 'prior'.
        We want to simulate and record Kasparov's number of wins, x, given the number of random
samples we take of the six-game series. Below is a visual for the prior model given our inputs and random
number of samples."),
      #Simulated win outcomes and talk more about the graphic 
      style = "padding-right: 5%; padding-left: 5%"),
    
    div(
      plotOutput('plot'),
      style = "padding-right: 10%; padding-left: 10%"),
    div(
      p("Here we have the simulated frequency of won games, or x, separated by 
        each level of pi, or the probabilty that Kasparov will win against Deep Blue.
For example, if Kasparov were to have a 20% probability of winning 
against Deep Blue, this is the frequency distribution of simulated games he would have won 
against the IBM computer. Depending on the prior weights you set for each value of pi, 
the distributions below will be different. Now having played around with the pi and their 
prior weights, what can you tell about pi and the simulated match outcomes, x? "),
      #Given that he won one game.... 
      br(),
      style = "padding-right: 5%; padding-left: 5%"),
    div(
      plotOutput('plot2'),
      style = "padding-right: 10%; padding-left: 10%"
    ),
    div(
  
      p("For this last graph, we have the shared outcome that Kasparov won one game. What
        can you tell about this graph? Does it resemble information about the posterior
        distribution that we gathered in 2.3.4?"),
      br(),
      style = "padding-right: 5%; padding-left: 5%"),
    div(
      plotOutput('plot3'),
      style = "padding-right: 10%; padding-left: 10%"
    ),
    br(),
    br(),
    br(),
    p("This application was made by Elaona Lemoto in 
      supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
      is a textbook for undergarduates learning Bayesian statistics. "))
  )
