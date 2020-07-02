library(shiny)
set.seed(84735)



ui<-fluidPage(
  titlePanel(
    h1("Chapter 2.1.5: Posterior Simulation with Article Verification", align = "center")
  ),
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
   
  sliderInput("num", "Random Sample Number", value=0, min=1, max=10000),
    h6("The slider above changes the number of random samples based 
on our weights for real and fake articles." 
),
style = "position:fixed;width:inherit;",
"Inputs"),
  
  mainPanel(
    div(
      strong("Recall from Chapter 2 where we were
interested in distinguishing fake news articles from real news articles. 
         After looking through our sample of 150 articles, 
         we found our prior probability to be 40% for fake articles and 60% for real 
         articles.  In this activity, we are interested in providing a connection 
         between articles posted online and their corresponding probabilites.
         To start, we have a dataframe for types of articles;
         real and fake, and defined our prior model as the probabilities
         found before. Since we want to simulate articles we might see on social 
         media, we are randomly sampling articles from the our article dataframe with
         their weighted probabilities. Throughout this exercise, you will look
             at the different distributions and below as you change the number
             or random samples "),
      style = "padding-right: 5%; padding-left: 5%"
      ),
    br(),
    div(
    p("Now in our analysis of real and fake articles, we created a likelihood 
      variable tying together exclamation usage and an articles chance of being 
real or fake. In this case, we defined any article with an exclamation mark 
to have a 28% chance of being fake and 2% of being real.
To better visualize this simulation, below is our prior distribution given
our random sample numbers. Confirm that the prior distribution 
is similar to the the 
      weights that we defined earlier."),
    style = "padding-right: 5%; padding-left: 5%"),

    div(
      plotOutput('plot'),
      style = "padding-right: 10%; padding-left: 10%"),
    div(
      p("This here is the likelihood distribution."),
      style = "padding-right: 5%; padding-left: 5%"),
  div(
        plotOutput('plot2'),
        style = "padding-right: 10%; padding-left: 10%"
      ),
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
