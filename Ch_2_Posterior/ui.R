library(shiny)
set.seed(84735)



ui<-fluidPage(
  titlePanel(
    h1("Chapter 2.1.5: Posterior Simulation with Article Verification", align = "center")
  ),
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      "The slider changes the number of random samples we take of articles overall.",
   
  sliderInput("num", "Number of Random Samples", value=5000, min=1, max=10000),
  
  "Below, you can alter the prior probabilities for articles that have an exclamation mark
     in their title. Again, the default prior probabilities for articles that are fake and that have an
     exclamation mark is 28% and 2% for real articles. Therefore, the values need to remain 
     between 0 and 1. Don't forget that these values do not include the probabilities for articles 
     without exclamation points in their titles.",
  numericInput("p1", "P(Fake Articles w/ Exclamation Marks) ", value=0.28,min=0,max=1), 
  numericInput("p2", "P(Real Articles w/ Exclamation Marks) ",value=0.02, min=0,max=1), 
  submitButton("Submit"),
style = "position:fixed;width:inherit;"
),
  
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
    p("
To better visualize this simulation, below is our prior distribution given
our random sample numbers. Confirm that the prior distribution 
is similar to the the 
      weights that we defined earlier."),
    style = "padding-right: 5%; padding-left: 5%"),

    div(
      plotOutput('plot'),
      style = "padding-right: 10%; padding-left: 10%"),
    div(
      p("Now in our analysis of real and fake articles, we created a likelihood 
      variable tying together exclamation usage and an articles chance of being 
        real or fake. In this case, we defined any article with an exclamation mark 
        to have a 28% chance of being fake and 2% chance of being real. You can alter these 
      prior probabilities in the side panel to see how the likelihood distribution and the posterior distribution would alter."),
      style = "padding-right: 5%; padding-left: 5%"),
  div(
        plotOutput('plot2'),
        style = "padding-right: 10%; padding-left: 10%"
      ),
  div(
    p("Finally, we have our posterior distribution. The graph below shows the proportion of 
      real and fake articles with exclamation usage and it seems as thought Based on our prior probabilities, the 
      posterior distribution as shown in the graph below will change. "),
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
