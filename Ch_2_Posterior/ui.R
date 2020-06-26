library(shiny)
set.seed(84735)



ui<-fluidPage(
  titlePanel("Chapter 2.1.5: Posterior Simulation"),
  mainPanel(
    p("Recall from Chapter 2 where we were
interested in distinguishing fake news articles 
      from real news articles. After looking through our 
      sample of 150 articles, we found our prior probability 
      to be 40% for fake articles and 60% for real articles. 
      Now in this section, we are interested in providing a 
      connection between articles posted online and their corresponding
      probabilites. To start, we created a dataframe for types of articles;
real and fake, and defined our prior model which is mentioned earlier
0.6 and 0.4. Since we want to simulate articles we might see on social 
media, we are randomly sampling articles from the our article dataframe 
      with the added weights of our priors.")
  ),
  sliderInput("num", "Sample Number", value=0, min=10, max=10000),
  mainPanel(
    p("The slider above changes the number of random samples we take 
    given our types of articles and their prior probability. Below is a
      frequency plot of each article. What do you notice as n increases?")
  ),
  plotOutput("plot"),
  mainPanel(
    br(),
    br(),
    br(),
    br(),
    br(),
      p("This application was made by Elaona Lemoto in 
        supplement to Bayes Rules! by Miles Ott, Mine Dogucu, and ---- . Bayes Rules!
        is a textbook for undergarduates learning Bayesian statistics. ")
  )
  )
