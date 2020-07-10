library(shiny)

ui<-fluidPage(
  navbarPage("Navbar",
             tabPanel("Boop"),
             navbarMenu("Links",
                        tabPanel("Chapter 1 Quiz",
                                 titlePanel("What type of Statistician are you?"),
                                 mainPanel(
    p("Take this quiz to see whether you share more frequentist ideas,
bayesian ideas, or both!"),
    br()
    ),
  fluidRow(
    column(6, 
           radioButtons("q1", "When flipping a coin, we say that 'the probability of flipping heads is 0.5'. 
               How do you interpret this probability?",choices=list("If I flip this coin over and over, 
                                                                    roughly 50% will be Heads."=1, "Heads and Tails are equally plausible."=3,
                                                                    "Both a and b make sense."=2),selected=character(0)),
             radioButtons("q2", "A meterologist warns that 'there's a 0.1 probability
                          of rain today.' How do you interpret this probability?",
                          choices =list("If we observe today over and over, it will rain on roughly 10% of todays.
                                        "=1,"It's equally likely to rain or not rain."=3,
                                        "The meteorologist's calculation is wrong. It will either rain or not rain, 
                                        thus the probability of rain can only be 0 or 1"=1),selected=character(0))),
    column(6, 
            radioButtons("q3", "Consider two claims. (1) Zuofu claims that he 
can predict the out- come of a coin flip. To test his claim, you flip a 
                           fair coin 10 times and he correctly predicts all 10! (2) Kavya claims that 
                           she can dis- tinguish Dunkin’ Donuts1 coffee from Starbucks coffee. To test 
                           her claim, you give her 10 coffee samples and she correctly identifies the 
                           origin of each! In light of these experiments, what do you conclude? ", 
                           choices = list("You’re more confident in Kavya’s claim than Zuofu’s claim."
                                          =3,"The evidence supporting Zuofu’s claim is just as strong as the
                                          evidence supporting Kavya’s claim."=1),selected=character(0)),
           radioButtons("q4","Suppose that during a recent doctor’s visit, you tested positive for a very rare disease. 
                        If you only get to ask the doctor one question, which would it be?", 
                        choices= list("What’s the chance that I actually have the disease?"=3,
                                      "If in fact I don’t have the disease, what’s the chance that 
                                      I would’ve gotten this positive test result?"=1),selected=character(0)))),

  verbatimTextOutput("null"),
  submitButton("Submit"),
verbatimTextOutput("answer"),
mainPanel(
  br(),
  br(),
  br(),
  br(),
  p("This application was made by Elaona Lemoto in 
    supplement to Bayes Rules! by Miles Ott, Mine Dogucu, and ---- . Bayes Rules!
      is a textbook for undergarduates learning Bayesian statistics. ")
)),
tabPanel("Chapter 2 Posterior",
         titlePanel(
           h1("Chapter 2.1.5: Posterior Simulation with Article Verification", align = "center")
         ),
         # Sidebar to demonstrate various slider options ----
         sidebarPanel(
           "The slider changes the number of random samples we take of articles overall.",
           
           sliderInput("num", "Number of Random Samples", value=5000, min=1, max=10000),
           
           "Below, you can alter the prior probabilities for type of articles that have an exclamation point
           in their title. Again, the default prior probabilities as they are in the book for articles that are fake and that have an
           exclamation point is 28% and 2% of real news articles use exclamation points. Therefore, the values need to remain 
           between 0 and 1.",
           #Change wroding to reference book -  We're using the example from the book.
           
           
           numericInput("p1", "P(Exclamation Point | Fake Article ) ", value=0.28,min=0,max=1), 
           numericInput("p2", "P(Exclamation Point | Real Article ) ",value=0.02, min=0,max=1), 
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
             #We're going to keep the prior probabilities fixed 
             #This is what we observed in the data
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
             #What is the likelihood of exclamation point given fake news articles and 
             #what is the prob, of the vice versa 
             
             p("Now in our analysis of real and fake articles, we created a likelihood 
               variable tying together exclamation point usage and types of articles.
               As our default inputs, we are using the data on exclmation points and types of
               articles that we gathered earlier in Chapter 2. Recall that we found a 
               28% chance of an articles title having an exclamtion point given it's fake
               and a 2% chance of having an exclamation point given it's a real article.
               To see how the likelihood distribution 
               and the posterior distribution would alter with different data,
               change the probabilities in the right column. In this case, what is the likelihood
               of an exclamation point given fake news article and what is the likelihood
               of an article title using an exclamation point given it's a real news article?"),
             style = "padding-right: 5%; padding-left: 5%"),
           div(
             plotOutput('plot2'),
             style = "padding-right: 10%; padding-left: 10%"
           ),
           div(
             p("Finally, we have our posterior distribution. The graph below shows the proportion of 
               real and fake articles with exclamation point usage. Based on the data we input, the 
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
)#navbarMenu end
)#navbarPage end
)#end of fluid page
