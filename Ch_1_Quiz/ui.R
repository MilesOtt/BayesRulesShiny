library(shiny)
library(tidyverse)
library(shinyjs)


ui<-fluidPage(
  navbarPage("Bayes Rules!",
             tabPanel("About",
                        h2("Welcome to Bayes Rules! Interactive Exercises "),
                      fluidRow(
                        column(6,
                               h4("Aboout This Page"),
                               div(
                        p("Here you'll have exercises that supplement certain sections of Bayes Rules!
                           to further assist you in your journey of learning Bayesian statistics. These exercises can be 
                           found in the top bar titled, 'Exercises'. From there you will have a drop-down list of 
                           different chapters and sections that we've created for you. After choosing
                           an app, follow the in-app instructions to learn more about that topic. Certain 
                          chapters include Bayes Rule! R-package functions, so be sure to load our R-package
                          to use it on your own."), 
                        style = "padding-right: 5%; padding-left: 5%"),

                        h5("Built with",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                           "by",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                           ".")),
                      column(6,
                             h4("About the Authors"),
                             div(
                               p("Bayes Rules! is a textbook designed for students interested in learning about
                               Bayesian statistics at the undergraduate and graduate level.
                               ")
                             )
                    )))
             ,
             navbarMenu("Exercises",
                        #Chapter 1 Quiz
                        tabPanel("Chapter 1 Quiz",
                                 titlePanel("What type of Statistician are you?"),
                                 mainPanel(
p("Take this quiz to see whether you share more frequentist ideas,
bayesian ideas, or both!"),
br()),
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
  br(),
  br(),
  br(),
  br(),
  p("This application was made by Elaona Lemoto in
    supplement to Bayes Rules! by Alicia Johnson, Mine Dogucu, and Miles Ott. Bayes Rules!
      is a textbook for undergarduates learning Bayesian statistics. ")),

#Chapter 2, Posterior
tabPanel("Chapter 2: Posterior",
         titlePanel(
           h1("Chapter 2.1.5: Posterior Simulation with Article Verification", align = "center")
         ),
         # Sidebar to demonstrate various slider options ----
         sidebarPanel(
           "The slider changes the number of random samples we take of articles overall.",

           sliderInput("c2num", "Number of Random Samples", value=5000, min=1, max=10000),

           "Below, you can alter the prior probabilities for type of articles that have an exclamation point
           in their title. Again, the default prior probabilities as they are in the book for articles that are fake and that have an
           exclamation point is 28% and 2% of real news articles use exclamation points. Therefore, the values need to remain
           between 0 and 1.",
           #Change wording to reference book -  We're using the example from the book.


           numericInput("c2p1", "P(Exclamation Point | Fake Article ) ", value=0.28,min=0,max=1),
           numericInput("c2p2", "P(Exclamation Point | Real Article ) ",value=0.02, min=0,max=1),
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
           ),
#Chapter 3 Beta Model
tabPanel("Chapter 3: Beta Model",
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
           submitButton("submit"),
           style = "position:fixed;width:inherit;"),
         mainPanel(
           div(
             strong("The goal of this app is to get you a bit more familiarized with the
                    beta model which will be discussed throughout chapter 3. In this exercise,
                    you can play around with the beta model's two parameters, alpha and beta.
                    You can also choose whether to have the mean and mode depicted on the graph. This 
                    Shiny app uses the plot_beta function from the  Bayes Rules! R-package. Be sure to 
                    check it out on your R-studio! "),
             style = "padding-right: 5%; padding-left: 5%"
             ),
           div(
             plotOutput('plot_b'),
             style = "padding-right: 10%; padding-left: 10%"),
           div(
             "This 
             Shiny app uses the plot_beta function from the  Bayes Rules! R-package. Be sure to 
             load it on your own R and try it yourself. ",
             style = "padding-right: 5%; padding-left: 5%"
           ),
           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. "),
             style = "padding-right: 5%; padding-left: 5%")
           )),
#Chapter 3 Beta Binomial
tabPanel("Chapter 3: Beta Binomial Model",
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
           radioButtons("c3alpha", "Alpha", choices=c(45, 55, 65, 75)),
           radioButtons("c3beta", "Beta", choices=c(45, 55, 65,75)),
           "Now the sliders here are to gain more insight on pi, better known
           as Michelle's election support. In order to do so, we need to conduct
           polls (n) and record the number of pollers interested in voting
           for Michelle (X). Remember that number of pollers must be greater than or equal to
           Pollers in support of Michelle. ",
           sliderInput("c3n", "Number of Respondents to the Poll", value=50, min=1, max=100),
           sliderInput("c3x", "Number of Respondents in Support of Michelle", value=30, min=1, max=100),
           submitButton("Submit"),
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
             plotOutput('plot_bb'),
             style = "padding-right: 10%; padding-left: 10%"),
  
           div(
             "This 
             Shiny app uses the plot_beta_binomial function from the  Bayes Rules! R-package. Be sure to 
             load it on your own R-studio and try it out yourself. ",
           style = "padding-right: 5%; padding-left: 5%")
           ,
           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. "),
             style = "padding-right: 5%; padding-left: 5%")
         )),

#Chapter 4: Sequential Bayesian Analysis


tabPanel("Chapter 4: Sequential Bayesian Analysis",
         titlePanel(
           h1("Chapter 4: Sequential Bayesian Analysis")
         ),
           sidebarPanel(
             "Here you can change the inputs for the models to our right. Let's start by choosing an alpha
             and beta value for our first beta binomial model. Along with choosing the alpha and beta 
             values for the fist model, choose a number of trials and successes. ",
             radioButtons("ch4_alpha", "Alpha", choices=c(1, 2,10 )),
             radioButtons("ch4_beta", "Beta", choices=c(1,2,10)),
             sliderInput("ch4_x1", "Number of Successes for Day 1", value=5, min=1, max=10),
             sliderInput("ch4_n1", "Number of Trials for Day 1", value=6, min=1, max=10),
             
             "For day 2, choose a new number of trials and successes to
             show our updated beta binomial model.",
             sliderInput("ch4_x2", "Number of Successes for Day 2", value=5, min=1, max=20),
             sliderInput("ch4_n2", "Number of Trials for Day 2", value=6, min=1, max=20),
             
             "This is the last day of data collection. Choose the last number of trials and successes.",
             
             sliderInput("ch4_x3", "Number of Successes for Day 3", value=5, min=1, max=10),
             sliderInput("ch4_n3", "Number of Trials for Day 3", value=6, min=1, max=10),
             submitButton("Submit")),
           mainPanel(
             div(
               "In Chapter 4, we began to look at sequential bayesian analyses where 
               with more and more data, we're able to learn more about our parameter in
question. Below is our first graph showing our beta binomial model. Play around with the alpha and
               beta buttons as well as trial and successes data to your left to see how it alters the
               distribution. Just like our last section, Chapter 3: Beta Binomial Model, we are using the plot_beta_binomrial 
               function from our R-package to plot these models.",
               style = "padding-right: 5%; padding-left: 5%"
             ),
             br(),
             div(
               plotOutput('plot_ch41'),
               style = "padding-right: 10%; padding-left: 10%"),
             div(
               "Now it is day 2 and we just collected new data. Using the prior data from day 1 as well as 
               your inputs for day 2, we now have our updated beta bimomial distribution. What do you notice about this models prior 
               distribution and day 1's posterior distribution?",
               
               style = "padding-right: 5%; padding-left: 5%"),
             br(),
         div(
           plotOutput('plot_ch42'),
           style = "padding-right: 10%; padding-left: 10%"),
         div(
           "It's now day 3 and our final day of data collection. Do you find similarities between this distribution and that of day 2's? ",
          style= "padding-right: 5%; padding-left: 5%"
         ),
         br(),
         div(
           plotOutput('plot_ch43'),
           style = "padding-right: 10%; padding-left: 10%"),
         div(
           br(),
           br(),
           p("This application was made by Elaona Lemoto in
             supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
             is a textbook for undergarduates learning Bayesian statistics. "),
           style = "padding-right: 5%; padding-left: 5%")
             )
         ),

#Chapter 4: Balancing Bayesian Models

tabPanel("Chapter 4: Balancing Bayesian Models",
         titlePanel(
           h1("Chapter 4 : Striking a Balance! Finding Balance Between the Prior and Data
              ")
         ),
         sidebarPanel(
           "Below, choose a sample size and an alpha and beta value. Recall that the proportion 
           of success is fixed at 60%!",
           radioButtons("balance_samplen", "Sample Size", choices=c(5, 10, 15, 20, 100)),
           sliderInput("balance_alpha", "Alpha", value=14, min=0.01, max=100),
           sliderInput("balance_beta", "Beta", value=1, min=0.01, max=100), 
           submitButton("Submit")
           
         ),
         mainPanel(
           "We have been talking more and more about the posterior distribution, however
           this wouldn't be possible without combinations of observed data and prior models. The
posterior distribution is to be a balance between the prior and the likelihood.
           For this exercise, the propportion of success is at 60%. You can choose different sample sizes and 
different alpha and beta
           values. As the sample sizes change, how does it change the balance between the prior
           and the likelihood? ",
           div(
             plotOutput(
               "balance_bayesian"),
               style = "padding-right: 10%; padding-left: 10%")
           ,
           
           div(
             "This 
             Shiny app uses the plot_beta_binomial function from the  Bayes Rules! R-package. Be sure to 
             load it on your R-studio and try it out! ",
             style = "padding-right: 5%; padding-left: 5%")
           ,
           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. "),
             style = "padding-right: 5%; padding-left: 5%")
           
         )
         
),
#Chapter 5: Gamma-Poisson

tabPanel("Chapter 5: Introduction to the Gamma-Poisson Model",
         titlePanel(
           h1("Chapter 5: The Gamma-Poisson Model")
         ),
         sidebarPanel(
           "Below you will find different alpha and beta values for our Gamma prior.
           Choose different values to see how it would alter the Gamma-Poisson model.
           ",
           radioButtons("gamma_alpha", "Alpha", choices=c(9, 3, 6, 10)),
           radioButtons("gamma_beta", "Beta", choices=c(3, 9, 6, 10)),
           "After choosing values above, we now need a sample size and the sum of our observed x's.",
           sliderInput("poi_n", "Number of Samples", value=4, min=1, max=10), 
           sliderInput("poi_xn", "Sum of our xi's", value=11, min=0, max=100),
           submitButton("Submit")),
         mainPanel(
           "In Chapter 5.2, we talk about the Gamma-Poisson Conjugacy Family. If you
           recall, this conjugate family has a lambda parameter that uses a Gamma prior
           and a Poisson likelihood. In other words, the posterior distribution for 
           this conjugate has hyperparameters depending on the prior and the likelihood. 
           Since the Gamma-Poisson requires a Gamma prior and Poisson likelihood, use the 
inputs to your left to play around with the model.
For default, we are using values
           from our fraud risk calls example in section 2, but change the values yourself
           and see how that alters our distribution. ",
           div(
             plotOutput("gamma_poisson"),
             style = "padding-right: 10%; padding-left: 10%"
           ),
           div(
             "This 
             Shiny app uses the plot_gamma_poisson function from the  Bayes Rules! R-package to plot these
distributions. Try plugging in values outside of the alpha and beta values provided on your own R-studio! ",
           style = "padding-right: 5%; padding-left: 5%"
           ),
           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. "),
             style = "padding-right: 5%; padding-left: 5%"))),

#Chapter 5: Normal Normal

tabPanel("Chapter 5: Introduction to the Normal-Normal Model",
         titlePanel( 
           h1("Chapter 5: The Normal-Normal Conjugate Family")
         ),
         sidebarPanel(
           "Choose a mean and a standard deviation below for our Normal prior.",
           radioButtons("nmean", "Normal Mean", choices=c(3.25,2, 3)), 
           radioButtons("nsd", "Normal Prior Standard Deviation", choices=c(0.5, 1, 4)),
           
           "Now that you've chosen values for our Normal prior, we need to choose values for
           our Normal likelihood. In this case, we need a sample mean and a sample size.",
           sliderInput("nsample_mean", "Normal Sample Mean", value = 3, min=0, max=10),
           sliderInput("nsample_n","Normal Sample Size", value = 1,min=1, max=100),
           sliderInput("npop_sd","Population Standard Deviation (Assumingly Known)", value = 0.4, min=0.01, 
                       max=10),
           submitButton("Submit")
 ),
         mainPanel(
           "Last section, we were introduced to the idea of conjugate families.
           In Chapter 5.3 and in this exercise, we are extending our understanding 
of these types of families by learning more about the 
           Normal-Normal model. If you recall, a conjugate family
           is a model where the posterior distribution has hyperparamters that 
           depend on both the prior and the Likelihood. In this case, the Normal-Normal falls into
           that category seeing as though our posterior distribution for mew will be normal with
           the hyperparamters for the mean and standard deviation dependent on our values for our prior
           and likelihood. Like our last exercise, change the inputs for our Normal prior and our Normal
           likelihood to see how it alters our distribution. As default, we are using values from our
           in-text example of volume of hippocampus in the brain from 50 different collegiate American
           football players. Also, be sure to check out the plot_normal_normal function in the Bayes Rules!
           R-package!- This plot below wouldn't be possible without it!",
           div(
             plotOutput("normal_normal"),
             style = "padding-right: 10%; padding-left: 10%"
             ),
           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. "),
             style = "padding-right: 5%; padding-left: 5%")
           
         )),
tabPanel("Chapter 6: Grid Approximation",
         titlePanel(
           h1("Chapter 6: Approximating a Gamma-Poisson Posterior with Grid Approximation")
           ),
         sidebarPanel(
           "For your first step, alter the alpha, beta, and lambda values below. Recall that we ususally
           have a number of trials to choose from. In this case, we are keeping n fixed at 1.",
           radioButtons("g6_alpha", "Alpha", choices=c(3, 6, 7, 8)),
           radioButtons("g6_beta", "Beta", choices=c(1, 3, 6, 9)),
           sliderInput("g6_lambda", "Rate of Success", value=5, min=0, max=50),
           "Given how your Gamma prior and Poisson Likelihood distributions appear, change your grid values!",
           sliderInput("g6_grid", "Grid values ", value=15, min=0, max=50),
           submitButton("Submit")
         ),
         mainPanel(
           "In section 6.1.2, we begin to apply our grid approximation knowledge to a
           Gamma-Poisson Model. For this exercise, we are going to try approximate different posterior's given different prior and likelihood values.
In order to utilize our approximation tool, we need to establish
           more information about our model. Recall for a Gamma-Poisson model, you need a Gamma prior
and Poisson likelihood.
           As default we used the in-text example alpha and beta values as well as rate of success, but change
the values yourself to see how it would alter the distributions. Now having set up our models, let's
move on to the grid approximations. For our first step, we need to specify a discrete grid of lambda values. Those
values are where we would approximate the posterior. Recall that the Poisson likelihood paramater, lambda,
can only take positive
values from 0 to infinity. However, we also found that after plotting our prior and likelihood functions,
we would only
have a possible lambda values from 0 to 15. As you alter the alpha and beta values to your left,
look at how the
distribution changes and change your lambda grid values accordingly. Again, we are using the 
           plot_gamma_poisson function to plot out our initial model.",
           div(
             plotOutput("grid_p1"),
             style = "padding-right: 5%; padding-left: 5%"),

           div(
             "Now seeing how our prior and likelihood distributions look, use the grid slider to your
             left to change the lambda values accordingly. Does our approximation check out?",
             style= "padding-right: 5%; padding-left: 5%"
           ),
           div(
             plotOutput("grid_p2"),
             style = "padding-right: 10%; padding-left: 10%"),



           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. ")

         ))),
tabPanel("Chapter 7: Metropolis-Hastings Algorithm",
         titlePanel(
           h1("Chapter 7: Tuning the Metropolis-Hastings Algorithm")
         ),
         sidebarPanel(
           "Using the slider below, change the standard deviation of our model. ",
           
           #Try really small values and really big values and try values to see
           #try to find an app. sd (use wording from the book)
           #We're using a normal proposal model.
           # Explicty where our model we're using
           #Have priors be different (Changes the alpha and beta for current)
           #Change the data (change 0 do it  for the depois)
           
           #We're doing doing a Gamma-Poisson. Prior and data used. 
           sliderInput("mcmc_sd", "Standard Deviation", value = 0.3, min=0, max=10, step=0.0001),
           submitButton("Submit")
         ), 
         
         mainPanel(
           "In Chapter 6, we began to talk about Markov chain tours for simulating and 
approximating a posterior
           distribution. The Metropolis-Hastings algorithm R code in section 7.2 works at formulating
           that chain step-by-step without rStan which you can follow with the in-text R code. 
For the purpose 
           of this exercise, you will be changing the size of the standard deviation of our chosen 
model and 
           evaluating it's impact on our Markov trace plots. What do you notice about using a 
           really small standard deviation? What about a large one?",
           div(
             plotOutput("mcmc_trace_plot"),
             style = "padding-right: 10%; padding-left: 10%"),

           div(
             plotOutput("mcmc_iteration"),
             style = "padding-right: 10%; padding-left: 10%"),
           div(
             br(),
             br(),
             p("This application was made by Elaona Lemoto in
               supplement to Bayes Rules! by  Mine Dogucu, Alicia Johnson, and Miles Ott. Bayes Rules!
               is a textbook for undergarduates learning Bayesian statistics. ")
)))
#not looking for too small and not too big proprosals 

#read the chapters and run the code
#Shiny app to make would be one o the ex. but changidng how bing the standard deviation and 
#how it changes to trace plot
#independent sampling theorem
#make the metropiplolish hastinsgs ex. and have the options to change the sd for the
#propposal model 
#have a plot tour plot and density plot. 
)#navbarMenu end
)#navbarPage end
)#end of fluid page
