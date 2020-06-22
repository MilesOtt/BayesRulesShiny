library(shiny)

ui<-fluidPage(
  #setting  up radio buttons
  radioButtons("q1", "When flipping a coin, we say that 'the probability of flipping heads is 0.5'. How do you interpret this probability?",choices=list("If I flip this coin over and over, roughly 50% will be Heads."=1, "Heads and Tails are equally plausible."=3,"Both a and b make sense."=2),selected=NULL),
  radioButtons("q2", "A meterologist warns that 'there's a 0.1 probability
               of rain today.' How do you interpret this probability?",
               choices =list("If we observe today over and over, it will rain on roughly 10% of todays."=1,"It's equally likely to rain or not rain."=3,"The meteorologist's calculation is wrong. It will either rain or not rain, thus the probability of rain can only be 0 or 1"=1),selected=1),
  radioButtons("q3", "Consider two claims. (1) Zuofu claims that he can predict the out- come of a coin flip. To test his claim, you flip a fair coin 10 times and he correctly predicts all 10! (2) Kavya claims that she can dis- tinguish Dunkin’ Donuts1 coffee from Starbucks coffee. To test her claim, you give her 10 coffee samples and she correctly identifies the origin of each! In light of these experiments, what do you conclude? ", choices = list("You’re more confident in Kavya’s claim than Zuofu’s claim."=3,"The evidence supporting Zuofu’s claim is just as strong as the
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     evidence supporting Kavya’s claim."=1), selected=1),
  radioButtons("q4","Suppose that during a recent doctor’s visit, you tested positive for a very rare disease. If you only get to ask the doctor one question, which would it be?", choices= list("What’s the chance that I actually have the disease?"=3,"If in fact I don’t have the disease, what’s the chance that I would’ve gotten this positive test result?"=1),selected=1),
  
  submitButton("Submit"),
  
  verbatimTextOutput("answer")
  
  )

