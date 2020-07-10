library(shiny)

server<-function(input, output, session) {
 output$answer<-renderPrint({ 
   req(input$q1)
   req(input$q2)
   req(input$q3)
   req(input$q4)
   asInt1=as.integer(input$q1)
     asInt2=as.integer(input$q2)
     asInt3=as.integer(input$q3)
     asInt4=as.integer(input$q4)
     x=asInt1+asInt2+asInt3+asInt4
   if(x<=5&x>=4){
      "Your current thinking are fairly frequentist!";
    } else if(x>=9){
      "According to your answers, you already think like a Bayesian!";
    } else if(x<=8 |x>=6){
      "You don't take sides; you shared ideas from frequentists and Bayesians";
    }
    })
 data_1<-reactive({
   validate(
     need(input$p1<=1, "Make sure your inputs are less than 1!")
   )
   input$p1
   
 })
 
 data_2<-reactive({
   validate(
     need(input$p2<=1, "Make sure your inputs are less than 1!")
   )
   input$p2
   
 })
 
 observeEvent(list(input$p2, input$p1, input$num),{
   output$plot<-renderPlot({
     # Define possible articles
     article <- data.frame(type = c("real", "fake"))
     # Define the prior model
     prior <- c(0.6, 0.4)
     article_sim <- sample_n(article,
                             size = input$num,weight = prior, replace = TRUE)
     #First Plot
     ggplot(article_sim, aes(x=type))+
       geom_bar()+
       labs(title = "Prior Distribution", x="Type", y="Count")+
       theme(axis.text=element_text(size=14),
             axis.title=element_text(size=14
                                     ,face="bold"),
             plot.title = element_text(size=22))
   })
   
   
   
   output$plot2<-renderPlot({
     article <- data.frame(type = c("real", "fake"))
     # Define the prior model
     prior <- c(0.6, 0.4)
     article_sim <- sample_n(article,
                             size = input$num,weight = prior, replace = TRUE)
     #Likelihood 
     article_sim <- article_sim %>% 
       mutate(likelihood = 
                case_when(type == "fake" ~ data_1(),type == "real" ~ data_2() ))
     #Creting Proportions
     data <- c("no", "yes")
     # Simulate exclamation point usage
     set.seed(3)
     article_sim <- article_sim %>%
       group_by(1:n()) %>%
       mutate(usage = sample(data, size = 1,
                             prob = c(1 - likelihood, likelihood))) 
     
     ggplot(article_sim, aes(x = usage)) + geom_bar() +
       facet_wrap(~ type)    +
       labs(title = "Likelihood Distribution", x="Usage", y="Count")+
       theme(axis.text=element_text(size=14),
             axis.title=element_text(size=14
                                     ,face="bold"),
             plot.title = element_text(size=22))
     
   })
   output$plot3<-renderPlot({
     
     article <- data.frame(type = c("real", "fake"))
     # Define the prior model
     prior <- c(0.6, 0.4)
     article_sim <- sample_n(article,
                             size = input$num,weight = prior, replace = TRUE)
     #Likelihood 
     article_sim <- article_sim %>% 
       mutate(likelihood = 
                case_when(type == "fake" ~  data_1(),type == "real" ~ data_2() ))
     #Creting Proportions
     
     
     data <- c("no", "yes")
     set.seed(3)
     article_sim <- article_sim %>%
       group_by(1:n()) %>%
       mutate(usage = sample(data, size = 1,
                             prob = c(1 - likelihood, likelihood))) 
     
     
     ggplot(article_sim, aes(x = type)) + 
       geom_bar() +
       facet_wrap(~ usage)+
       labs(title = "Posterior Distribution", x="Type", y="Count")+
       theme(axis.text=element_text(size=14),
             axis.title=element_text(size=14
                                     ,face="bold"),
             plot.title = element_text(size=22))
   })
   
 })

}
