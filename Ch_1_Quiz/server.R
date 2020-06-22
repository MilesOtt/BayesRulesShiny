library(shiny)

server<-function(input, output, session) {
  output$answer<-renderText({
    asInt1=as.integer(input$q1)
    asInt2=as.integer(input$q2)
    asInt3=as.integer(input$q3)
    asInt4=as.integer(input$q4)
    x=asInt1+asInt2+asInt3+asInt4
    if(x<=5){
      paste("Your current thinking are fairly frequentist!");
    } else if(x>=9){
      paste("According to your answers, you already think like a Bayesian!");
    } else {
      paste("You don't take sides; you shared ideas from frequentists and Bayesians")
    }
  })
}
