library(shiny)

server<-function(input, output, session) {
 output$answer<-renderPrint({ 
     asInt1=as.integer(input$q1)
     asInt2=as.integer(input$q2)
     asInt3=as.integer(input$q3)
     asInt4=as.integer(input$q4)
     x=asInt1+asInt2+asInt3+asInt4
   if(x<=5){
      "Your current thinking are fairly frequentist!";
    } else if(x>=9){
      "According to your answers, you already think like a Bayesian!";
    } else if(x<=8 |x>=6){
      "You don't take sides; you shared ideas from frequentists and Bayesians";
    } else {
      "0 ";
    }

    })

}
