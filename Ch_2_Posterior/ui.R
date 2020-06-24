library(shiny)
set.seed(84735)

ui<-fluidPage(
  titlePanel("Section 2.1.5: Posterior Simulation"),
  sliderInput("num", "Sample Number", value=0, min=10, max=10000),
  plotOutput("plot")
)