#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Predictor de Desertores", windowTitle = "Predictor"),
  sidebarLayout(
    sidebarPanel (numericInput(inputId = "notaAnalisis", label= "Ingrese su nota de Analisis",value=0,min=0,max=10, step=1),
                  numericInput(inputId = "notaAlgebra1", label= "Ingrese su nota del primer parcial de Algebra",value=0,min=0,max=10, step=1),
                  numericInput(inputId = "notaAlgebra2", label= "Ingrese su nota del segundo parcial de Algebra",value=0,min=0,max=10, step=1),
                  numericInput(inputId = "notaProg", label= "Ingrese su promedio de Intro. prog",value=0,min=0,max=10, step=1),
                  actionButton(inputId="predecir",label="Predecir",icon=icon("arrow-alt-circle-right")),
                  width =4
                  ),
  
  mainPanel(
    textOutput("result"),
    imageOutput("resultImage")
  ),
  position=c("left","right"),
  fluid=TRUE),
  
  
  
  title = "predictor",
  theme= shinytheme("superhero")
)
  

server<- function(input, output){
  load("student.RData")
  student$`Algebra exam 1`<-input$notaAlgebra1
  student$`Algebra exam 2`<-input$notaAlgebra2
  student$`Analisis exam 1`<-input$notaAnalisis
  student$`IntroProg exam 1`<-input$notaProg
  prediction<-predict(model_nnet,student)
  output$result<-renderText()
  
  output$resultImage<-renderImage()
  
}


# Run the application 
shinyApp(ui = ui, server= server)

