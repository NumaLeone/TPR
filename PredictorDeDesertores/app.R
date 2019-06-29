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
library(magick)
source("Funcion.R")
load(file="funcionPredictor.rdata")
ui <- fluidPage(
  
  titlePanel(strong(h1("Predictor de Desertores")), windowTitle = "Predictor"),
  sidebarLayout(
    
    sidebarPanel (h6(numericInput(inputId = "notaAnalisis", label= h3("Ingrese su nota de Analisis:"),value=9,min=0,max=10, step=1)),
                  numericInput(inputId = "notaAlgebra1", label= h3("Ingrese su nota del primer parcial de Algebra:"),value=9,min=0,max=10, step=1),
                  numericInput(inputId = "notaAlgebra2", label= h3("Ingrese su nota del segundo parcial de Algebra:"),value=9,min=0,max=10, step=1),
                  numericInput(inputId = "notaProg", label= h3("Ingrese su promedio de Intro. prog:"),value=9,min=0,max=10, step=1),
                  width =4
                  ),
  
  mainPanel(
    p(h4("Con esta aplicacion, construida a partir de las notas de los alumnos en el primer cuatrimestre ,que nos proporciono la universidad, podemos predecir con una cierta
      precision si un alumno va a abandonar la carrera en Ingenieria.")),
    strong(h3("Nuestro objetivo no es desmotivar a nadie a dejar sus estudios, asi que:")),
    strong(h2("¡ Mucha suerte y no dejen de persistir!")),
    div(h2("Resultado:"),style="color:red"), 
    h1(textOutput("result")),
    uiOutput(outputId = "gif")
    
  
  ),
  position=c("left","right"),
  fluid=TRUE),
  
  
  
  title = "predictor",
  theme= shinytheme("superhero")
)
  

server<- function(input, output){
  state<-reactiveValues()
 observe({
state$result<-funcionPredictor(input$notaAlgebra1,input$notaAlgebra2,input$notaAnalisis,input$notaProg)
  
})
output$result<-renderText({
  if(state$result==1){
    paste("Este alumno DEJO la carrera")
  }
  else{
    paste("Este alumno NO dejo la carrera")
  }
})
output$gif<-renderUI({
  if(state$result==1)
    tags$img(src = 'https://media.giphy.com/media/1AgDsmf7ASiJr72ZZC/giphy.gif')
  else
    tags$img(src = 'https://media.giphy.com/media/jqqdRrgxFMuXQxTthe/giphy.gif' )
})

  
  
  
}

 

shinyApp(ui = ui, server= server)


