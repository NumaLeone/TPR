#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predictor de Desertores"),

    # Sidebar with a slider input for number of bins 
  
        sidebarLayout(
            sidebarPanel(
                numericInput("numInput", "Ingresa tu Nota del parcial de Analisis:", value=0, min = 1, max = 10),
                numericInput("numInput", "Ingresa tu Nota del primer parcial de Algebra:", value=0,min = 1, max = 10),
                numericInput("numInput", "Ingresa tu Nota del  segundo parcial de Analisis:", value=0, min = 1, max = 10),
                numericInput("numInput", "Ingresa tu Promedio de Introduccion a la Programacion:", value=0, min = 1, max = 10),
                actionButton("MiBoton","Predecir")
            
                
            ),
            mainPanel(
                p(strong("bold font "), em("italic font"))
            
        )
    ),
# Define server logic required to draw a histogram


# Run the application 
shinyApp(ui = ui)

