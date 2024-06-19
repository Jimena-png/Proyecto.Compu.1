#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Encuesta a hogares"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #elemento 1: variable a seleccionar
      fluidPage(
        
        # Copy the line below to make a select box 
        selectInput(inputId = "Entrada1",
                    label = h3("Variable a seleccionar"), 
                    choices = names(nhanes.reducida[,6:8]), 
                    selected = names(nhanes.reducida[7])),
        
        hr(),
        fluidRow(column(3, verbatimTextOutput("value")))
        
      ),
      
      #elemento 2
      sliderInput("Entrada2",
                  "Numero de intervalos:",
                  min = 1,
                  max = 60,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("grafico1"),
      plotOutput("grafico2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #salda 1
  output$grafico1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- nhanes.reducida[[input$Entrada1]]
    bins <- seq(min(x), max(x), length.out = input$Entrada2 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'lightpink', border = 'black',
         xlab = 'personas',
         ylab = 'Frecuencia',
         main = 'Histograma de encuesta a hogares')
  })
  output$grafico2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- nhanes.reducida[[input$Entrada1]]
    bins <- seq(min(x), max(x), length.out = input$Entrada2 + 1)
    
    # draw the histogram with the specified number of bins
    boxplot(x, breaks = bins, col = 'lightpink', border = 'black',
            xlab = 'personas',
            ylab = 'Frecuencia',
            main = 'Diagrama de cajas de encuesta a hogares')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)