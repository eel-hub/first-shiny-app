library(shiny)
library(shinythemes)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  
  titlePanel(
    tagList(
      tags$img(src = "logo-shiny.png", height = "80px", width = "80px"),
      "Min først shiny app"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Variabler"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      selectInput("xvar",
                  "Vælg x-variablen:",
                  choices = colnames(faithful),
                  selected = "eruptions"),
      
      selectInput("yvar",
                  "Vælg y-variablen:",
                  choices = colnames(faithful),
                  selected = "waiting")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Histogram",
          plotOutput("distPlot")
        ),
        tabPanel(
          "Plot",
          plotOutput("scatterPlot")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = 'orange', border = 'blue',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(faithful, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "pink", size = 3) + 
      geom_smooth(method = "lm", se = FALSE, color = "purple") + 
      labs(
        title = paste("Scatterplot af", input$xvar, "vs", input$yvar),
        x = input$xvar,
        y = input$yvar
      )
  })
}

shinyApp(ui = ui, server = server)
