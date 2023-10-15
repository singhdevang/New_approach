library(shiny)
library(readxl)
library(plotly)
library(DT)
setwd("C:/Users/De122459/OneDrive - NHS Wales/Documents/new approach/New_approach")

server <- function(input, output) {
  
  # Reactive expression to read in the data
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    }
  })
  
  # Reactive expression to create the plot
  output$barPlot <- renderPlotly({
    req(data())
    
    df <- data()
    
    # Depending on input, set the axis variables
    if(input$graphType == 'Horizontal Bar Graph') {
      x <- 'x'
      y <- 'y'
    } else {
      x <- 'y'
      y <- 'x'
    }
    
    # Apply sorting if specified
    if(input$sortOrder == 'Ascending') {
      df <- df[order(df[[x]]), ]
    } else if(input$sortOrder == 'Descending') {
      df <- df[order(-df[[x]]), ]
    }
    
    # Create the plot
    p <- plot_ly(df, x = ~get(x), y = ~get(y), type = 'bar', orientation = ifelse(input$graphType == 'Horizontal Bar Graph', 'h', 'v')) %>%
      layout(title = list(text = input$chartTitle, font = list(family = "Arial", size = input$fontSize, color = "rgb(27,87,104)")), 
             xaxis = list(title = input$xLabel, showgrid = FALSE), 
             yaxis = list(title = input$yLabel, showgrid = FALSE),
             bargap = input$barSpace)
    
    p <- p %>% layout(colorway = c('rgb(74, 121, 134)'))
    p
  })
  
  # Reactive expression for the data table
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 25))
  })
}








library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Improvement Cymru Graph Customisation Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data", accept = c(".xlsx", ".csv")),
      selectInput("graphType", "Graph Type", choices = c("Horizontal Bar Graph", "Vertical Bar Graph")),
      textInput("chartTitle", "Chart Title", value = "Enter title..."),
      numericInput("fontSize", "Font Size", value = 14, min = 1, max = 72),
      sliderInput("barSpace", "Bar Spacing", min = 0.1, max = 1, value = 0.7, step = 0.1),
      textInput("xLabel", "X-axis Label", value = "Enter label..."),
      textInput("yLabel", "Y-axis Label", value = "Enter label..."),
      selectInput("sortOrder", "Sort Order", choices = c("None", "Ascending", "Descending"))
    ),
    
    mainPanel(
      plotlyOutput("barPlot"),
      DTOutput("dataTable")
    )
  )
)
