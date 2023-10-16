ui <- fluidPage(
  titlePanel("Improvement Cymru Graph Customisation Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV or Excel File",
                accept = c(".csv", ".xlsx")),
      selectInput("sheet", "Select the sheet to be used:", choices = NULL),
      
      selectInput("graphType", "Graph Type:",
                  choices = c("Horizontal Bar Graph", "Vertical Bar Graph")),
      
      # UI elements for sorting
      selectInput("sortCol", "Sort Column:", NULL), # Choices are set dynamically in server
      selectInput("sortOrder", "Sort Order:", choices = c("Ascending" = "asc", "Descending" = "desc")),
      
      textInput("chartTitle", "Chart Title:", ""),
      textInput("chart_caption", "Chart Caption:", ""),
      numericInput("fontSize", "Font Size:", 14, min = 1, max = 40),
      sliderInput("barSpace", "Bar Spacing:",
                  min = 0.1, max = 1, value = 0.7, step = 0.1),
      textInput("xlab", "X Axis Label:", ""),
      textInput("ylab", "Y Axis Label:", "")
    ),
    
    mainPanel(
      plotlyOutput("barPlot"),
      DTOutput('dataTable')
    )
  )
)
