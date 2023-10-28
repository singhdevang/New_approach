ui <- dashboardPage(
  dashboardHeader(title = "Improvement Cymru Graph Customisation Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Graph Customisation", tabName = "graph_customisation", icon = icon("chart-bar"))
    ),
    fileInput("file1", "Choose CSV, Excel", accept = c(".csv", ".xlsx")),
    selectInput("sheet", "Select the sheet to be used:", choices = NULL),
    selectInput("graphType", "Graph Type:", choices = c("Horizontal Bar Graph", "Vertical Bar Graph")),
    uiOutput("numeric_selection")
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data_upload",
              fluidRow(
                box(DTOutput('dataTable'), width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "graph_customisation",
              fluidRow(
                box(plotlyOutput("barPlot"), width = 12)
              ),
              fluidRow(
                box(
                  title = "Customisation Options", 
                  status = "primary", 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  textInput("chartTitle", "Chart Title:", ""),
                  numericInput("fontSize", "Font Size:", 14, min = 1, max = 40),
                  sliderInput("barSpace", "Bar Width:", min = 0.1, max = 1, value = 0.7, step = 0.1),
                  textInput("chart_caption", "Chart Caption:", ""),
                  textInput("xlab", "X Axis Label:", ""),
                  textInput("ylab", "Y Axis Label:", ""),
                  selectInput("sortCol", "Sort Column:", NULL), # Choices are set dynamically in server
                  selectInput("sortOrder", "Sort Order:", choices = c("None" = "none", "Ascending" = "asc", "Descending" = "desc"))
                )
              )
      )
    )
  )
)
