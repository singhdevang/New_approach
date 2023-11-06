ui <- dashboardPage(
  dashboardHeader(title = "Improvement Cymru Graph Customisation Tool", titleWidth = 450),
  
  dashboardSidebar(width = 310,tags$head(
    tags$style(HTML(".sidebar {
height: 90vh; overflow-y: auto;
}"
    ) # close HTML
    ) # close tags$style
  ), # close tags#Head
    sidebarMenu(
      id = "sidebar_tabs",  # Added an ID here
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Graph Customisation", tabName = "graph_customisation", icon = icon("chart-bar"))
    ),
    conditionalPanel(
      condition = "input.sidebar_tabs == 'data_upload'",  # Updated the condition
      fileInput("file1", "Choose CSV, Excel", accept = c(".csv", ".xlsx")),
      uiOutput("sheet_ui")  # Render the sheet selection UI dynamically
    ),
    conditionalPanel(
      condition = "input.sidebar_tabs == 'graph_customisation'",  # Updated the condition
      selectInput("graphType", "Graph Type:", choices = c("Horizontal Bar Graph", "Vertical Bar Graph")),
      uiOutput("numeric_selection"),
      selectInput("sortCol", "Sort Column:", NULL), # Choices are set dynamically in server
      selectInput("sortOrder", "Sort Order:", choices = c("None" = "none", "Ascending" = "asc", "Descending" = "desc")),
      textInput("chartTitle", "Chart Title:", ""),
      numericInput("fontSize", "Font Size:", 14, min = 1, max = 40),
      sliderInput("barSpace", "Bar Width:", min = 0.1, max = 1, value = 0.7, step = 0.1),
      textInput("chart_caption", "Chart Caption:", ""),
      textInput("xlab", "X Axis Label:", ""),
      textInput("ylab", "Y Axis Label:", ""),
      selectInput("exportFormat", "Export Format:", choices = c("CSV" = "csv", "Excel" = "xlsx")),
      
      downloadButton("downloadData", label = textOutput("downloadLabel")),
      downloadButton("downloadPlot", "Download Plot")
      
      )
      
      
     )
  ,
  
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
                box(plotlyOutput("barPlot"), width = 12),
                box(DTOutput('dataTableGraphCustomisation'), width = 12)  # Box for the table
              )
              )
      )
    )
  )

