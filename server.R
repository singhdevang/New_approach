server <- function(input, output, session) {
  
  # This reactive expression handles the file input
  observeEvent(input$file1, {
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Check if it's an Excel file before trying to read sheets
    if (grepl("\\.xlsx$", inFile$name)) {
      # Get the sheet names
      sheets <- excel_sheets(inFile$datapath)
      
      # Update the choices in the sheet selection input
      updateSelectInput(session, "sheet", choices = sheets)
    }
  }, ignoreInit = TRUE)
  
  # This reactive expression reads the data from the file
  data <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Check file type to read the data accordingly
    if (grepl("\\.csv$", inFile$name)) {
      df <- read.csv(inFile$datapath)
    } else {
      # Read the specific sheet
      df <- read_excel(inFile$datapath, sheet = input$sheet)
    }
    
    # Set column names for the sortCol input only when a new file is uploaded
    updateSelectInput(session, "sortCol", choices = names(df))
    
    return(df)
  })
  
  # This reactive expression handles the sorting and is dependent on the uploaded data and sort inputs
  sortedData <- reactive({
    df <- data()
    
    if (is.null(df)) {
      return(NULL)
    }
    
    # Sort data if inputs are not null and are applicable
    if (!is.null(input$sortCol) && !is.null(input$sortOrder) && input$sortCol %in% names(df)) {
      df <- df[order(df[[input$sortCol]], decreasing = input$sortOrder == "desc"), ]
    }
    
    return(df)
  })
  
  output$barPlot <- renderPlotly({
    req(sortedData())
    
    df <- sortedData()
    
    # Determine which is the character column
    char_col <- names(which(sapply(df, is.character)))[1]
    num_col <- names(which(sapply(df, is.numeric)))[1]
    
    # If data is sorted and it's a bar graph, reorder the factor levels
    if (!is.null(input$sortCol) && !is.null(input$sortOrder)) {
      df[[char_col]] <- factor(df[[char_col]], levels = df[[char_col]])
    }
    
    # Assign labels based on graph type
    if (input$graphType == 'Horizontal Bar Graph') {
      x_axis <- input$ylab
      y_axis <- input$xlab
    } else {
      x_axis <- input$xlab
      y_axis <- input$ylab
    }
    
    
    p <- ggplot(df, aes_string(x = char_col, y = num_col)) + 
      geom_bar(stat = "identity", fill = rgb(74/255, 121/255, 134/255), width = input$barSpace) +
      labs(title = input$chartTitle, x = x_axis, y = y_axis) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = input$fontSize, 
          family = "Arial",
          colour = rgb(27/255, 87/255, 104/255)
        ),
        panel.grid = element_blank()
      )
    
    # Add coord_flip if Horizontal Bar Graph is selected
    if (input$graphType == 'Horizontal Bar Graph') {
      p <- p + coord_flip()
    }
    
    # Convert ggplot object to plotly object
    p <- ggplotly(p)
    
    # Customize axis lines and enable autorange based on graph type
    if (input$graphType == 'Horizontal Bar Graph') {
      p <- p %>% layout(
        xaxis = list(zeroline = TRUE, zerolinecolor = 'gray', zerolinewidth = 1, autorange = TRUE),
        yaxis = list(autorange = TRUE)  # Ensuring y-axis also uses autorange
      )
    } else { # 'Vertical Bar Graph'
      p <- p %>% layout(
        xaxis = list(autorange = TRUE),  # Ensuring x-axis also uses autorange
        yaxis = list(zeroline = TRUE, zerolinecolor = 'gray', zerolinewidth = 1, autorange = TRUE)
      )
    }
    
    # If there's a chart caption, add it
    if (input$chart_caption != "") {
      p <- p %>% 
        layout(
          margin = list(l = 50, r = 50, b = 100, t = 50),
          annotations = list(
            list(
              x = 1, y = -0.3, # Set to the bottom right
              xref = 'paper', yref = 'paper',
              text = input$chart_caption,
              showarrow = FALSE, xanchor = 'right',
              font = list(size = 12, color = 'black', family = "Arial")
            )
          )
        )
    }
    
    # Return the modified plotly object
    p 
  })
  output$dataTable <- renderDT({
    req(sortedData())
    datatable(sortedData())
  })
}