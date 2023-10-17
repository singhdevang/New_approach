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
  
  sortedData <- reactive({
    df <- data()
    
    if (is.null(df)) {
      return(NULL)
    }
    
    # Check if "None" is selected for sorting. If so, return the original data frame.
    if (input$sortOrder == "none" || is.null(input$sortCol) || !(input$sortCol %in% names(df))) {
      return(df)
    }
    
    # Otherwise, proceed with sorting
    df <- df[order(df[[input$sortCol]], decreasing = input$sortOrder == "desc"), ]
    
    return(df)
  })
  
  output$numeric_selection <- renderUI({
    df <- data()
    # Check if data frame has exactly two numeric columns
    if (!is.null(df) && sum(sapply(df, is.numeric)) == 2) {
      selectInput("selected_numeric", "Select the numeric column:", 
                  choices = names(df)[sapply(df, is.numeric)], selected = NULL)
    }
  })
  
  output$barPlot <- renderPlotly({
    req(sortedData())
    
    df <- sortedData()
    
    # Determine which is the character column and numeric column
    if (!is.null(input$selected_numeric)) {
      num_col <- input$selected_numeric
      # Get the other column by excluding the selected numeric column
      char_col <- names(df)[!names(df) %in% num_col]
    } else {
      char_col <- names(which(sapply(df, is.character)))[1]
      num_col <- names(which(sapply(df, is.numeric)))[1]
    }
    
    
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
        axis.title.x = element_text(family = "Arial"), # Set Arial font for x axis title
        axis.title.y = element_text(family = "Arial"), # Set Arial font for y axis title
        panel.grid = element_blank()
      )
    
    
    # Add coord_flip if Horizontal Bar Graph is selected
    if (input$graphType == 'Horizontal Bar Graph') {
      p <- p + coord_flip()
    }
    
    # Convert ggplot object to plotly object
    p <- ggplotly(p)
    
    
    
    # Customize axis lines, enable autorange, and adjust title standoff
    if (input$graphType == 'Horizontal Bar Graph') {
      p <- p %>% layout(
        xaxis = list(
          zeroline = TRUE, 
          zerolinecolor = 'gray', 
          zerolinewidth = 1, 
          autorange = TRUE,
          titlestandoff = 15  # Adjust standoff to increase distance from axis
        ),
        yaxis = list(
          autorange = TRUE,
          titlestandoff = 15  # Adjust standoff to increase distance from axis
        )
      )
    } else { # 'Vertical Bar Graph'
      p <- p %>% layout(
        xaxis = list(
          autorange = TRUE,
          titlestandoff = 15  # Adjust standoff to increase distance from axis
        ),
        yaxis = list(
          zeroline = TRUE, 
          zerolinecolor = 'gray', 
          zerolinewidth = 1, 
          autorange = TRUE,
          titlestandoff = 15  # Adjust standoff to increase distance from axis
        )
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