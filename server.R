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
    df <- if (grepl("\\.csv$", inFile$name)) {
      read.csv(inFile$datapath)
    } else {
      read_excel(inFile$datapath, sheet = input$sheet)
    }
    
    # Convert date columns (based on some common date formats)
    date_cols <- sapply(df, function(x) any(grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) || any(grepl("^\\d{2}/\\d{2}/\\d{4}$", x)))
    df[date_cols] <- lapply(df[date_cols], as.Date, format = ifelse(grepl("^\\d{4}", df[1, date_cols]), "%Y-%m-%d", "%d/%m/%Y"))
    
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
    
    # Sort by Date or Other column types
    df <- if (input$sortCol %in% names(df)[sapply(df, class) == "Date"]) {
      df[order(as.Date(df[[input$sortCol]]), decreasing = input$sortOrder == "desc"), ]
    } else {
      df[order(df[[input$sortCol]], decreasing = input$sortOrder == "desc"), ]
    }
    
    
    return(df)
  })
  
  output$numeric_selection <- renderUI({
    df <- data()
    # Check if data frame has exactly one numeric column and one date column
    if (!is.null(df) && sum(sapply(df, is.numeric)) == 1 && sum(sapply(df, class) == "Date") == 1) {
      num_col <- names(df)[sapply(df, is.numeric)]
      selectInput("selected_numeric", "Select the numeric column:", choices = num_col, selected = num_col)
    } else if (!is.null(df) && sum(sapply(df, is.numeric)) == 2) {
      selectInput("selected_numeric", "Select the numeric column:", 
                  choices = names(df)[sapply(df, is.numeric)], selected = NULL)
    }
  })
  
  output$barPlot <- renderPlotly({
    req(sortedData())
    
    df <- sortedData()
    # Check for the selection of numeric column and set character/date column
    if (!is.null(input$selected_numeric)) {
      num_col <- input$selected_numeric
      char_col <- ifelse(sum(sapply(df, class) == "Date") == 1, names(df)[sapply(df, class) == "Date"], names(df)[!names(df) %in% num_col])
    } else {
      char_col <- names(which(sapply(df, is.character)))[1]
      num_col <- names(which(sapply(df, is.numeric)))[1]
    }
    
    # Reorder factor levels if data is sorted
    if (!is.null(input$sortCol) && input$sortCol == char_col) {
      df[[char_col]] <- factor(df[[char_col]], levels = unique(df[[char_col]]))
    }
    df[[char_col]] <- factor(df[[char_col]], levels = unique(df[[char_col]]))
    
    # Assign labels based on graph type
    if (input$graphType == 'Horizontal Bar Graph') {
      x_axis <- input$ylab
      y_axis <- input$xlab
    } else {
      x_axis <- input$xlab
      y_axis <- input$ylab
    }
    # Determine the unique number of categories/labels on x-axis
    num_categories <- length(unique(df[[char_col]]))
    
    # Set label angle dynamically based on the number of categories
    # For fewer categories, a smaller angle (or 0) can be used, and for more, a larger angle
    label_angle <- if (input$graphType == 'Vertical Bar Graph') {
      ifelse(num_categories <= 10, 45, ifelse(num_categories <= 20, 90, 90))
    } else {
      # For Horizontal Bar Graph or other graph types, you might want to set it to a default value, like 0
      0
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
        axis.text.x = element_text(angle = label_angle, vjust = 1, hjust = 1), # Rotate x-axis labels
        panel.grid = element_blank()
      )
    
    # Add coord_flip if Horizontal Bar Graph is selected
    if (input$graphType == 'Horizontal Bar Graph') {
      p <- p + coord_flip()
    }
    
    # Convert ggplot object to plotly object
    p <- ggplotly(p)
    
    # Customize axis lines, enable autorange, and adjust title standoff
    standoff_value <- 30  # Increase this value to move the axis title further away
    if (input$graphType == 'Horizontal Bar Graph') {
      p <- p %>% layout(
        xaxis = list(
          zeroline = TRUE, 
          zerolinecolor = 'gray', 
          zerolinewidth = 1, 
          autorange = TRUE,
          title = list(
            text = y_axis,
            standoff = standoff_value
          )
        ),
        yaxis = list(
          autorange = TRUE,
          title = list(
            text = x_axis,
            standoff = standoff_value
          )
        ),
        margin = list(l = 60, r = 50, b = 100, t = 50, pad = 4)  # Adjust margins if needed
      )
    } else { # 'Vertical Bar Graph'
      p <- p %>% layout(
        xaxis = list(
          autorange = TRUE,
          title = list(
            text = x_axis,
            standoff = standoff_value
          )
        ),
        yaxis = list(
          zeroline = TRUE, 
          zerolinecolor = 'gray', 
          zerolinewidth = 1, 
          autorange = TRUE,
          title = list(
            text = y_axis,
            standoff = standoff_value
          )
        ),
        margin = list(l = 60, r = 50, b = 100, t = 50, pad = 4)  # Adjust margins if needed
      )
    }
    
    # If there's a chart caption, add it
    if (input$chart_caption != "") {
      p <- p %>% 
        layout(
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
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(sortedData(), options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10))
  })
}