server <- function(input, output, session) {
  
  # This reactive expression specifically handles file uploads
  uploadedData <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Read the data file
    if (grepl("\\.csv$", inFile$name)) {
      df <- read.csv(inFile$datapath)
    } else {
      df <- read_excel(inFile$datapath)
    }
    
    # Set column names for the sortCol input only when a new file is uploaded
    updateSelectInput(session, "sortCol", choices = names(df))
    
    return(df)
  })
  
  # This reactive expression handles the sorting and is dependent on the uploaded data and sort inputs
  data <- reactive({
    df <- uploadedData()
    
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
    req(data())
    
    df <- data()
    
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
    p <- ggplotly(p)
    
    if (input$chart_caption != "") {
      p <- p %>% 
        layout(
          margin = list(l = 50, r = 50, b = 100, t = 50), # Adjust as necessary to create enough space
          annotations = list(
            list(
              x = 1, y = -0.3, # Set to the bottom right
              xref = 'paper', yref = 'paper',
              text = input$chart_caption,
              showarrow = FALSE, xanchor = 'right',
              font = list(size = 10, color = 'grey50')
            )
          )
        )
    }
    
    p # This returns the modified plotly object
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
}


