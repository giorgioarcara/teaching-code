########################
# This shiny app is an utility to explore how correlation works
################

library(shiny)
library(ggplot2)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Scatterplot with Correlation"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter Data (up to 20 pairs)"),
      p("Edit the table below to enter your X and Y values:"),
      
      # Buttons to add/remove rows
      fluidRow(
        column(6, actionButton("add_row", "Add Row", class = "btn-sm btn-success")),
        column(6, actionButton("remove_row", "Remove Row", class = "btn-sm btn-warning"))
      ),
      
      br(),
      
      # Button to fill with random data
      actionButton("fill_random", "Fill with Random Data", class = "btn-sm btn-info btn-block"),
      
      br(),
      
      # Editable table
      rHandsontableOutput("data_table"),
      
      br(),
      
      # Calculate button
      actionButton("calculate", "Calculate", class = "btn-primary btn-lg btn-block"),
      
      br(),
      textOutput("data_info")
    ),
    
    mainPanel(
      # Main plot
      plotOutput("scatterplot", height = "500px"),
      
      br(),
      
      # Help panel below the plot
      wellPanel(
        style = "background-color: #f0f7ff; border: 1px solid #b8d4f0; border-radius: 6px;",
        h4(icon("circle-info", lib = "glyphicon"), " Help & Tutorial",
           style = "color: #2c6fad; margin-top: 0;"),
        p(strong("What is this app?"),
          "This is a tutorial tool to explore how statistical correlation works.
           Enter paired X and Y values, then visualise the relationship between them.
           The scatterplot title shows both Pearson and Spearman correlation coefficients."),
        hr(),
        h5("Button Reference", style = "color: #2c6fad;"),
        tags$table(
          class = "table table-condensed",
          style = "font-size: 13px;",
          tags$thead(
            tags$tr(
              tags$th("Button"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$span(class = "label label-success", "Add Row")),
              tags$td("Adds a new empty row to the data table (maximum 20 rows).")
            ),
            tags$tr(
              tags$td(tags$span(class = "label label-warning", "Remove Row")),
              tags$td("Removes the last row from the data table (minimum 1 row).")
            ),
            tags$tr(
              tags$td(tags$span(class = "label label-info", "Fill with Random Data")),
              tags$td("Fills all current rows with random integers between 1 and 100, useful for quick testing.")
            ),
            tags$tr(
              tags$td(tags$span(class = "label label-primary", "Calculate")),
              tags$td("Computes correlations and renders the scatterplot using all valid (non-empty) data pairs.")
            )
          )
        ),
        hr(),
        h5("Correlation Coefficients", style = "color: #2c6fad;"),
        tags$ul(
          tags$li(strong("Pearson (r):"),
                  " Measures the strength and direction of the",
                  em("linear"), "relationship between X and Y.
                   Assumes both variables are continuous and normally distributed.
                   Values range from -1 (perfect negative) to +1 (perfect positive)."),
          tags$li(strong("Spearman (ρ):"),
                  " A rank-based, non-parametric measure of monotonic association.
                   More robust to outliers and does not assume normality.
                   Useful when data are ordinal or the relationship is non-linear.")
        ),
        p(em("Tip: compare the two coefficients — large differences may indicate outliers
              or a non-linear relationship in your data."),
          style = "color: #555; font-size: 12px;"),
        p(em("Author: Giorgio Arcara with the help of Claude Sonnet 4.6 - 2026/03/17"),
          style = "color: #555; font-size: 12px;")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize data with 5 rows to start
  initial_data <- data.frame(
    X = rep(NA, 5),
    Y = rep(NA, 5)
  )
  
  # Reactive values to store the table data
  values <- reactiveValues(
    data = initial_data,
    database = data.frame(X = numeric(0), Y = numeric(0))
  )
  
  # Add row (up to 20 max)
  observeEvent(input$add_row, {
    current_rows <- nrow(values$data)
    if (current_rows < 20) {
      new_row <- data.frame(X = NA, Y = NA)
      values$data <- rbind(values$data, new_row)
    } else {
      showNotification("Maximum of 20 rows reached!", type = "warning")
    }
  })
  
  # Remove row (minimum 1 row)
  observeEvent(input$remove_row, {
    current_rows <- nrow(values$data)
    if (current_rows > 1) {
      values$data <- values$data[-current_rows, , drop = FALSE]
    } else {
      showNotification("Must have at least 1 row!", type = "warning")
    }
  })
  
  # Fill table with random integers (1 to 100)
  observeEvent(input$fill_random, {
    n_rows <- nrow(values$data)
    values$data <- data.frame(
      X = sample(1:100, n_rows, replace = TRUE),
      Y = sample(1:100, n_rows, replace = TRUE)
    )
    showNotification("Table filled with random integers!", type = "message")
  })
  
  # Render the editable table
  output$data_table <- renderRHandsontable({
    n_rows <- nrow(values$data)
    rhandsontable(values$data, rowHeaders = 1:n_rows, height = 400) %>%
      hot_col("X", type = "numeric") %>%
      hot_col("Y", type = "numeric") %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  # Update values when table is edited
  observeEvent(input$data_table, {
    values$data <- hot_to_r(input$data_table)
  })
  
  # Calculate and store data when button is clicked
  observeEvent(input$calculate, {
    current_data  <- values$data
    valid_data    <- current_data[complete.cases(current_data), ]
    values$database <- valid_data
  })
  
  # Display info about data
  output$data_info <- renderText({
    n_total <- nrow(values$data)
    n_valid <- sum(complete.cases(values$data))
    paste("Valid data pairs:", n_valid, "/", n_total)
  })
  
  # Create scatterplot
  output$scatterplot <- renderPlot({
    req(input$calculate)
    req(nrow(values$database) > 0)
    
    data <- values$database
    
    # Calculate both correlations
    if (nrow(data) >= 2) {
      r_pearson  <- cor(data$X, data$Y, method = "pearson")
      r_spearman <- cor(data$X, data$Y, method = "spearman")
      plot_title <- sprintf(
        "Pearson r = %.3f   |   Spearman \u03c1 = %.3f",
        r_pearson, r_spearman
      )
    } else {
      plot_title <- "Need at least 2 valid data points"
    }
    
    ggplot(data, aes(x = X, y = Y)) +
      geom_point(size = 3, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
      labs(title = plot_title,
           x = "X Values",
           y = "Y Values") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
      )
  })
}

shinyApp(ui = ui, server = server)