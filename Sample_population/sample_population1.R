library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Population vs Sample Percentiles"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n",     "Number of observations (n)", value = 50,  min = 2),
      numericInput("mu",    "Population mean (μ)",         value = 100, step = 1),
      numericInput("sigma", "Population SD (σ)",           value = 15,  min = 0.01, step = 1),
      actionButton("simulate", "Simulate data", class = "btn-primary")
    ),
    
    mainPanel(
      plotOutput("hist_plot"),
      br(),
      tableOutput("pct_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive sample, drawn on startup and on every button press
  dat <- eventReactive(input$simulate, {
    req(input$n >= 2, input$sigma > 0)
    rnorm(input$n, mean = input$mu, sd = input$sigma)
  }, ignoreNULL = FALSE)
  
  probs  <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  labels <- c("P5", "P25", "P50 (Median)", "P75", "P95")
  
  output$hist_plot <- renderPlot({
    req(dat())
    xbar <- mean(dat())
    
    ggplot(data.frame(x = dat()), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = max(10, min(50, round(sqrt(length(dat())) * 2))),
                     fill = "steelblue", colour = "white", alpha = 0.7) +
      stat_function(fun  = dnorm,
                    args = list(mean = input$mu, sd = input$sigma),
                    colour = "red", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = input$mu, colour = "red",       linewidth = 1) +
      geom_vline(xintercept = xbar,     colour = "steelblue", linewidth = 1) +
      annotate("text", x = input$mu, y = Inf, vjust = 2, hjust = -0.1,
               label = paste0("μ = ", round(input$mu, 2)), colour = "red") +
      annotate("text", x = xbar, y = Inf, vjust = 3.8, hjust = -0.1,
               label = paste0("x̄ = ", round(xbar, 2)), colour = "steelblue") +
      labs(
        title = paste0("n = ", length(dat()),
                       "  |  red dashed = population density  |  red = μ  |  blue = x̄"),
        x = "Value", y = "Density"
      ) +
      theme_bw()
  })
  
  output$pct_table <- renderTable({
    req(dat())
    pop_vals  <- qnorm(probs, mean = input$mu, sd = input$sigma)
    samp_vals <- quantile(dat(), probs = probs)
    disc      <- samp_vals - pop_vals
    
    data.frame(
      Percentile   = labels,
      Population   = round(pop_vals,  3),
      Sample       = round(samp_vals, 3),
      Discrepancy  = round(disc,      3)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrr")
}

shinyApp(ui, server)