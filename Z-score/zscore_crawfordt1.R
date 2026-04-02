library(shiny)
library(bslib)
library(ggplot2)

# Crawford t function (from crawford_t.R)
crawford.t <- function(pat.score, control.scores, tails = c("lower", "upper", "two")) {
  n <- length(control.scores)
  craw.t <- (pat.score - mean(control.scores)) / (sd(control.scores) * sqrt((n + 1) / n))
  df <- n - 1
  
  if (tails[1] == "lower")  p.val <- pt(craw.t, df = df, lower.tail = TRUE)
  if (tails[1] == "upper")  p.val <- pt(craw.t, df = df, lower.tail = FALSE)
  if (tails[1] == "two")    p.val <- pt(abs(craw.t), df = df, lower.tail = FALSE) * 2
  
  list(t = craw.t, p = p.val, df = df)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Single Case vs. Normative Sample",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 280,
    
    h5("Normative sample"),
    numericInput("n",    "N (observations)", value = 50,  min = 5,   step = 1),
    numericInput("mean", "Mean",             value = 100, step = 0.1),
    numericInput("sd",   "SD",               value = 15,  min = 0.01, step = 0.1),
    actionButton("simulate", "Simulate normative data",
                 class = "btn-primary w-100 mt-1"),
    
    hr(),
    
    h5("Single observation"),
    numericInput("obs", "Observed score", value = 70, step = 0.1),
    
    hr(),
    
    radioButtons("show_crawford", "Show Crawford t",
                 choices = c("No" = "no", "Yes" = "yes"),
                 selected = "no",
                 inline = TRUE)
  ),
  
  # Main panel
  card(
    card_header("Distribution of simulated normative data"),
    plotOutput("hist_plot", height = "380px")
  ),
  
  card(
    card_header("Statistics"),
    tableOutput("stats_table")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive: simulated data (re-generated on button press)
  sample_data <- eventReactive(input$simulate, {
    req(input$n, input$mean, input$sd)
    set.seed(NULL)
    rnorm(n = input$n, mean = input$mean, sd = input$sd)
  }, ignoreNULL = FALSE)  # run once on startup too
  
  # ── Histogram ──────────────────────────────────────────────────────────────
  output$hist_plot <- renderPlot({
    scores <- sample_data()
    req(scores, input$obs)
    
    obs <- input$obs
    df  <- data.frame(score = scores)
    
    ggplot(df, aes(x = score)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 30,
                     fill = "#2c8c6e", colour = "white", alpha = 0.85) +
      geom_density(colour = "#1a5c47", linewidth = 0.9) +
      geom_vline(xintercept = obs,
                 linetype = "dashed", colour = "#e74c3c", linewidth = 1.2) +
      annotate("text",
               x = obs, y = Inf,
               label = paste0(" Obs = ", obs),
               hjust = -0.1, vjust = 1.5,
               colour = "#e74c3c", size = 4, fontface = "bold") +
      labs(x = "Score", y = "Density") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank())
  })
  
  # ── Unified statistics table ───────────────────────────────────────────────
  output$stats_table <- renderTable({
    scores <- sample_data()
    req(scores, input$obs)
    
    z      <- (input$obs - mean(scores)) / sd(scores)
    pctile <- pnorm(z) * 100
    
    # Base rows
    stats <- data.frame(
      Section   = c("Sample", "Sample", "Observation", "Z-score", "Z-score"),
      Statistic = c("Mean", "SD", "Observed score", "Z-score", "Percentile"),
      Value     = c(round(mean(scores), 3),
                    round(sd(scores),   3),
                    round(input$obs,    3),
                    round(z,            3),
                    paste0(round(pctile, 1), " %"))
    )
    
    # Append Crawford rows when toggled on
    if (input$show_crawford == "yes") {
      res <- crawford.t(input$obs, scores, tails = "two")
      crawford_rows <- data.frame(
        Section   = rep("Crawford t", 3),
        Statistic = c("t-value", "Degrees of freedom", "p-value (two-tailed)"),
        Value     = c(round(res$t, 4),
                      as.character(res$df),
                      formatC(res$p, format = "f", digits = 4))
      )
      stats <- rbind(stats, crawford_rows)
    }
    
    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}

shinyApp(ui, server)