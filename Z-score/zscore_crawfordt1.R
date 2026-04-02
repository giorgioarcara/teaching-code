library(shiny)
library(bslib)
library(ggplot2)

# Crawford t function (Crawford & Howell, 1998)
crawford.t <- function(pat.score, control.scores, tails = c("lower", "upper", "two")) {
  n      <- length(control.scores)
  craw.t <- (pat.score - mean(control.scores)) / (sd(control.scores) * sqrt((n + 1) / n))
  df     <- n - 1
  
  if (tails[1] == "lower") p.val <- pt(craw.t, df = df, lower.tail = TRUE)
  if (tails[1] == "upper") p.val <- pt(craw.t, df = df, lower.tail = FALSE)
  if (tails[1] == "two")   p.val <- pt(abs(craw.t), df = df, lower.tail = FALSE) * 2
  
  list(t = craw.t, p = p.val, df = df)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Single Case vs. Normative Sample",
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$head(tags$style(HTML("
    /* ── Compact sidebar ── */
    .bslib-sidebar-layout > .sidebar { font-size: 0.78rem; }
    .bslib-sidebar-layout > .sidebar h5 {
      font-size: 0.82rem; font-weight: 700; margin-bottom: 4px; margin-top: 2px;
      text-transform: uppercase; letter-spacing: 0.03em; color: #2c3e50;
    }
    .bslib-sidebar-layout > .sidebar .form-group,
    .bslib-sidebar-layout > .sidebar .form-check { margin-bottom: 4px !important; }
    .bslib-sidebar-layout > .sidebar label { font-size: 0.75rem; margin-bottom: 1px; }
    .bslib-sidebar-layout > .sidebar .form-control,
    .bslib-sidebar-layout > .sidebar input[type='number'] {
      font-size: 0.75rem; padding: 2px 6px; height: 26px;
    }
    .bslib-sidebar-layout > .sidebar hr { margin: 6px 0; }
    .bslib-sidebar-layout > .sidebar .btn { font-size: 0.75rem; padding: 3px 8px; }
    .bslib-sidebar-layout > .sidebar .shiny-input-radiogroup label { font-size: 0.75rem; }

    /* ── Legend dl styling ── */
    .legend-box dt {
      font-weight: 600;
      color: #2c3e50;
      margin-top: 0.55rem;
    }
    .legend-box dd {
      margin-left: 1rem;
      color: #444;
      margin-bottom: 0;
    }
  "))),
  
  sidebar = sidebar(
    width = 240,
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
                 selected = "no", inline = TRUE)
  ),
  
  # ── Two-column main area ───────────────────────────────────────────────────
  layout_columns(
    col_widths = c(7, 5),
    gap = "1rem",
    
    # LEFT — histogram
    card(
      card_header("Distribution of simulated normative data"),
      plotOutput("hist_plot", height = "420px")
    ),
    
    # RIGHT — stats table stacked above legend
    layout_columns(
      col_widths = 12,
      gap = "1rem",
      
      card(
        card_header("Statistics"),
        tableOutput("stats_table"),
        height = "280px"
      ),
      
      card(
        card_header("Legend & Interpretation"),
        height = "195px",
        style = "overflow-y: auto;",
        tags$dl(
          class = "legend-box",
          style = "font-size: 0.82rem; line-height: 1.5; margin-bottom: 0;",
          
          tags$dt("Mean (SD)"),
          tags$dd("Arithmetic mean and standard deviation of the simulated normative sample."),
          
          tags$dt("Observed score"),
          tags$dd("The single patient or case score entered by the user."),
          
          tags$dt("Z-score"),
          tags$dd(HTML(
            "Standardised distance of the observed score from the sample mean:
             <em>z = (X &minus; M) / SD</em>.
             Assumes the normative sample is large enough for the normal approximation to hold."
          )),
          
          tags$dt("Percentile"),
          tags$dd(HTML(
            "Proportion of the normative sample estimated to score <em>at or below</em>
             the observed score, expressed as a percentage.
             Derived from the Z-score via the standard normal CDF."
          )),
          
          tags$dt("Crawford t (df)"),
          tags$dd(HTML(
            "Modified single-case <em>t</em>-statistic (Crawford &amp; Howell, 1998).
             Unlike the Z-score, this method accounts for uncertainty in the estimated
             mean and SD when the normative sample is small.
             Degrees of freedom = <em>N</em> &minus; 1."
          )),
          
          tags$dt("p-value (two-tailed)"),
          tags$dd(HTML(
            "Probability, under H<sub>0</sub> that the patient is drawn from the normative
             population, of obtaining a deviation at least as extreme as observed.
             Evaluated on the <em>t</em>-distribution with <em>N</em> &minus; 1 df.
             <strong>p &lt; .05</strong> is conventionally taken as evidence of a
             statistically unusual score."
          ))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  sample_data <- eventReactive(input$simulate, {
    req(input$n, input$mean, input$sd)
    set.seed(NULL)
    rnorm(n = input$n, mean = input$mean, sd = input$sd)
  }, ignoreNULL = FALSE)
  
  # Histogram
  output$hist_plot <- renderPlot({
    scores <- sample_data()
    req(scores, input$obs)
    
    ggplot(data.frame(score = scores), aes(x = score)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 30, fill = "#2c8c6e", colour = "white", alpha = 0.85) +
      geom_density(colour = "#1a5c47", linewidth = 0.9) +
      geom_vline(xintercept = input$obs,
                 linetype = "dashed", colour = "#e74c3c", linewidth = 1.2) +
      annotate("text", x = input$obs, y = Inf,
               label = paste0(" Obs = ", input$obs),
               hjust = -0.1, vjust = 1.5,
               colour = "#e74c3c", size = 4, fontface = "bold") +
      labs(x = "Score", y = "Density") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank())
  })
  
  # Statistics table
  output$stats_table <- renderTable({
    scores <- sample_data()
    req(scores, input$obs)
    
    z       <- (input$obs - mean(scores)) / sd(scores)
    pctile  <- pnorm(z) * 100
    mean_sd <- paste0(round(mean(scores), 2), " (", round(sd(scores), 2), ")")
    
    stats <- data.frame(
      Section   = c("Sample", "Observation", "Z-score", "Z-score"),
      Statistic = c("Mean (SD)", "Observed score", "Z-score", "Percentile"),
      Value     = c(mean_sd,
                    as.character(round(input$obs, 3)),
                    as.character(round(z, 3)),
                    paste0(round(pctile, 1), " %"))
    )
    
    if (input$show_crawford == "yes") {
      res  <- crawford.t(input$obs, scores, tails = "lower")
      t_df <- paste0(round(res$t, 3), " (df = ", res$df, ")")
      stats <- rbind(stats, data.frame(
        Section   = c("Crawford t", "Crawford t"),
        Statistic = c("t (df)", "p-value (lower-tail)"),
        Value     = c(t_df, formatC(res$p, format = "f", digits = 4))
      ))
    }
    
    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}

shinyApp(ui, server)
