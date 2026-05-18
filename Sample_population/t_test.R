library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("p-value via Simulation — Independent Samples t-test"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Group 1", style = "color: #185FA5;"),
      numericInput("m1", "Mean (μ₁)", value = 50, step = 1),
      numericInput("s1", "SD (σ₁)",   value = 10, min = 0.1, step = 1),
      numericInput("n1", "n₁",         value = 30, min = 2,   step = 1),

      hr(),

      h4("Group 2", style = "color: #993C1D;"),
      numericInput("m2", "Mean (μ₂)", value = 53, step = 1),
      numericInput("s2", "SD (σ₂)",   value = 10, min = 0.1, step = 1),
      numericInput("n2", "n₂",         value = 30, min = 2,   step = 1),

      hr(),

      actionButton("simulate", "▶  Run one simulation",
                   width = "100%",
                   style = "font-weight:500;"),

      br(), br(),

      fluidRow(
        column(6, actionButton("sim100",  "▶▶  +100",  width = "100%")),
        column(6, actionButton("sim1000", "▶▶▶  +1000", width = "100%"))
      ),

      br(),

      actionButton("reset", "↺  Reset",
                   width = "100%",
                   class = "btn-default"),

      br(), br(),

      h5("Summary"),
      tableOutput("summary_table")
    ),

    mainPanel(
      width = 9,
      plotOutput("dot_plot", height = "480px"),
      br(),
      p(textOutput("caption_text", inline = TRUE),
        style = "color: #666; font-size: 13px;")
    )
  )
)

server <- function(input, output, session) {

  # ── reactive store ────────────────────────────────────────────────────────────
  sim_data <- reactiveVal(data.frame(t = numeric(0), p = numeric(0)))

  # ── Welch-Satterthwaite df and critical value (reacts to n & sd inputs) ───────
  welch_df <- reactive({
    req(input$s1 > 0, input$s2 > 0, input$n1 >= 2, input$n2 >= 2)
    v1 <- input$s1^2 / input$n1
    v2 <- input$s2^2 / input$n2
    (v1 + v2)^2 / (v1^2 / (input$n1 - 1) + v2^2 / (input$n2 - 1))
  })

  crit_val <- reactive({
    qt(0.975, df = welch_df())
  })

  # ── shared simulation helper ──────────────────────────────────────────────────
  run_n <- function(n) {
    req(input$s1 > 0, input$s2 > 0, input$n1 >= 2, input$n2 >= 2)
    results <- lapply(seq_len(n), function(i) {
      g1  <- rnorm(input$n1, mean = input$m1, sd = input$s1)
      g2  <- rnorm(input$n2, mean = input$m2, sd = input$s2)
      res <- t.test(g1, g2, var.equal = FALSE)
      data.frame(t = as.numeric(res$statistic), p = res$p.value)
    })
    sim_data(rbind(sim_data(), do.call(rbind, results)))
  }

  observeEvent(input$simulate, run_n(1))
  observeEvent(input$sim100,   run_n(100))
  observeEvent(input$sim1000,  run_n(1000))

  observeEvent(input$reset, {
    sim_data(data.frame(t = numeric(0), p = numeric(0)))
  })

  # ── caption ───────────────────────────────────────────────────────────────────
  output$caption_text <- renderText({
    cv <- crit_val()
    df <- welch_df()
    paste0(
      "Each dot = one simulated sample. ",
      "Critical value: \u00b1", round(cv, 3),
      " (Welch df = ", round(df, 1), ", \u03b1 = .05, two-tailed). ",
      "Dots and shading beyond the dashed lines are significant."
    )
  })

  # ── dot plot ──────────────────────────────────────────────────────────────────
  output$dot_plot <- renderPlot({

    df <- sim_data()
    cv <- crit_val()

    base <- ggplot() +
      annotate("rect", xmin = -Inf, xmax = -cv,
               ymin = -Inf, ymax = Inf, fill = "#D85A30", alpha = 0.07) +
      annotate("rect", xmin =  cv,  xmax =  Inf,
               ymin = -Inf, ymax = Inf, fill = "#D85A30", alpha = 0.07) +
      geom_vline(xintercept = c(-cv, cv),
                 linetype = "dashed", colour = "#3B6D11", linewidth = 0.6) +
      geom_vline(xintercept = 0,
                 colour = "grey60", linewidth = 0.4) +
      annotate("text", x = -cv, y = Inf,
               label = paste0("\u2212", round(cv, 3)),
               vjust = -0.4, hjust =  1.1, size = 3.2, colour = "#3B6D11") +
      annotate("text", x =  cv, y = Inf,
               label = paste0("+", round(cv, 3)),
               vjust = -0.4, hjust = -0.1, size = 3.2, colour = "#3B6D11") +
      xlim(-5, 5) +
      labs(
        x     = "t-statistic",
        y     = "Count",
        title = "Empirical distribution of simulated t-scores",
        fill  = NULL, colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "bottom",
        plot.title         = element_text(face = "bold", size = 14)
      )

    if (nrow(df) == 0) {
      base +
        annotate("text", x = 0, y = 0.5,
                 label = "Hit a simulate button to begin",
                 colour = "grey50", size = 5)
    } else if (nrow(df) <= 300) {
      df$sig <- ifelse(df$p < 0.05, "p < .05 (significant)", "p \u2265 .05 (not significant)")
      base +
        geom_dotplot(
          data = df, aes(x = t, fill = sig, colour = sig),
          binwidth = 0.18, stackdir = "up", dotsize = 0.9, binpositions = "all"
        ) +
        scale_fill_manual(values = c(
          "p < .05 (significant)"          = "#D85A30",
          "p \u2265 .05 (not significant)" = "#378ADD")) +
        scale_colour_manual(values = c(
          "p < .05 (significant)"          = "#a33d1d",
          "p \u2265 .05 (not significant)" = "#185FA5")) +
        theme(legend.key.size = unit(0.8, "lines"))
    } else {
      df$sig <- ifelse(df$p < 0.05, "p < .05 (significant)", "p \u2265 .05 (not significant)")
      base +
        geom_histogram(
          data = df, aes(x = t, fill = sig),
          binwidth = 0.18, colour = "white", linewidth = 0.2
        ) +
        scale_fill_manual(values = c(
          "p < .05 (significant)"          = "#D85A30",
          "p \u2265 .05 (not significant)" = "#378ADD")) +
        theme(legend.key.size = unit(0.8, "lines"))
    }
  })

  # ── summary table ─────────────────────────────────────────────────────────────
  output$summary_table <- renderTable({
    df <- sim_data()
    if (nrow(df) == 0) {
      data.frame(Metric = c("Simulations", "Last t", "Last p", "% p < .05", "Crit. value"),
                 Value  = c("0", "\u2014", "\u2014", "\u2014",
                            paste0("\u00b1", round(crit_val(), 3))))
    } else {
      last    <- tail(df, 1)
      sig_pct <- mean(df$p < 0.05) * 100
      data.frame(
        Metric = c("Simulations", "Last t", "Last p", "% p < .05", "Crit. value"),
        Value  = c(
          as.character(nrow(df)),
          formatC(last$t, digits = 3, format = "f"),
          ifelse(last$p < 0.001, "< .001", formatC(last$p, digits = 3, format = "f")),
          paste0(formatC(sig_pct, digits = 1, format = "f"), "%"),
          paste0("\u00b1", round(crit_val(), 3))
        )
      )
    }
  }, striped = FALSE, bordered = FALSE, spacing = "s", align = "lr")
}

shinyApp(ui, server)
