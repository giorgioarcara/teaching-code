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

      actionButton("simulate", "▶  Simulate one sample",
                   width = "100%",
                   style = "font-weight:500;"),

      br(), br(),

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
      p("Each dot = one simulated sample. Dots beyond the dashed critical lines (±1.96) are significant at α = .05.",
        style = "color: #666; font-size: 13px;")
    )
  )
)

server <- function(input, output, session) {

  # ── reactive store ────────────────────────────────────────────────────────────
  sim_data <- reactiveVal(data.frame(t = numeric(0), p = numeric(0)))

  # ── run one simulation ────────────────────────────────────────────────────────
  observeEvent(input$simulate, {

    req(input$s1 > 0, input$s2 > 0, input$n1 >= 2, input$n2 >= 2)

    g1 <- rnorm(input$n1, mean = input$m1, sd = input$s1)
    g2 <- rnorm(input$n2, mean = input$m2, sd = input$s2)

    result  <- t.test(g1, g2, var.equal = FALSE)
    t_stat  <- as.numeric(result$statistic)
    p_val   <- result$p.value

    new_row <- data.frame(t = t_stat, p = p_val)
    sim_data(rbind(sim_data(), new_row))
  })

  # ── reset ─────────────────────────────────────────────────────────────────────
  observeEvent(input$reset, {
    sim_data(data.frame(t = numeric(0), p = numeric(0)))
  })

  # ── dot plot ──────────────────────────────────────────────────────────────────
  output$dot_plot <- renderPlot({

    df <- sim_data()

    base <- ggplot() +
      # shaded rejection regions
      annotate("rect", xmin = -Inf, xmax = -1.96,
               ymin = -Inf, ymax = Inf,
               fill = "#D85A30", alpha = 0.07) +
      annotate("rect", xmin =  1.96, xmax =  Inf,
               ymin = -Inf, ymax = Inf,
               fill = "#D85A30", alpha = 0.07) +
      # critical value lines
      geom_vline(xintercept = c(-1.96, 1.96),
                 linetype = "dashed", colour = "#3B6D11", linewidth = 0.6) +
      geom_vline(xintercept = 0,
                 colour = "grey60", linewidth = 0.4) +
      annotate("text", x = -1.96, y = Inf,
               label = "−1.96", vjust = -0.4, hjust =  1.1,
               size = 3.2, colour = "#3B6D11") +
      annotate("text", x =  1.96, y = Inf,
               label = "+1.96", vjust = -0.4, hjust = -0.1,
               size = 3.2, colour = "#3B6D11") +
      xlim(-5, 5) +
      labs(
        x     = "t-statistic",
        y     = "Count (stacked)",
        title = "Empirical distribution of simulated t-scores",
        colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position   = "bottom",
        plot.title        = element_text(face = "bold", size = 14)
      )

    if (nrow(df) == 0) {
      base +
        annotate("text", x = 0, y = 0.5,
                 label = "Hit 'Simulate one sample' to begin",
                 colour = "grey50", size = 5)
    } else {
      df$sig <- ifelse(df$p < 0.05, "p < .05 (significant)", "p ≥ .05 (not significant)")

      base +
        geom_dotplot(
          data     = df,
          aes(x = t, fill = sig, colour = sig),
          binwidth  = 0.18,
          stackdir  = "up",
          dotsize   = 0.9,
          binpositions = "all"
        ) +
        scale_fill_manual(
          values = c("p < .05 (significant)"     = "#D85A30",
                     "p ≥ .05 (not significant)" = "#378ADD")
        ) +
        scale_colour_manual(
          values = c("p < .05 (significant)"     = "#a33d1d",
                     "p ≥ .05 (not significant)" = "#185FA5")
        ) +
        theme(legend.key.size = unit(0.8, "lines"))
    }
  })

  # ── summary table ─────────────────────────────────────────────────────────────
  output$summary_table <- renderTable({

    df <- sim_data()
    if (nrow(df) == 0) {
      data.frame(Metric = c("Simulations", "Last t", "Last p", "% p < .05"),
                 Value  = c("0", "—", "—", "—"))
    } else {
      last   <- tail(df, 1)
      sig_pct <- mean(df$p < 0.05) * 100
      data.frame(
        Metric = c("Simulations", "Last t", "Last p", "% p < .05"),
        Value  = c(
          as.character(nrow(df)),
          formatC(last$t, digits = 3, format = "f"),
          ifelse(last$p < 0.001, "< .001", formatC(last$p, digits = 3, format = "f")),
          paste0(formatC(sig_pct, digits = 1, format = "f"), "%")
        )
      )
    }
  }, striped = FALSE, bordered = FALSE, spacing = "s", align = "lr")
}

shinyApp(ui, server)
