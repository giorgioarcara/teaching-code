library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("p-value via Simulation — Independent Samples t-test"),
  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Group 1", style = "color: #185FA5;"),
      numericInput("m1", "Mean (\u03bc\u2081)", value = 50, step = 1),
      numericInput("s1", "SD (\u03c3\u2081)",   value = 10, min = 0.1, step = 1),
      numericInput("n1", "n\u2081",              value = 30, min = 2,   step = 1),

      hr(),

      h4("Group 2", style = "color: #993C1D;"),
      numericInput("m2", "Mean (\u03bc\u2082)", value = 53, step = 1),
      numericInput("s2", "SD (\u03c3\u2082)",   value = 10, min = 0.1, step = 1),
      numericInput("n2", "n\u2082",              value = 30, min = 2,   step = 1),

      hr(),

      actionButton("simulate", "\u25b6  Simulate one sample",
                   width = "100%", style = "font-weight:500;"),
      br(), br(),
      fluidRow(
        column(6, actionButton("sim100",  "\u25b6\u25b6  +100",       width = "100%")),
        column(6, actionButton("sim1000", "\u25b6\u25b6\u25b6  +1000", width = "100%"))
      ),
      br(),
      actionButton("reset", "\u21ba  Reset", width = "100%", class = "btn-default"),

      hr(),

      h5("Compare observed t"),
      numericInput("obs_t", "Observed t-value", value = NA, step = 0.01),
      fluidRow(
        column(7, actionButton("check_t", "\u25c8  Check", width = "100%",
                               style = "font-weight:500;")),
        column(5, actionButton("clear_t", "\u00d7  Clear", width = "100%",
                               class = "btn-default"))
      ),
      br(),
      uiOutput("obs_result"),

      hr(),

      h5("Summary"),
      tableOutput("summary_table")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",

        # ── Tab 1: t-score distribution ────────────────────────────────────────
        tabPanel(
          "t-score distribution",
          br(),
          plotOutput("dot_plot", height = "460px"),
          br(),
          p(textOutput("caption_text", inline = TRUE),
            style = "color: #666; font-size: 13px;")
        ),

        # ── Tab 2: last sample histograms ──────────────────────────────────────
        tabPanel(
          "Last sample",
          br(),
          p("Histograms of the two groups drawn in the most recent simulation.",
            style = "color: #666; font-size: 13px; margin-bottom: 12px;"),
          plotOutput("sample_plot", height = "460px")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ── reactive stores ───────────────────────────────────────────────────────────
  sim_data    <- reactiveVal(data.frame(t = numeric(0), p = numeric(0)))
  active_t    <- reactiveVal(NULL)
  last_sample <- reactiveVal(NULL)   # list(g1 = numeric, g2 = numeric)

  # ── Welch-Satterthwaite df and critical value ─────────────────────────────────
  welch_df <- reactive({
    req(input$s1 > 0, input$s2 > 0, input$n1 >= 2, input$n2 >= 2)
    v1 <- input$s1^2 / input$n1
    v2 <- input$s2^2 / input$n2
    (v1 + v2)^2 / (v1^2 / (input$n1 - 1) + v2^2 / (input$n2 - 1))
  })

  crit_val <- reactive({ qt(0.975, df = welch_df()) })

  # ── simulation helper ─────────────────────────────────────────────────────────
  run_n <- function(n) {
    req(input$s1 > 0, input$s2 > 0, input$n1 >= 2, input$n2 >= 2)
    last_g1 <- last_g2 <- NULL
    results <- lapply(seq_len(n), function(i) {
      g1  <- rnorm(input$n1, mean = input$m1, sd = input$s1)
      g2  <- rnorm(input$n2, mean = input$m2, sd = input$s2)
      last_g1 <<- g1
      last_g2 <<- g2
      res <- t.test(g1, g2, var.equal = FALSE)
      data.frame(t = as.numeric(res$statistic), p = res$p.value)
    })
    sim_data(rbind(sim_data(), do.call(rbind, results)))
    last_sample(list(g1 = last_g1, g2 = last_g2))
  }

  observeEvent(input$simulate, run_n(1))
  observeEvent(input$sim100,   run_n(100))
  observeEvent(input$sim1000,  run_n(1000))

  observeEvent(input$reset, {
    sim_data(data.frame(t = numeric(0), p = numeric(0)))
    last_sample(NULL)
  })

  # ── observed-t controls ───────────────────────────────────────────────────────
  observeEvent(input$check_t, {
    req(!is.na(input$obs_t))
    active_t(input$obs_t)
  })

  observeEvent(input$clear_t, {
    active_t(NULL)
  })

  # ── observed-t result panel ───────────────────────────────────────────────────
  output$obs_result <- renderUI({
    ot <- active_t()
    df <- sim_data()
    if (is.null(ot) || nrow(df) == 0) return(NULL)

    emp_pct   <- mean(abs(df$t) <= abs(ot)) * 100
    emp_p     <- mean(abs(df$t) >= abs(ot))
    emp_p_fmt <- ifelse(emp_p < 0.001, "< .001", formatC(emp_p, digits = 3, format = "f"))

    tagList(
      div(
        style = paste0(
          "background:#f0f0f8; border-left:3px solid #7B2D8B;",
          "padding:8px 10px; border-radius:4px; font-size:12px; line-height:1.7;"
        ),
        tags$b(paste0("Observed t = ", round(ot, 3))), br(),
        paste0("Empirical percentile (|t|): ", round(emp_pct, 1), "%"), br(),
        paste0("Empirical two-tailed p: ", emp_p_fmt), br(),
        tags$span(style = "color:#888;",
                  paste0("Based on ", nrow(df), " simulations"))
      )
    )
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

  # ── Tab 1: dot / histogram plot ───────────────────────────────────────────────
  output$dot_plot <- renderPlot({

    df <- sim_data()
    cv <- crit_val()
    ot <- active_t()

    base <- ggplot() +
      annotate("rect", xmin = -Inf, xmax = -cv,
               ymin = -Inf, ymax = Inf, fill = "#D85A30", alpha = 0.07) +
      annotate("rect", xmin = cv, xmax = Inf,
               ymin = -Inf, ymax = Inf, fill = "#D85A30", alpha = 0.07) +
      geom_vline(xintercept = c(-cv, cv),
                 linetype = "dashed", colour = "#3B6D11", linewidth = 0.6) +
      geom_vline(xintercept = 0,
                 colour = "grey60", linewidth = 0.4) +
      annotate("text", x = -cv, y = Inf,
               label = paste0("\u2212", round(cv, 3)),
               vjust = -0.4, hjust = 1.1, size = 3.2, colour = "#3B6D11") +
      annotate("text", x = cv, y = Inf,
               label = paste0("+", round(cv, 3)),
               vjust = -0.4, hjust = -0.1, size = 3.2, colour = "#3B6D11") +
      xlim(-6, 6) +
      labs(
        x = "t-statistic", y = "Count",
        title = "Empirical distribution of simulated t-scores",
        fill = NULL, colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "bottom",
        plot.title         = element_text(face = "bold", size = 14)
      )

    if (!is.null(ot) && nrow(df) > 0) {
      emp_p     <- mean(abs(df$t) >= abs(ot))
      emp_pct   <- mean(abs(df$t) <= abs(ot)) * 100
      emp_p_fmt <- ifelse(emp_p < 0.001, "< .001", formatC(emp_p, digits = 3, format = "f"))
      obs_label <- paste0(
        "t = ", round(ot, 3), "\n",
        "pctile = ", round(emp_pct, 1), "%\n",
        "emp. p = ", emp_p_fmt
      )
      hjust_val <- if (ot >= 0) -0.08 else 1.08
      base <- base +
        geom_vline(xintercept = ot,
                   linetype = "dotted", colour = "#7B2D8B", linewidth = 1.1) +
        annotate("text", x = ot, y = Inf,
                 label = obs_label,
                 vjust = -0.05, hjust = hjust_val,
                 size = 3.1, colour = "#7B2D8B", lineheight = 1.3)
    }

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

  # ── Tab 2: last-sample histograms ────────────────────────────────────────────
  # Helper: build one group histogram with mean / median / sd lines
  make_group_hist <- function(x, group_label, fill_col, line_col) {
    m   <- mean(x)
    med <- median(x)
    s   <- sd(x)

    # shared x range centred on data for a tidy look
    xlo <- min(x) - 0.5 * s
    xhi <- max(x) + 0.5 * s

    # label positions (above bars, nudged to avoid overlap)
    bw <- diff(range(x)) / 20          # rough bin width
    y_top <- length(x) / 5             # rough label height

    df_hist <- data.frame(x = x)

    ggplot(df_hist, aes(x = x)) +
      geom_histogram(binwidth = bw, fill = fill_col, colour = "white",
                     alpha = 0.75, linewidth = 0.3) +

      # mean
      geom_vline(xintercept = m,   colour = line_col, linewidth = 1.0, linetype = "solid") +
      # median
      geom_vline(xintercept = med, colour = line_col, linewidth = 0.7, linetype = "dashed") +
      # mean - sd
      geom_vline(xintercept = m - s, colour = line_col, linewidth = 0.5, linetype = "dotted") +
      # mean + sd
      geom_vline(xintercept = m + s, colour = line_col, linewidth = 0.5, linetype = "dotted") +

      # annotations at top of each line
      annotate("text", x = m,     y = Inf,
               label = paste0("mean\n", round(m, 2)),
               vjust = -0.15, hjust = -0.1, size = 3.1,
               colour = line_col, fontface = "bold", lineheight = 1.1) +
      annotate("text", x = med,   y = Inf,
               label = paste0("median\n", round(med, 2)),
               vjust = -0.15, hjust =  1.1, size = 3.1,
               colour = line_col, lineheight = 1.1) +
      annotate("text", x = m - s, y = Inf,
               label = paste0("\u2212SD\n", round(m - s, 2)),
               vjust = -0.15, hjust =  1.1, size = 2.8,
               colour = line_col, lineheight = 1.1) +
      annotate("text", x = m + s, y = Inf,
               label = paste0("+SD\n", round(m + s, 2)),
               vjust = -0.15, hjust = -0.1, size = 2.8,
               colour = line_col, lineheight = 1.1) +

      # SD bracket label inside plot
      annotate("text", x = m, y = -Inf,
               label = paste0("SD = ", round(s, 2)),
               vjust = -0.4, hjust = 0.5, size = 3.0,
               colour = line_col) +

      coord_cartesian(clip = "off") +
      labs(
        title = group_label,
        x     = "Value",
        y     = "Count"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(face = "bold", colour = line_col, size = 13),
        plot.margin      = margin(t = 30, r = 10, b = 20, l = 10),
        panel.grid.minor = element_blank()
      )
  }

  output$sample_plot <- renderPlot({
    ls <- last_sample()
    if (is.null(ls)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Run at least one simulation to see the sample",
                 colour = "grey50", size = 5) +
        theme_void()
    } else {
      p1 <- make_group_hist(ls$g1, "Group 1", "#93C4EE", "#185FA5")
      p2 <- make_group_hist(ls$g2, "Group 2", "#F0A98A", "#993C1D")

      # stack vertically using base graphics layout via cowplot-free approach:
      # use gridExtra if available, otherwise patchwork, otherwise grid.arrange
      if (requireNamespace("patchwork", quietly = TRUE)) {
        library(patchwork)
        p1 / p2
      } else if (requireNamespace("gridExtra", quietly = TRUE)) {
        library(gridExtra)
        gridExtra::grid.arrange(p1, p2, ncol = 1)
      } else {
        # fallback: side by side using grid
        gridExtra::grid.arrange(p1, p2, ncol = 2)
      }
    }
  })

  # ── summary table ─────────────────────────────────────────────────────────────
  output$summary_table <- renderTable({
    df <- sim_data()
    if (nrow(df) == 0) {
      data.frame(
        Metric = c("Simulations", "Last t", "Last p", "% p < .05", "Crit. value"),
        Value  = c("0", "\u2014", "\u2014", "\u2014",
                   paste0("\u00b1", round(crit_val(), 3)))
      )
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
