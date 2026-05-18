library(shiny)
library(ggplot2)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Collider Control Bias",
  
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500&family=IBM+Plex+Mono&display=swap');

    body {
      font-family: 'IBM Plex Sans', sans-serif;
      background-color: #f5f4f0;
      color: #2c2c2a;
      margin: 0;
    }

    .app-header {
      background: #2c2c2a;
      color: #f5f4f0;
      padding: 18px 32px 14px;
    }
    .app-header h1 {
      font-size: 20px;
      font-weight: 500;
      margin: 0 0 4px;
      letter-spacing: -0.3px;
    }
    .app-header p {
      font-size: 13px;
      margin: 0;
      color: #b4b2a9;
    }

    .main-layout {
      display: flex;
      gap: 0;
      min-height: calc(100vh - 70px);
    }

    .sidebar {
      width: 300px;
      min-width: 300px;
      background: #ffffff;
      border-right: 1px solid #d3d1c7;
      padding: 24px 20px;
      box-sizing: border-box;
      overflow-y: auto;
    }

    .main-panel {
      flex: 1;
      padding: 24px 28px;
      box-sizing: border-box;
    }

    .section-label {
      font-size: 11px;
      font-weight: 500;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      color: #888780;
      margin: 0 0 12px;
    }

    .control-block { margin-bottom: 24px; }

    .slider-label {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      margin-bottom: 6px;
    }
    .slider-label span { font-size: 13px; color: #444441; }
    .slider-label .val {
      font-family: 'IBM Plex Mono', monospace;
      font-size: 13px;
      font-weight: 500;
      color: #2c2c2a;
    }

    .stat-grid {
      display: grid;
      grid-template-columns: 1fr 1fr 1fr;
      gap: 10px;
      margin-bottom: 20px;
    }
    .stat-card {
      background: #ffffff;
      border: 1px solid #d3d1c7;
      border-radius: 8px;
      padding: 12px 14px;
    }
    .stat-card .label {
      font-size: 11px;
      color: #888780;
      margin-bottom: 4px;
      text-transform: uppercase;
      letter-spacing: 0.06em;
    }
    .stat-card .value {
      font-size: 22px;
      font-weight: 500;
      font-family: 'IBM Plex Mono', monospace;
      color: #2c2c2a;
    }
    .stat-card.warn .value  { color: #ba7517; }
    .stat-card.bad  .value  { color: #d4537e; }

    .toggle-btn {
      width: 100%;
      padding: 9px 0;
      background: #0f6e56;
      color: #f5f4f0;
      border: none;
      border-radius: 6px;
      font-family: 'IBM Plex Sans', sans-serif;
      font-size: 13px;
      font-weight: 500;
      cursor: pointer;
      margin-bottom: 20px;
      transition: background 0.15s;
    }
    .toggle-btn:hover { background: #085041; }
    .toggle-btn.off {
      background: transparent;
      color: #444441;
      border: 1px solid #d3d1c7;
    }
    .toggle-btn.off:hover { background: #f1efe8; }

    .legend-row {
      display: flex;
      flex-direction: column;
      gap: 6px;
      font-size: 12px;
      color: #888780;
      margin-bottom: 16px;
    }
    .legend-item { display: flex; align-items: center; gap: 8px; }
    .legend-line {
      width: 22px; height: 3px;
      display: inline-block; border-radius: 2px;
    }
    .legend-line.dashed {
      background: repeating-linear-gradient(
        to right, #888780 0, #888780 5px, transparent 5px, transparent 9px);
    }

    .info-box {
      background: #eaf3de;
      border-left: 3px solid #3b6d11;
      border-radius: 0 6px 6px 0;
      padding: 12px 14px;
      font-size: 12px;
      color: #27500a;
      line-height: 1.65;
      margin-top: 8px;
    }
    .info-box strong { font-weight: 500; }

    .label-inputs {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 10px;
      margin-bottom: 24px;
    }
    .label-inputs label {
      font-size: 11px;
      color: #888780;
      display: block;
      margin-bottom: 4px;
      text-transform: uppercase;
      letter-spacing: 0.06em;
    }
    .label-inputs input[type=text] {
      width: 100%;
      padding: 6px 8px;
      font-size: 13px;
      font-family: 'IBM Plex Sans', sans-serif;
      border: 1px solid #d3d1c7;
      border-radius: 5px;
      background: #f5f4f0;
      color: #2c2c2a;
      box-sizing: border-box;
      outline: none;
      transition: border-color 0.15s;
    }
    .label-inputs input[type=text]:focus {
      border-color: #3b6d11;
      background: #ffffff;
    }

    .plot-area {
      background: #ffffff;
      border: 1px solid #d3d1c7;
      border-radius: 10px;
      padding: 8px 4px 4px;
    }

    hr.divider {
      border: none;
      border-top: 1px solid #d3d1c7;
      margin: 20px 0;
    }
  "))),
  
  div(class = "app-header",
      h1("Collider Control Bias"),
      p("Adjusting for a collider covariate opens a backdoor path — inducing spurious X\u2013Y associations")
  ),
  
  div(class = "main-layout",
      
      # ── Sidebar ───────────────────────────────────────────────────────────────
      div(class = "sidebar",
          
          div(class = "section-label", "Variable labels"),
          
          div(class = "label-inputs",
              div(
                tags$label(`for` = "x_label", "X axis"),
                tags$input(id = "x_label", type = "text",
                           placeholder = "X  (exposure)",
                           oninput = "Shiny.setInputValue('x_label', this.value)")
              ),
              div(
                tags$label(`for` = "y_label", "Y axis"),
                tags$input(id = "y_label", type = "text",
                           placeholder = "Y  (outcome)",
                           oninput = "Shiny.setInputValue('y_label', this.value)")
              )
          ),
          
          tags$hr(class = "divider"),
          div(class = "section-label", "Causal structure"),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("True X \u2192 Y effect (\u03b2)"),
                  tags$span(class = "val", textOutput("slope_val", inline = TRUE))
              ),
              sliderInput("slope", label = NULL,
                          min = -1, max = 1, value = 0, step = 0.05, width = "100%")
          ),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("X \u2192 C path (\u03b31)"),
                  tags$span(class = "val", textOutput("gam1_val", inline = TRUE))
              ),
              sliderInput("gam1", label = NULL,
                          min = 0, max = 1, value = 0.7, step = 0.05, width = "100%")
          ),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("Y \u2192 C path (\u03b32)"),
                  tags$span(class = "val", textOutput("gam2_val", inline = TRUE))
              ),
              sliderInput("gam2", label = NULL,
                          min = 0, max = 1, value = 0.7, step = 0.05, width = "100%")
          ),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("Sample size (N)"),
                  tags$span(class = "val", textOutput("n_val", inline = TRUE))
              ),
              sliderInput("n_pts", label = NULL,
                          min = 200, max = 1200, value = 600, step = 100, width = "100%")
          ),
          
          tags$hr(class = "divider"),
          
          actionButton("toggle_adj", "Control for C (adjusted)",
                       class = "toggle-btn", width = "100%"),
          
          div(class = "legend-row",
              div(class = "legend-item",
                  tags$span(class = "legend-line dashed"),
                  "Unadjusted X\u2013Y trend"
              ),
              div(class = "legend-item",
                  tags$span(class = "legend-line",
                            style = "background:#0f6e56;"),
                  "Adjusted trend (controlling C)"
              )
          ),
          
          div(class = "info-box",
              tags$strong("How it works:"), br(),
              "C is caused by both X and Y (a collider). Adding C to a regression",
              " of Y on X opens the path X \u2192 C \u2190 Y,",
              " creating a spurious partial association between X and Y",
              tags$em(" even when the true causal effect is zero.")
          )
      ),
      
      # ── Main panel ────────────────────────────────────────────────────────────
      div(class = "main-panel",
          
          uiOutput("stat_cards"),
          
          div(class = "plot-area",
              plotOutput("scatter", height = "460px")
          )
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  show_adj <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_adj, {
    show_adj(!show_adj())
    label <- if (show_adj()) "Hide adjusted trend" else "Control for C (adjusted)"
    updateActionButton(session, "toggle_adj", label = label)
  })
  
  output$slope_val <- renderText({ sprintf("%+.2f", input$slope) })
  output$gam1_val  <- renderText({ sprintf("%.2f",  input$gam1)  })
  output$gam2_val  <- renderText({ sprintf("%.2f",  input$gam2)  })
  output$n_val     <- renderText({ input$n_pts })
  
  # ── Simulate data ──────────────────────────────────────────────────────────
  # DAG: X independent, Y = slope*X + e_y, C = gam1*X + gam2*Y + e_c
  sim_data <- reactive({
    set.seed(42)
    n     <- input$n_pts
    slope <- input$slope
    gam1  <- input$gam1
    gam2  <- input$gam2
    
    x   <- rnorm(n)
    e_y <- rnorm(n)
    y   <- slope * x + sqrt(max(1 - slope^2, 1e-6)) * e_y
    c_v <- gam1 * x + gam2 * y + 0.3 * rnorm(n)
    
    # Standardise C for display
    c_v <- scale(c_v)[, 1]
    
    data.frame(x = x, y = y, c = c_v)
  })
  
  # ── Regression coefficients ────────────────────────────────────────────────
  regs <- reactive({
    df       <- sim_data()
    lm_crude <- lm(y ~ x, data = df)
    lm_adj   <- lm(y ~ x + c, data = df)
    
    # FWL residuals for partial-regression plot
    res_x <- residuals(lm(x ~ c, data = df))
    res_y <- residuals(lm(y ~ c, data = df))
    
    list(
      crude_coef = coef(lm_crude)["x"],
      adj_coef   = coef(lm_adj)["x"],
      res_x      = res_x,
      res_y      = res_y,
      df         = df
    )
  })
  
  # ── Stat cards ─────────────────────────────────────────────────────────────
  output$stat_cards <- renderUI({
    r   <- regs()
    adj <- show_adj()
    
    crude <- sprintf("%+.3f", r$crude_coef)
    adjv  <- sprintf("%+.3f", r$adj_coef)
    bias  <- sprintf("%+.3f", r$adj_coef - r$crude_coef)
    true  <- sprintf("%+.2f", input$slope)
    
    delta <- abs(r$adj_coef - r$crude_coef)
    adj_cls  <- if (adj && delta > 0.05) "stat-card bad" else "stat-card"
    bias_cls <- if (adj && delta > 0.05) "stat-card warn" else "stat-card"
    
    div(class = "stat-grid",
        div(class = "stat-card",
            div(class = "label", "True \u03b2"),
            div(class = "value", true)
        ),
        div(class = "stat-card",
            div(class = "label", "Crude \u03b2 (X only)"),
            div(class = "value", crude)
        ),
        div(class = adj_cls,
            div(class = "label", "Adjusted \u03b2 (+C)"),
            div(class = "value", if (adj) adjv else "\u2014")
        )
    )
  })
  
  # ── Main plot ──────────────────────────────────────────────────────────────
  output$scatter <- renderPlot({
    r    <- regs()
    adj  <- show_adj()
    df   <- r$df
    
    x_lab <- if (!is.null(input$x_label) && nzchar(trimws(input$x_label)))
      input$x_label else "X  (exposure)"
    y_lab <- if (!is.null(input$y_label) && nzchar(trimws(input$y_label)))
      input$y_label else "Y  (outcome)"
    
    # Crude trend
    lm_crude <- lm(y ~ x, data = df)
    xr        <- range(df$x)
    trend_crude <- data.frame(
      x = xr,
      y = coef(lm_crude)[1] + coef(lm_crude)[2] * xr
    )
    
    if (!adj) {
      # ── Panel A: raw scatter + crude trend ──────────────────────────────────
      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(colour = "#888780", alpha = 0.22, size = 1.7, shape = 16) +
        geom_line(data = trend_crude, aes(x = x, y = y),
                  colour = "#888780", linewidth = 1.2, linetype = "dashed") +
        coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5)) +
        labs(x = x_lab, y = y_lab,
             title = "Unadjusted: Y ~ X")
      
    } else {
      # ── Panel B: partial-regression (FWL) plot ──────────────────────────────
      # Residuals after partialling out C from both X and Y
      fwl <- data.frame(rx = r$res_x, ry = r$res_y)
      lm_fwl <- lm(ry ~ rx, data = fwl)
      rxr <- range(fwl$rx)
      trend_fwl <- data.frame(
        rx = rxr,
        ry = coef(lm_fwl)[1] + coef(lm_fwl)[2] * rxr
      )
      
      # Also show crude trend projected on the residual scale for reference
      # (slope only — centred at 0)
      crude_b <- coef(lm_crude)[2]
      trend_crude_fwl <- data.frame(
        rx = rxr,
        ry = crude_b * rxr
      )
      
      p <- ggplot(fwl, aes(x = rx, y = ry)) +
        geom_point(colour = "#1d9e75", alpha = 0.30, size = 1.7, shape = 16) +
        geom_line(data = trend_crude_fwl, aes(x = rx, y = ry),
                  colour = "#888780", linewidth = 1, linetype = "dashed") +
        geom_line(data = trend_fwl, aes(x = rx, y = ry),
                  colour = "#0f6e56", linewidth = 1.8) +
        labs(
          x = paste0(x_lab, "  |  C  (residual)"),
          y = paste0(y_lab, "  |  C  (residual)"),
          title = "Adjusted: Y ~ X + C  \u2014 partial regression (FWL)"
        )
    }
    
    p +
      theme_minimal(base_family = "sans") +
      theme(
        panel.background  = element_rect(fill = "#ffffff", colour = NA),
        plot.background   = element_rect(fill = "#ffffff", colour = NA),
        panel.grid.major  = element_line(colour = "#d3d1c7", linewidth = 0.4),
        panel.grid.minor  = element_blank(),
        axis.title        = element_text(size = 12, colour = "#444441"),
        axis.text         = element_text(size = 10, colour = "#888780"),
        plot.title        = element_text(size = 13, colour = "#2c2c2a",
                                         face = "plain", margin = margin(b = 8)),
        plot.margin       = margin(12, 16, 12, 12)
      )
  }, bg = "white")
}

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)