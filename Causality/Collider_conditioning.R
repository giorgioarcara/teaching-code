library(shiny)
library(ggplot2)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Collider Bias Explorer",
  
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
      margin-bottom: 0;
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

    .control-block {
      margin-bottom: 28px;
    }

    .slider-label {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      margin-bottom: 6px;
    }
    .slider-label span {
      font-size: 13px;
      color: #444441;
    }
    .slider-label .val {
      font-family: 'IBM Plex Mono', monospace;
      font-size: 13px;
      font-weight: 500;
      color: #2c2c2a;
    }

    .js-irs-0 .irs-single, .js-irs-1 .irs-single {
      background: #3c3489 !important;
      font-family: 'IBM Plex Mono', monospace;
      font-size: 11px;
    }
    .js-irs-0 .irs-bar, .js-irs-1 .irs-bar {
      background: #3c3489 !important;
      border-top: 1px solid #3c3489 !important;
      border-bottom: 1px solid #3c3489 !important;
    }
    .js-irs-0 .irs-handle > i:first-child,
    .js-irs-1 .irs-handle > i:first-child {
      background: #534ab7 !important;
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
    .stat-card.highlight .value {
      color: #d4537e;
    }

    .toggle-btn {
      width: 100%;
      padding: 9px 0;
      background: #2c2c2a;
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
    .toggle-btn:hover { background: #444441; }
    .toggle-btn.off {
      background: transparent;
      color: #444441;
      border: 1px solid #d3d1c7;
    }
    .toggle-btn.off:hover { background: #f1efe8; }

    .legend-row {
      display: flex;
      gap: 18px;
      font-size: 12px;
      color: #888780;
      margin-bottom: 6px;
    }
    .legend-dot {
      width: 10px; height: 10px; border-radius: 50%;
      display: inline-block; margin-right: 5px; vertical-align: middle;
    }

    .info-box {
      background: #eeedfe;
      border-left: 3px solid #534ab7;
      border-radius: 0 6px 6px 0;
      padding: 12px 14px;
      font-size: 12px;
      color: #3c3489;
      line-height: 1.65;
      margin-top: 24px;
    }
    .info-box strong { font-weight: 500; }

    .label-inputs {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 10px;
      margin-bottom: 28px;
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
      border-color: #534ab7;
      background: #ffffff;
    }

    .plot-area {
      background: #ffffff;
      border: 1px solid #d3d1c7;
      border-radius: 10px;
      padding: 8px 4px 4px;
    }
  "))),
  
  div(class = "app-header",
      h1("Collider Bias Explorer"),
      p("Conditioning on a collider induces spurious correlations — illustrated interactively")
  ),
  
  div(class = "main-layout",
      
      # ── Sidebar ────────────────────────────────────────────────────────────────
      div(class = "sidebar",
          
          div(class = "section-label", "Variable labels"),
          
          div(class = "label-inputs",
              div(
                tags$label(`for` = "x_label", "X axis"),
                tags$input(id = "x_label", type = "text",
                           placeholder = "X  (exposure / predictor)",
                           oninput = "Shiny.setInputValue('x_label', this.value)")
              ),
              div(
                tags$label(`for` = "y_label", "Y axis"),
                tags$input(id = "y_label", type = "text",
                           placeholder = "Y  (outcome)",
                           oninput = "Shiny.setInputValue('y_label', this.value)")
              )
          ),
          
          div(class = "section-label", "Simulation controls"),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("True relationship (X → Y)"),
                  tags$span(class = "val", textOutput("slope_val", inline = TRUE))
              ),
              sliderInput("slope", label = NULL,
                          min = -1, max = 1, value = 0, step = 0.05,
                          width = "100%")
          ),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("Selection: top % on collider"),
                  tags$span(class = "val", textOutput("thresh_val", inline = TRUE))
              ),
              sliderInput("thresh", label = NULL,
                          min = 10, max = 70, value = 40, step = 5,
                          width = "100%")
          ),
          
          div(class = "control-block",
              div(class = "slider-label",
                  tags$span("Sample size (N)"),
                  tags$span(class = "val", textOutput("n_val", inline = TRUE))
              ),
              sliderInput("n_pts", label = NULL,
                          min = 200, max = 1200, value = 600, step = 100,
                          width = "100%")
          ),
          
          actionButton("toggle_sel", "Hide selection",
                       class = "toggle-btn", width = "100%"),
          
          div(class = "legend-row",
              span(tags$span(class = "legend-dot",
                             style = "background:#888780; opacity:0.5;"), "Full population"),
              span(tags$span(class = "legend-dot",
                             style = "background:#d4537e;"), "Selected subset")
          ),
          
          div(class = "info-box",
              tags$strong("How it works:"), br(),
              "Collider C = 0.7·X + 0.7·Y + noise. Selecting only high-C individuals",
              " (e.g. hospitalised patients, admitted students) conditions on C,",
              " inducing a negative X–Y correlation", tags$em(" within the selected group"),
              " regardless of the true relationship."
          )
      ),
      
      # ── Main panel ─────────────────────────────────────────────────────────────
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
  
  show_sel <- reactiveVal(TRUE)
  
  observeEvent(input$toggle_sel, {
    show_sel(!show_sel())
    label <- if (show_sel()) "Hide selection" else "Show selection"
    cls   <- if (show_sel()) "toggle-btn" else "toggle-btn off"
    updateActionButton(session, "toggle_sel", label = label)
    # swap class via JS
    session$sendCustomMessage("setClass",
                              list(id = "toggle_sel", cls = cls))
  })
  
  output$slope_val <- renderText({ sprintf("%+.2f", input$slope) })
  output$thresh_val <- renderText({ paste0("top ", input$thresh, "%") })
  output$n_val     <- renderText({ input$n_pts })
  
  sim_data <- reactive({
    set.seed(42)
    n <- input$n_pts
    x <- rnorm(n)
    slope <- input$slope
    y <- slope * x + sqrt(max(1 - slope^2, 0)) * rnorm(n)
    c_var <- 0.7 * x + 0.7 * y + 0.3 * rnorm(n)
    cutoff <- quantile(c_var, probs = 1 - input$thresh / 100)
    selected <- c_var >= cutoff
    data.frame(x = x, y = y, c_var = c_var, selected = selected)
  })
  
  output$stat_cards <- renderUI({
    df <- sim_data()
    r_pop <- cor(df$x, df$y)
    df_sel <- df[df$selected, ]
    r_sel  <- if (nrow(df_sel) > 1) cor(df_sel$x, df_sel$y) else NA
    n_sel  <- nrow(df_sel)
    
    sel_val  <- if (!is.na(r_sel) && show_sel()) sprintf("%.2f", r_sel) else "—"
    n_disp   <- if (show_sel()) as.character(n_sel) else "—"
    hl_class <- if (show_sel() && !is.na(r_sel) && r_sel < -0.1) "stat-card highlight" else "stat-card"
    
    div(class = "stat-grid",
        div(class = "stat-card",
            div(class = "label", "Population r"),
            div(class = "value", sprintf("%.2f", r_pop))
        ),
        div(class = hl_class,
            div(class = "label", "Selected r"),
            div(class = "value", sel_val)
        ),
        div(class = "stat-card",
            div(class = "label", "Selected n"),
            div(class = "value", n_disp)
        )
    )
  })
  
  output$scatter <- renderPlot({
    df     <- sim_data()
    show   <- show_sel()
    df_sel <- df[df$selected, ]
    
    # trend lines
    lm_pop <- lm(y ~ x, data = df)
    xr     <- range(df$x)
    trend_pop <- data.frame(
      x = xr,
      y = coef(lm_pop)[1] + coef(lm_pop)[2] * xr
    )
    
    p <- ggplot() +
      geom_point(data = df,
                 aes(x = x, y = y),
                 colour = "#888780", alpha = 0.25, size = 1.6, shape = 16) +
      geom_line(data = trend_pop,
                aes(x = x, y = y),
                colour = "#888780", linewidth = 1, linetype = "dashed")
    
    if (show && nrow(df_sel) > 1) {
      lm_sel    <- lm(y ~ x, data = df_sel)
      xr_sel    <- range(df_sel$x)
      trend_sel <- data.frame(
        x = xr_sel,
        y = coef(lm_sel)[1] + coef(lm_sel)[2] * xr_sel
      )
      p <- p +
        geom_point(data = df_sel,
                   aes(x = x, y = y),
                   colour = "#d4537e", alpha = 0.7, size = 2.2, shape = 16) +
        geom_line(data = trend_sel,
                  aes(x = x, y = y),
                  colour = "#d4537e", linewidth = 1.6)
    }
    
    x_lab <- if (!is.null(input$x_label) && nzchar(trimws(input$x_label)))
      input$x_label else "X  (exposure / predictor)"
    y_lab <- if (!is.null(input$y_label) && nzchar(trimws(input$y_label)))
      input$y_label else "Y  (outcome)"
    
    p +
      coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5)) +
      labs(x = x_lab, y = y_lab) +
      theme_minimal(base_family = "sans") +
      theme(
        panel.background  = element_rect(fill = "#ffffff", colour = NA),
        plot.background   = element_rect(fill = "#ffffff", colour = NA),
        panel.grid.major  = element_line(colour = "#d3d1c7", linewidth = 0.4),
        panel.grid.minor  = element_blank(),
        axis.title        = element_text(size = 12, colour = "#444441"),
        axis.text         = element_text(size = 10, colour = "#888780"),
        plot.margin       = margin(12, 16, 12, 12)
      )
  }, bg = "white")
}

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)