library(shiny)
library(ggplot2)

# ── DAG drawing helpers ───────────────────────────────────────────────────────

draw_arrow <- function(x0, y0, x1, y1, r = 0.085,
                       lwd = 1, col = "#534ab7", arr_len = 0.10) {
  dx <- x1 - x0; dy <- y1 - y0; d <- sqrt(dx^2 + dy^2)
  ux <- dx / d;  uy <- dy / d
  lines(c(x0 + ux*r, x1 - ux*r), c(y0 + uy*r, y1 - uy*r),
        lwd = lwd, col = col, lend = 1)
  arrows(x0 + ux*r, y0 + uy*r, x1 - ux*r, y1 - uy*r,
         length = arr_len, angle = 20, lwd = lwd, col = col, code = 2)
}

draw_node <- function(cx, cy, label, r = 0.085,
                      fill = "#eeedfe", border = "#534ab7",
                      txt_col = "#2c2c2a", cex = 0.82) {
  theta <- seq(0, 2*pi, length.out = 120)
  polygon(cx + r*cos(theta), cy + r*sin(theta),
          col = fill, border = border, lwd = 1.8)
  lbl <- if (nchar(label) > 13) paste0(substr(label, 1, 11), "\u2026") else label
  text(cx, cy, lbl, cex = cex, col = txt_col, adj = c(0.5, 0.5))
}

corr_lwd <- function(r) 0.8 + abs(r) * 5.5

corr_col <- function(r, pos_hex = "#534ab7", neg_hex = "#d4537e") {
  if (is.na(r) || r == 0) return("#cccccc")
  base <- if (r >= 0) pos_hex else neg_hex
  a    <- 0.20 + 0.70 * abs(r)
  bc   <- col2rgb(base) / 255
  gc   <- col2rgb("#b4b2a9") / 255
  m    <- bc * a + gc * (1 - a)
  rgb(m[1], m[2], m[3])
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Collider Bias Explorer",

  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500&family=IBM+Plex+Mono&display=swap');
    body { font-family:'IBM Plex Sans',sans-serif; background:#f5f4f0; color:#2c2c2a; margin:0; }
    .app-header { background:#2c2c2a; color:#f5f4f0; padding:18px 32px 14px; }
    .app-header h1 { font-size:20px; font-weight:500; margin:0 0 4px; letter-spacing:-0.3px; }
    .app-header p  { font-size:13px; margin:0; color:#b4b2a9; }
    .main-layout   { display:flex; gap:0; align-items:flex-start; }
    .sidebar {
      width:300px; min-width:300px; background:#ffffff;
      border-right:1px solid #d3d1c7; padding:24px 20px;
      box-sizing:border-box; position:sticky; top:0;
      max-height:100vh; overflow-y:auto;
    }
    .main-panel { flex:1; padding:24px 28px; box-sizing:border-box; overflow-y:auto; }
    .section-label {
      font-size:11px; font-weight:500; letter-spacing:0.08em;
      text-transform:uppercase; color:#888780; margin:0 0 12px;
    }
    .control-block { margin-bottom:28px; }
    .slider-label  { display:flex; justify-content:space-between; align-items:baseline; margin-bottom:6px; }
    .slider-label span { font-size:13px; color:#444441; }
    .slider-label .val { font-family:'IBM Plex Mono',monospace; font-size:13px; font-weight:500; color:#2c2c2a; }
    .stat-grid { display:grid; grid-template-columns:1fr 1fr 1fr; gap:10px; margin-bottom:20px; }
    .stat-card { background:#ffffff; border:1px solid #d3d1c7; border-radius:8px; padding:12px 14px; }
    .stat-card .label { font-size:11px; color:#888780; margin-bottom:4px; text-transform:uppercase; letter-spacing:0.06em; }
    .stat-card .value { font-size:22px; font-weight:500; font-family:'IBM Plex Mono',monospace; color:#2c2c2a; }
    .stat-card.highlight .value { color:#d4537e; }
    .toggle-btn {
      width:100%; padding:9px 0; background:#2c2c2a; color:#f5f4f0; border:none;
      border-radius:6px; font-family:'IBM Plex Sans',sans-serif; font-size:13px;
      font-weight:500; cursor:pointer; margin-bottom:20px; transition:background 0.15s;
    }
    .toggle-btn:hover { background:#444441; }
    .toggle-btn.off { background:transparent; color:#444441; border:1px solid #d3d1c7; }
    .toggle-btn.off:hover { background:#f1efe8; }
    .legend-row { display:flex; gap:18px; font-size:12px; color:#888780; margin-bottom:6px; }
    .legend-dot { width:10px; height:10px; border-radius:50%; display:inline-block; margin-right:5px; vertical-align:middle; }
    .info-box {
      background:#eeedfe; border-left:3px solid #534ab7; border-radius:0 6px 6px 0;
      padding:12px 14px; font-size:12px; color:#3c3489; line-height:1.65; margin-top:24px;
    }
    .info-box strong { font-weight:500; }
    .label-inputs { display:grid; grid-template-columns:1fr 1fr; gap:10px; margin-bottom:28px; }
    .label-inputs label { font-size:11px; color:#888780; display:block; margin-bottom:4px; text-transform:uppercase; letter-spacing:0.06em; }
    .label-inputs input[type=text] {
      width:100%; padding:6px 8px; font-size:13px; font-family:'IBM Plex Sans',sans-serif;
      border:1px solid #d3d1c7; border-radius:5px; background:#f5f4f0; color:#2c2c2a;
      box-sizing:border-box; outline:none; transition:border-color 0.15s;
    }
    .label-inputs input[type=text]:focus { border-color:#534ab7; background:#ffffff; }
    .plot-area { background:#ffffff; border:1px solid #d3d1c7; border-radius:10px; padding:8px 4px 4px; }
    .dag-area  { background:#ffffff; border:1px solid #d3d1c7; border-radius:10px; padding:0 4px 4px; margin-top:16px; }
    .dag-title { font-size:11px; font-weight:500; letter-spacing:0.07em; text-transform:uppercase; color:#888780; padding:10px 12px 0; }
  "))),

  div(class = "app-header",
    h1("Collider Bias Explorer"),
    p("Conditioning on a collider induces spurious correlations \u2014 illustrated interactively")
  ),

  div(class = "main-layout",

    div(class = "sidebar",

      div(class = "section-label", "Variable labels"),
      div(class = "label-inputs",
        div(
          tags$label(`for` = "x_label", "X axis"),
          tags$input(id = "x_label", type = "text", placeholder = "X",
                     oninput = "Shiny.setInputValue('x_label', this.value)")
        ),
        div(
          tags$label(`for` = "y_label", "Y axis"),
          tags$input(id = "y_label", type = "text", placeholder = "Y",
                     oninput = "Shiny.setInputValue('y_label', this.value)")
        )
      ),

      div(class = "section-label", "Simulation controls"),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("True relationship (X \u2192 Y)"),
          tags$span(class = "val", textOutput("slope_val", inline = TRUE))
        ),
        sliderInput("slope", NULL, min=-1, max=1, value=0, step=0.05, width="100%")
      ),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("Selection: top % on collider"),
          tags$span(class = "val", textOutput("thresh_val", inline = TRUE))
        ),
        sliderInput("thresh", NULL, min=10, max=70, value=40, step=5, width="100%")
      ),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("Sample size (N)"),
          tags$span(class = "val", textOutput("n_val", inline = TRUE))
        ),
        sliderInput("n_pts", NULL, min=200, max=1200, value=600, step=100, width="100%")
      ),

      actionButton("toggle_sel", "Hide selection", class="toggle-btn", width="100%"),

      div(class = "legend-row",
        span(tags$span(class="legend-dot", style="background:#888780;opacity:0.5;"), "Full population"),
        span(tags$span(class="legend-dot", style="background:#d4537e;"), "Selected subset")
      ),

      div(class = "info-box",
        tags$strong("How it works:"), tags$br(),
        "Collider C = 0.7\u00b7X + 0.7\u00b7Y + noise.",
        " Selecting only high-C individuals conditions on C,",
        " inducing a negative X\u2013Y correlation",
        tags$em(" within the selected group"),
        " regardless of the true relationship."
      )
    ),

    div(class = "main-panel",

      uiOutput("stat_cards"),

      div(class = "plot-area",
        plotOutput("scatter", height = "400px")
      ),

      div(class = "dag-area",
        div(class = "dag-title", "Sample DAG \u2014 arrow width \u221d |r|"),
        plotOutput("dag_plot", height = "260px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  show_sel <- reactiveVal(TRUE)

  observeEvent(input$toggle_sel, {
    show_sel(!show_sel())
    updateActionButton(session, "toggle_sel",
      label = if (show_sel()) "Hide selection" else "Show selection")
  })

  output$slope_val  <- renderText(sprintf("%+.2f", input$slope))
  output$thresh_val <- renderText(paste0("top ", input$thresh, "%"))
  output$n_val      <- renderText(input$n_pts)

  x_lab <- reactive({
    v <- input$x_label
    if (!is.null(v) && nzchar(trimws(v))) trimws(v) else "X"
  })
  y_lab <- reactive({
    v <- input$y_label
    if (!is.null(v) && nzchar(trimws(v))) trimws(v) else "Y"
  })

  sim_data <- reactive({
    set.seed(42)
    n     <- input$n_pts
    slope <- input$slope
    x     <- rnorm(n)
    y     <- slope * x + sqrt(max(1 - slope^2, 0)) * rnorm(n)
    c_var <- 0.7 * x + 0.7 * y + 0.3 * rnorm(n)
    cutoff   <- quantile(c_var, probs = 1 - input$thresh / 100)
    selected <- c_var >= cutoff
    data.frame(x=x, y=y, c_var=c_var, selected=selected)
  })

  output$stat_cards <- renderUI({
    df     <- sim_data()
    r_pop  <- cor(df$x, df$y)
    df_sel <- df[df$selected, ]
    r_sel  <- if (nrow(df_sel) > 1) cor(df_sel$x, df_sel$y) else NA
    n_sel  <- nrow(df_sel)

    sel_val  <- if (!is.na(r_sel) && show_sel()) sprintf("%.2f", r_sel) else "\u2014"
    n_disp   <- if (show_sel()) as.character(n_sel) else "\u2014"
    hl_cls   <- if (show_sel() && !is.na(r_sel) && r_sel < -0.1) "stat-card highlight" else "stat-card"

    div(class="stat-grid",
      div(class="stat-card",
        div(class="label","Population r"), div(class="value", sprintf("%.2f", r_pop))),
      div(class=hl_cls,
        div(class="label","Selected r"),   div(class="value", sel_val)),
      div(class="stat-card",
        div(class="label","Selected n"),   div(class="value", n_disp))
    )
  })

  output$scatter <- renderPlot({
    df     <- sim_data()
    show   <- show_sel()
    df_sel <- df[df$selected, ]
    lm_pop <- lm(y ~ x, data=df)
    xr     <- range(df$x)
    tp     <- data.frame(x=xr, y=coef(lm_pop)[1] + coef(lm_pop)[2]*xr)

    p <- ggplot() +
      geom_point(data=df, aes(x=x,y=y), colour="#888780", alpha=0.25, size=1.6, shape=16) +
      geom_line(data=tp, aes(x=x,y=y), colour="#888780", linewidth=1, linetype="dashed")

    if (show && nrow(df_sel) > 1) {
      lm_sel <- lm(y ~ x, data=df_sel)
      xrs    <- range(df_sel$x)
      ts     <- data.frame(x=xrs, y=coef(lm_sel)[1] + coef(lm_sel)[2]*xrs)
      p <- p +
        geom_point(data=df_sel, aes(x=x,y=y), colour="#d4537e", alpha=0.7, size=2.2, shape=16) +
        geom_line(data=ts, aes(x=x,y=y), colour="#d4537e", linewidth=1.6)
    }

    p + coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5)) +
      labs(x=paste0(x_lab()," (exposure / predictor)"), y=paste0(y_lab()," (outcome)")) +
      theme_minimal(base_family="sans") +
      theme(
        panel.background=element_rect(fill="#ffffff",colour=NA),
        plot.background =element_rect(fill="#ffffff",colour=NA),
        panel.grid.major=element_line(colour="#d3d1c7",linewidth=0.4),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=12,colour="#444441"),
        axis.text =element_text(size=10,colour="#888780"),
        plot.margin=margin(12,16,12,12)
      )
  }, bg="white")

  # ── DAG ──────────────────────────────────────────────────────────────────────
  # Layout:  X (left)  Y (right)  C (bottom-centre)
  # Arrows:  X->Y  X->C  Y->C
  # Correlations shown on arrows; line width and colour intensity ∝ |r|
  # When selection is active, C gets a dashed pink ring (= conditioned)

  output$dag_plot <- renderPlot({
    df     <- sim_data()
    show   <- show_sel()
    df_act <- if (show) df[df$selected, ] else df

    r_xy <- if (nrow(df_act) > 1) cor(df_act$x, df_act$y) else 0
    r_xc <- cor(df$x, df$c_var)
    r_yc <- cor(df$y, df$c_var)

    # Node centres (in [0,1]^2)
    nx <- 0.18; ny <- 0.62
    yx <- 0.82; yy <- 0.62
    cx <- 0.50; cy <- 0.18

    par(mar=c(0.2,0.2,0.2,0.2), bg="#ffffff")
    plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=FALSE)

    # Arrows
    draw_arrow(nx, ny, yx, yy, r=0.085,
               lwd=corr_lwd(r_xy), col=corr_col(r_xy))
    draw_arrow(nx, ny, cx, cy, r=0.085,
               lwd=corr_lwd(r_xc), col=corr_col(r_xc))
    draw_arrow(yx, yy, cx, cy, r=0.085,
               lwd=corr_lwd(r_yc), col=corr_col(r_yc))

    # Correlation labels
    text((nx+yx)/2, ny+0.12,
         sprintf("r = %+.2f", r_xy), cex=1.05, col="#2c2c2a", font=2)
    text((nx+cx)/2 - 0.09, (ny+cy)/2 + 0.03,
         sprintf("r = %+.2f", r_xc), cex=0.95, col="#666360")
    text((yx+cx)/2 + 0.09, (yy+cy)/2 + 0.03,
         sprintf("r = %+.2f", r_yc), cex=0.95, col="#666360")

    # C conditioning ring
    if (show) {
      theta <- seq(0, 2*pi, length.out=120)
      polygon(cx + 0.108*cos(theta), cy + 0.108*sin(theta),
              col=NA, border="#d4537e", lwd=2, lty=2)
    }

    c_fill   <- if (show) "#fbeaf0" else "#f1efe8"
    c_bord   <- if (show) "#d4537e" else "#888780"
    c_txt    <- if (show) "#993556" else "#5f5e5a"

    draw_node(cx, cy, "C (collider)", fill=c_fill, border=c_bord, txt_col=c_txt, cex=1.0)
    draw_node(nx, ny, x_lab(), fill="#eeedfe", border="#534ab7", txt_col="#2c2c2a", cex=1.1)
    draw_node(yx, yy, y_lab(), fill="#eeedfe", border="#534ab7", txt_col="#2c2c2a", cex=1.1)

    # Footer note
    note <- if (show) "Dashed ring = conditioned by selection  \u2014  r shown in selected subset"
            else      "Full population  \u2014  toggle selection to condition on C"
    text(0.50, 0.96, note, cex=0.85, col="#888780", adj=c(0.5,0.5))

  }, bg="white")
}

shinyApp(ui=ui, server=server)
