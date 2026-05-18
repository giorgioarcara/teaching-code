library(shiny)
library(ggplot2)

# ── DAG drawing helpers ───────────────────────────────────────────────────────

draw_arrow <- function(x0, y0, x1, y1, r = 0.085,
                       lwd = 1, col = "#0f6e56", arr_len = 0.10) {
  dx <- x1 - x0; dy <- y1 - y0; d <- sqrt(dx^2 + dy^2)
  ux <- dx / d;  uy <- dy / d
  lines(c(x0 + ux*r, x1 - ux*r), c(y0 + uy*r, y1 - uy*r),
        lwd = lwd, col = col, lend = 1)
  arrows(x0 + ux*r, y0 + uy*r, x1 - ux*r, y1 - uy*r,
         length = arr_len, angle = 20, lwd = lwd, col = col, code = 2)
}

draw_node <- function(cx, cy, label, r = 0.085,
                      fill = "#eaf3de", border = "#3b6d11",
                      txt_col = "#2c2c2a", cex = 0.82) {
  theta <- seq(0, 2*pi, length.out = 120)
  polygon(cx + r*cos(theta), cy + r*sin(theta),
          col = fill, border = border, lwd = 1.8)
  lbl <- if (nchar(label) > 13) paste0(substr(label, 1, 11), "\u2026") else label
  text(cx, cy, lbl, cex = cex, col = txt_col, adj = c(0.5, 0.5))
}

corr_lwd <- function(r) 0.8 + abs(r) * 5.5

corr_col <- function(r, pos_hex = "#0f6e56", neg_hex = "#d4537e") {
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
  title = "Collider Control Bias",

  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500&family=IBM+Plex+Mono&display=swap');
    body { font-family:'IBM Plex Sans',sans-serif; background:#f5f4f0; color:#2c2c2a; margin:0; }
    .app-header { background:#2c2c2a; color:#f5f4f0; padding:18px 32px 14px; }
    .app-header h1 { font-size:20px; font-weight:500; margin:0 0 4px; letter-spacing:-0.3px; }
    .app-header p  { font-size:13px; margin:0; color:#b4b2a9; }
    .main-layout   { display:flex; gap:0; min-height:calc(100vh - 70px); }
    .sidebar {
      width:300px; min-width:300px; background:#ffffff;
      border-right:1px solid #d3d1c7; padding:24px 20px;
      box-sizing:border-box; overflow-y:auto;
    }
    .main-panel { flex:1; padding:24px 28px; box-sizing:border-box; }
    .section-label {
      font-size:11px; font-weight:500; letter-spacing:0.08em;
      text-transform:uppercase; color:#888780; margin:0 0 12px;
    }
    .control-block { margin-bottom:24px; }
    .slider-label  { display:flex; justify-content:space-between; align-items:baseline; margin-bottom:6px; }
    .slider-label span { font-size:13px; color:#444441; }
    .slider-label .val { font-family:'IBM Plex Mono',monospace; font-size:13px; font-weight:500; color:#2c2c2a; }
    .stat-grid { display:grid; grid-template-columns:1fr 1fr 1fr; gap:10px; margin-bottom:20px; }
    .stat-card { background:#ffffff; border:1px solid #d3d1c7; border-radius:8px; padding:12px 14px; }
    .stat-card .label { font-size:11px; color:#888780; margin-bottom:4px; text-transform:uppercase; letter-spacing:0.06em; }
    .stat-card .value { font-size:22px; font-weight:500; font-family:'IBM Plex Mono',monospace; color:#2c2c2a; }
    .stat-card.warn .value { color:#ba7517; }
    .stat-card.bad  .value { color:#d4537e; }
    .toggle-btn {
      width:100%; padding:9px 0; background:#0f6e56; color:#f5f4f0; border:none;
      border-radius:6px; font-family:'IBM Plex Sans',sans-serif; font-size:13px;
      font-weight:500; cursor:pointer; margin-bottom:20px; transition:background 0.15s;
    }
    .toggle-btn:hover { background:#085041; }
    .toggle-btn.off { background:transparent; color:#444441; border:1px solid #d3d1c7; }
    .toggle-btn.off:hover { background:#f1efe8; }
    .legend-row  { display:flex; flex-direction:column; gap:6px; font-size:12px; color:#888780; margin-bottom:16px; }
    .legend-item { display:flex; align-items:center; gap:8px; }
    .legend-line { width:22px; height:3px; display:inline-block; border-radius:2px; }
    .legend-line.dashed {
      background:repeating-linear-gradient(to right,#888780 0,#888780 5px,transparent 5px,transparent 9px);
    }
    .info-box {
      background:#eaf3de; border-left:3px solid #3b6d11; border-radius:0 6px 6px 0;
      padding:12px 14px; font-size:12px; color:#27500a; line-height:1.65; margin-top:8px;
    }
    .info-box strong { font-weight:500; }
    .label-inputs { display:grid; grid-template-columns:1fr 1fr; gap:10px; margin-bottom:24px; }
    .label-inputs label { font-size:11px; color:#888780; display:block; margin-bottom:4px; text-transform:uppercase; letter-spacing:0.06em; }
    .label-inputs input[type=text] {
      width:100%; padding:6px 8px; font-size:13px; font-family:'IBM Plex Sans',sans-serif;
      border:1px solid #d3d1c7; border-radius:5px; background:#f5f4f0; color:#2c2c2a;
      box-sizing:border-box; outline:none; transition:border-color 0.15s;
    }
    .label-inputs input[type=text]:focus { border-color:#3b6d11; background:#ffffff; }
    .plot-area { background:#ffffff; border:1px solid #d3d1c7; border-radius:10px; padding:8px 4px 4px; }
    .dag-area  { background:#ffffff; border:1px solid #d3d1c7; border-radius:10px; padding:0 4px 4px; margin-top:16px; }
    .dag-title { font-size:11px; font-weight:500; letter-spacing:0.07em; text-transform:uppercase; color:#888780; padding:10px 12px 0; }
    hr.divider { border:none; border-top:1px solid #d3d1c7; margin:20px 0; }
  "))),

  div(class = "app-header",
    h1("Collider Control Bias"),
    p("Adjusting for a collider covariate opens a backdoor path \u2014 inducing spurious X\u2013Y associations")
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

      tags$hr(class="divider"),
      div(class = "section-label", "Causal structure"),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("True X \u2192 Y effect (\u03b2)"),
          tags$span(class="val", textOutput("slope_val", inline=TRUE))
        ),
        sliderInput("slope", NULL, min=-1, max=1, value=0, step=0.05, width="100%")
      ),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("X \u2192 C path (\u03b31)"),
          tags$span(class="val", textOutput("gam1_val", inline=TRUE))
        ),
        sliderInput("gam1", NULL, min=0, max=1, value=0.7, step=0.05, width="100%")
      ),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("Y \u2192 C path (\u03b32)"),
          tags$span(class="val", textOutput("gam2_val", inline=TRUE))
        ),
        sliderInput("gam2", NULL, min=0, max=1, value=0.7, step=0.05, width="100%")
      ),

      div(class = "control-block",
        div(class = "slider-label",
          tags$span("Sample size (N)"),
          tags$span(class="val", textOutput("n_val", inline=TRUE))
        ),
        sliderInput("n_pts", NULL, min=200, max=1200, value=600, step=100, width="100%")
      ),

      tags$hr(class="divider"),

      actionButton("toggle_adj", "Control for C (adjusted)", class="toggle-btn", width="100%"),

      div(class = "legend-row",
        div(class="legend-item",
          tags$span(class="legend-line dashed"), "Unadjusted X\u2013Y trend"),
        div(class="legend-item",
          tags$span(class="legend-line", style="background:#0f6e56;"), "Adjusted trend (controlling C)")
      ),

      div(class = "info-box",
        tags$strong("How it works:"), tags$br(),
        "C is caused by both X and Y (a collider). Adding C to a regression",
        " of Y on X opens the path X \u2192 C \u2190 Y,",
        " creating a spurious partial association between X and Y",
        tags$em(" even when the true causal effect is zero.")
      )
    ),

    div(class = "main-panel",

      uiOutput("stat_cards"),

      div(class = "plot-area",
        plotOutput("scatter", height = "400px")
      ),

      div(class = "dag-area",
        div(class = "dag-title", "Sample DAG \u2014 arrow width \u221d |r|"),
        plotOutput("dag_plot", height = "200px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  show_adj <- reactiveVal(FALSE)

  observeEvent(input$toggle_adj, {
    show_adj(!show_adj())
    updateActionButton(session, "toggle_adj",
      label = if (show_adj()) "Hide adjusted trend" else "Control for C (adjusted)")
  })

  output$slope_val <- renderText(sprintf("%+.2f", input$slope))
  output$gam1_val  <- renderText(sprintf("%.2f",  input$gam1))
  output$gam2_val  <- renderText(sprintf("%.2f",  input$gam2))
  output$n_val     <- renderText(input$n_pts)

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
    n <- input$n_pts; slope <- input$slope
    gam1 <- input$gam1; gam2 <- input$gam2
    x   <- rnorm(n)
    y   <- slope * x + sqrt(max(1 - slope^2, 1e-6)) * rnorm(n)
    c_v <- scale(gam1 * x + gam2 * y + 0.3 * rnorm(n))[, 1]
    data.frame(x=x, y=y, c=c_v)
  })

  regs <- reactive({
    df         <- sim_data()
    lm_crude   <- lm(y ~ x,     data=df)
    lm_adj     <- lm(y ~ x + c, data=df)
    res_x      <- residuals(lm(x ~ c, data=df))
    res_y      <- residuals(lm(y ~ c, data=df))
    list(crude_coef=coef(lm_crude)["x"], adj_coef=coef(lm_adj)["x"],
         res_x=res_x, res_y=res_y, df=df)
  })

  output$stat_cards <- renderUI({
    r   <- regs(); adj <- show_adj()
    delta    <- abs(r$adj_coef - r$crude_coef)
    adj_cls  <- if (adj && delta > 0.05) "stat-card bad"  else "stat-card"

    div(class="stat-grid",
      div(class="stat-card",
        div(class="label","True \u03b2"),        div(class="value", sprintf("%+.2f", input$slope))),
      div(class="stat-card",
        div(class="label","Crude \u03b2 (X only)"), div(class="value", sprintf("%+.3f", r$crude_coef))),
      div(class=adj_cls,
        div(class="label","Adjusted \u03b2 (+C)"),  div(class="value", if (adj) sprintf("%+.3f", r$adj_coef) else "\u2014"))
    )
  })

  output$scatter <- renderPlot({
    r <- regs(); adj <- show_adj(); df <- r$df
    xl <- paste0(x_lab(), " (exposure)")
    yl <- paste0(y_lab(), " (outcome)")

    lm_crude <- lm(y ~ x, data=df)
    xr <- range(df$x)
    tc <- data.frame(x=xr, y=coef(lm_crude)[1] + coef(lm_crude)[2]*xr)

    if (!adj) {
      p <- ggplot(df, aes(x=x, y=y)) +
        geom_point(colour="#888780", alpha=0.22, size=1.7, shape=16) +
        geom_line(data=tc, aes(x=x,y=y), colour="#888780", linewidth=1.2, linetype="dashed") +
        coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5)) +
        labs(x=xl, y=yl, title="Unadjusted: Y ~ X")
    } else {
      fwl    <- data.frame(rx=r$res_x, ry=r$res_y)
      lm_fwl <- lm(ry ~ rx, data=fwl)
      rxr    <- range(fwl$rx)
      tf     <- data.frame(rx=rxr, ry=coef(lm_fwl)[1] + coef(lm_fwl)[2]*rxr)
      tc_fwl <- data.frame(rx=rxr, ry=coef(lm_crude)[2]*rxr)

      p <- ggplot(fwl, aes(x=rx, y=ry)) +
        geom_point(colour="#1d9e75", alpha=0.30, size=1.7, shape=16) +
        geom_line(data=tc_fwl, aes(x=rx,y=ry), colour="#888780", linewidth=1, linetype="dashed") +
        geom_line(data=tf, aes(x=rx,y=ry), colour="#0f6e56", linewidth=1.8) +
        labs(x=paste0(x_lab()," | C (residual)"),
             y=paste0(y_lab()," | C (residual)"),
             title="Adjusted: Y ~ X + C \u2014 partial regression (FWL)")
    }

    p + theme_minimal(base_family="sans") +
      theme(
        panel.background=element_rect(fill="#ffffff",colour=NA),
        plot.background =element_rect(fill="#ffffff",colour=NA),
        panel.grid.major=element_line(colour="#d3d1c7",linewidth=0.4),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=12,colour="#444441"),
        axis.text =element_text(size=10,colour="#888780"),
        plot.title=element_text(size=13,colour="#2c2c2a",face="plain",margin=margin(b=8)),
        plot.margin=margin(12,16,12,12)
      )
  }, bg="white")

  # ── DAG ──────────────────────────────────────────────────────────────────────
  # Structure:  X -> Y  (true causal, may be 0)
  #             X -> C  (X causes collider)
  #             Y -> C  (Y causes collider)
  # When adjusted: C gets a box-in-box double border (controlled for)
  # Correlations: r(X,Y), r(X,C), r(Y,C) from full sample
  # When adjusted also show partial r (FWL) on X-Y path

  output$dag_plot <- renderPlot({
    df  <- sim_data()
    adj <- show_adj()
    r   <- regs()

    r_xy    <- cor(df$x, df$y)
    r_xc    <- cor(df$x, df$c)
    r_yc    <- cor(df$y, df$c)
    r_xy_pf <- if (adj) cor(r$res_x, r$res_y) else NA  # partial after FWL

    nx <- 0.18; ny <- 0.62
    yx <- 0.82; yy <- 0.62
    cx <- 0.50; cy <- 0.18

    par(mar=c(0.2,0.2,0.2,0.2), bg="#ffffff")
    plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=FALSE)

    # X->Y arrow — use partial r colour when adjusted
    xy_r_disp <- if (adj && !is.na(r_xy_pf)) r_xy_pf else r_xy
    draw_arrow(nx, ny, yx, yy, r=0.085,
               lwd=corr_lwd(xy_r_disp), col=corr_col(xy_r_disp))

    draw_arrow(nx, ny, cx, cy, r=0.085,
               lwd=corr_lwd(r_xc), col=corr_col(r_xc))
    draw_arrow(yx, yy, cx, cy, r=0.085,
               lwd=corr_lwd(r_yc), col=corr_col(r_yc))

    # Labels on arrows
    xy_label <- if (adj && !is.na(r_xy_pf))
      sprintf("partial r = %+.2f", r_xy_pf)
    else
      sprintf("r = %+.2f", r_xy)
    text((nx+yx)/2, ny+0.12, xy_label, cex=0.74, col="#2c2c2a", font=2)
    text((nx+cx)/2 - 0.09, (ny+cy)/2 + 0.03,
         sprintf("r = %+.2f", r_xc), cex=0.68, col="#666360")
    text((yx+cx)/2 + 0.09, (yy+cy)/2 + 0.03,
         sprintf("r = %+.2f", r_yc), cex=0.68, col="#666360")

    # C node: double ring when controlled for (adjusted)
    if (adj) {
      theta <- seq(0, 2*pi, length.out=120)
      # outer box-like square to signal conditioning
      polygon(cx + 0.115*cos(theta), cy + 0.115*sin(theta),
              col=NA, border="#0f6e56", lwd=2.5)
      polygon(cx + 0.122*cos(theta), cy + 0.122*sin(theta),
              col=NA, border="#0f6e56", lwd=1, lty=3)
    }

    c_fill <- if (adj) "#e1f5ee" else "#f1efe8"
    c_bord <- if (adj) "#0f6e56" else "#888780"
    c_txt  <- if (adj) "#085041" else "#5f5e5a"
    draw_node(cx, cy, "C (collider)", fill=c_fill, border=c_bord, txt_col=c_txt, cex=0.75)

    draw_node(nx, ny, x_lab(), fill="#eaf3de", border="#3b6d11", txt_col="#2c2c2a")
    draw_node(yx, yy, y_lab(), fill="#eaf3de", border="#3b6d11", txt_col="#2c2c2a")

    note <- if (adj) "Double ring = controlled for (adjusted)  \u2014  X\u2013Y shows partial correlation"
            else     "Unadjusted  \u2014  toggle adjustment to condition on C"
    text(0.50, 0.96, note, cex=0.62, col="#888780", adj=c(0.5,0.5))

  }, bg="white")
}

shinyApp(ui=ui, server=server)
