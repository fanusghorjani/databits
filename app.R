# Layers of Oppression - narrative flow/network visualization
# -----------------------------------------------------------
# Interactive Shiny app for public-facing data journalism
# Source framing: Neyazi et al. (2024), Fanon (theoretical context)
# Method note: This is a relative burden index from combined AOR associations,
# not a causal estimate and not a predicted probability.

library(shiny)
library(plotly)
library(dplyr)
library(htmlwidgets)

# -----------------------------
# 1) Data setup
# -----------------------------
factors <- data.frame(
  id = c("female", "low_income", "rural", "smoking", "trauma"),
  label = c("Female", "Low income", "Rural area", "Current smoking", "Traumatic event"),
  category = c("social", "social", "social", "behavioral", "experience"),
  aor = c(1.716, 2.080, 1.262, 2.496, 1 / 0.412),
  x = c(0.44, 0.52, 0.60, 0.52, 0.44),
  y = c(0.76, 0.62, 0.50, 0.38, 0.24),
  explanation = c(
    "Women in the study showed higher adjusted odds of anxiety than men.",
    "Low income was associated with higher adjusted odds of anxiety.",
    "Rural residence was associated with higher adjusted odds of anxiety.",
    "Smoking refers to cigarette smoking. It may reflect coping or correlated hardship, not a direct cause.",
    "Approximation based on the inverse of the reported 'No bad event' AOR. This layer represents recent adverse experiences."
  ),
  stringsAsFactors = FALSE
)

prevalence <- data.frame(
  metric = c("Depression", "Anxiety", "Stress"),
  value = c(72.05, 71.94, 66.49),
  stringsAsFactors = FALSE
)

# base marker sizes from AOR strength
factors <- factors %>%
  mutate(size = 30 + (aor - min(aor)) / (max(aor) - min(aor)) * 24)

# -----------------------------
# 2) UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body {background: #fcfbf8; font-family: 'Inter', 'Arial', sans-serif; color: #1f2933;}\n      .wrap {max-width: 1150px; margin: 0 auto;}\n      .title {font-size: 44px; margin-bottom: 6px; letter-spacing: -0.4px;}\n      .subtitle {font-size: 20px; color: #486581; margin-top: 0; margin-bottom: 18px;}\n      .panel {background: #fff; border: 1px solid #e4e7eb; border-radius: 14px; padding: 16px 18px; box-shadow: 0 6px 18px rgba(16,42,67,0.05);}\n      .control-text {font-size: 15px; color: #334e68; margin-bottom: 10px;}\n      .result-badge {border-radius: 12px; padding: 16px; text-align: center; color: #fff; margin-bottom: 12px;}\n      .level-title {font-size: 30px; font-weight: 800; letter-spacing: 0.7px;}\n      .level-sub {font-size: 22px; margin-top: 4px;}\n      .small-card-row {display: flex; gap: 12px; margin-top: 12px; margin-bottom: 10px;}\n      .small-card {flex: 1; background: #fff; border: 1px solid #d9e2ec; border-radius: 12px; padding: 12px; text-align: center;}\n      .small-card .v {font-size: 28px; font-weight: 800; color: #243b53;}\n      .small-card .l {font-size: 14px; color: #627d98;}\n      .method-note {font-size: 14px; color: #334e68; background: #f7f9fb; border-left: 4px solid #9fb3c8; padding: 10px 12px; border-radius: 8px;}\n      .plot-note {font-size: 13px; color: #627d98; margin-top: 8px;}\n    "))
  ),
  
  div(class = "wrap",
      h1("Layers of Oppression", class = "title"),
      p("How overlapping disadvantages are associated with higher mental health burden", class = "subtitle"),
      
      fluidRow(
        column(
          width = 4,
          div(class = "panel",
              h4("Choose overlapping disadvantages"),
              p("Select one or more layers. The burden index updates as factors overlap.", class = "control-text"),
              checkboxGroupInput(
                inputId = "selected_layers",
                label = NULL,
                choices = setNames(factors$id, factors$label),
                selected = character(0)
              ),
              uiOutput("risk_summary")
          )
        ),
        column(
          width = 8,
          div(class = "panel",
              plotlyOutput("network_plot", height = "560px"),
              p("Mental health risks can build up when several disadvantages overlap. This tool shows relative burden, not exact probability.", class = "plot-note")
          )
        )
      ),
      
      div(class = "small-card-row",
          uiOutput("prev_dep"),
          uiOutput("prev_anx"),
          uiOutput("prev_str")
      ),
      
      div(class = "method-note",
          "Index based on adjusted odds ratios from Neyazi et al. (2024). It shows relative association, not causation or predicted probability.")
  )
)

# -----------------------------
# 3) Server logic
# -----------------------------
server <- function(input, output, session) {
  
  burden_info <- reactive({
    selected <- factors %>% filter(id %in% input$selected_layers)
    idx <- if (nrow(selected) == 0) 1 else prod(selected$aor)
    
    level <- case_when(
      idx < 2 ~ "LOW",
      idx < 4 ~ "ELEVATED",
      idx < 8 ~ "HIGH",
      idx < 16 ~ "VERY HIGH",
      TRUE ~ "EXTREME"
    )
    
    col <- case_when(
      idx < 2 ~ "#2e7d32",
      idx < 4 ~ "#f0ad1d",
      idx < 8 ~ "#ef7d1a",
      idx < 16 ~ "#dd5a20",
      TRUE ~ "#b83232"
    )
    
    list(selected = selected, index = idx, level = level, color = col)
  })
  
  output$risk_summary <- renderUI({
    b <- burden_info()
    about_times <- round(b$index)
    
    div(
      div(class = "result-badge", style = paste0("background:", b$color, ";"),
          div(class = "level-title", b$level),
          div(class = "level-sub", paste0("≈ ", about_times, "x baseline"))
      ),
      p(style = "margin-top: 0; color: #334e68; font-size: 15px;",
        paste0("About ", about_times, " times higher relative burden than baseline."))
    )
  })
  
  output$network_plot <- renderPlotly({
    b <- burden_info()
    active_ids <- b$selected$id
    
    plot_ly()
    
    p <- plot_ly()
    
    # Node coordinates for baseline and burden
    baseline <- data.frame(x = 0.12, y = 0.50, label = "Baseline", txt = "Baseline: 1x relative burden")
    burden <- data.frame(
      x = 0.87,
      y = 0.50,
      label = "Mental health burden",
      txt = paste0("Current burden: ≈ ", round(b$index, 1), "x higher relative burden")
    )
    
    # Faint guide lines from baseline to each factor and to burden
    for (i in seq_len(nrow(factors))) {
      is_active <- factors$id[i] %in% active_ids
      line_col <- if (is_active) "rgba(197, 90, 17, 0.55)" else "rgba(180, 190, 200, 0.20)"
      line_w <- if (is_active) 3 else 1
      
      p <- p %>%
        add_segments(
          x = baseline$x, y = baseline$y,
          xend = factors$x[i], yend = factors$y[i],
          inherit = FALSE,
          line = list(color = line_col, width = line_w),
          hoverinfo = "skip",
          showlegend = FALSE
        ) %>%
        add_segments(
          x = factors$x[i], y = factors$y[i],
          xend = burden$x, yend = burden$y,
          inherit = FALSE,
          line = list(color = line_col, width = line_w),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
    
    # Baseline node
    p <- p %>% add_trace(
      data = baseline,
      x = ~x, y = ~y,
      type = "scatter", mode = "markers+text",
      marker = list(size = 78, color = "#dbe7f2", line = list(color = "#7b93a7", width = 2)),
      text = ~paste0("<b>", label, "</b><br>1x"),
      textposition = "middle center",
      textfont = list(size = 14, color = "#102a43"),
      hovertemplate = "<b>Baseline</b><br>Relative burden index: 1x<extra></extra>",
      showlegend = FALSE
    )
    
    # Factor nodes
    factor_df <- factors %>%
      mutate(
        active = id %in% active_ids,
        fill = case_when(
          category == "social" & active ~ "#6f8ea8",
          category == "social" & !active ~ "#cad7e2",
          category != "social" & active ~ "#a67c52",
          TRUE ~ "#e3d7c8"
        ),
        border = ifelse(active, "#3e4c59", "#b0bcc8"),
        opacity = ifelse(active, 1, 0.6),
        htxt = paste0(
          "<b>", label, "</b>",
          "<br>Adjusted odds ratio (AOR): ", round(aor, 3),
          "<br>", explanation,
          "<br><i>This is a simplified index based on combined associations, not a causal estimate.</i>"
        )
      )
    
    p <- p %>% add_trace(
      data = factor_df,
      x = ~x, y = ~y,
      type = "scatter", mode = "markers+text",
      marker = list(
        size = ~size,
        color = ~fill,
        opacity = ~opacity,
        line = list(color = ~border, width = 2)
      ),
      text = ~label,
      textposition = "middle center",
      textfont = list(size = 12, color = "#102a43"),
      hovertemplate = ~paste0(htxt, "<extra></extra>"),
      showlegend = FALSE
    )
    
    # Burden node
    burden_size <- min(115, 76 + round(log(b$index + 1) * 11, 0))
    p <- p %>% add_trace(
      data = burden,
      x = ~x, y = ~y,
      type = "scatter", mode = "markers+text",
      marker = list(size = burden_size, color = b$color, line = list(color = "#7a1f1f", width = 2.2)),
      text = paste0("<b>Mental health\nburden</b><br>≈ ", round(b$index, 1), "x"),
      textposition = "middle center",
      textfont = list(size = 14, color = "white"),
      hovertemplate = paste0(
        "<b>Mental health burden</b><br>",
        "Current relative burden index: ≈ ", round(b$index, 2), "x",
        "<br>Interpretive index from combined AOR associations.",
        "<extra></extra>"
      ),
      showlegend = FALSE
    )
    
    p %>% layout(
      xaxis = list(visible = FALSE, range = c(0, 1)),
      yaxis = list(visible = FALSE, range = c(0.08, 0.92)),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff",
      margin = list(l = 10, r = 10, t = 10, b = 10),
      annotations = list(
        list(x = 0.12, y = 0.88, xref = "x", yref = "y", text = "<b>Person / Baseline</b>", showarrow = FALSE, font = list(size = 13, color = "#627d98")),
        list(x = 0.52, y = 0.88, xref = "x", yref = "y", text = "<b>Layers</b>", showarrow = FALSE, font = list(size = 13, color = "#627d98")),
        list(x = 0.87, y = 0.88, xref = "x", yref = "y", text = "<b>Mental health burden</b>", showarrow = FALSE, font = list(size = 13, color = "#627d98"))
      )
    )
  })
  
  output$prev_dep <- renderUI({
    div(class = "small-card", div(class = "v", "72%"), div(class = "l", "Depression"))
  })
  output$prev_anx <- renderUI({
    div(class = "small-card", div(class = "v", "72%"), div(class = "l", "Anxiety"))
  })
  output$prev_str <- renderUI({
    div(class = "small-card", div(class = "v", "66%"), div(class = "l", "Stress"))
  })
}

shinyApp(ui = ui, server = server)
