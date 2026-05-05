# Layers of Oppression
# Editorial Plotly panel graphic with external HTML wrapper

library(ggplot2)
library(plotly)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# -----------------------------
# 1) Data
# -----------------------------
chart_df <- data.frame(
  layer = c("Low income", "Traumatic event", "Female", "Rural", "Smoking"),
  share = c(81.0, 56.7, 54.3, 33.4, 4.0),
  anxiety = c(75.7, 79.4, 77.6, 78.7, 77.6),
  depression = c(76.1, 80.1, 78.4, 79.3, 79.1),
  stringsAsFactors = FALSE
)

layer_levels <- rev(c("Low income", "Traumatic event", "Female", "Rural", "Smoking"))
chart_df$layer <- factor(chart_df$layer, levels = layer_levels)

# -----------------------------
# 2) Style constants
# -----------------------------
bg <- "#F7F3EE"
text_col <- "#172A3A"
muted_col <- "#5F6B73"
col_exposure <- "#C9A66B"
col_anxiety <- "#3C7A89"
col_depression <- "#B85C38"
font_stack <- '"Inter", "Segoe UI", Arial, sans-serif'

base_theme <- theme_minimal(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 16, color = text_col),
    plot.title = element_text(face = "bold", size = 18, color = text_col, hjust = 0.5),
    plot.margin = margin(6, 4, 6, 4)
  )

label_df <- function(df, col_name, measure_name) {
  vals <- df[[col_name]]
  df %>% mutate(
    value = vals,
    label = sprintf("%.1f%%", vals),
    label_x = ifelse(vals <= 8, vals + 3.0, vals - 2.0),
    label_hjust = ifelse(vals <= 8, 0, 1),
    hover = paste0(
      "Layer: ", layer,
      "<br>Measure: ", measure_name,
      "<br>Value: ", sprintf("%.1f", vals), "%",
      "<br>Within-group prevalence, not causal."
    )
  )
}

exposure_df <- label_df(chart_df, "share", "Who is exposed?")
anxiety_df <- label_df(chart_df, "anxiety", "Anxiety symptoms")
depression_df <- label_df(chart_df, "depression", "Depression symptoms")

# -----------------------------
# 3) Panel ggplots (fixed 0-100)
# -----------------------------
make_panel <- function(df, color, title_txt, show_y = TRUE) {
  ggplot(df, aes(x = value, y = layer, text = hover)) +
    geom_col(fill = color, width = 0.56) +
    geom_text(
      aes(x = label_x, label = label, hjust = label_hjust),
      family = "Arial", fontface = "bold", size = 4.8, color = text_col
    ) +
    # very subtle references
    geom_vline(xintercept = c(0, 50, 100), color = "rgba(23,42,58,0.10)", linewidth = 0.25) +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = "off") +
    ggtitle(title_txt) +
    base_theme +
    theme(
      plot.title = element_text(size = 18, face = "bold", family = "Arial", color = text_col, hjust = 0.5),
      axis.text.y = if (show_y) element_text(size = 16, color = text_col) else element_blank()
    )
}

exposure_plot <- make_panel(exposure_df, col_exposure, "Who is exposed?", TRUE)
anxiety_plot <- make_panel(anxiety_df, col_anxiety, "Anxiety symptoms", FALSE)
depression_plot <- make_panel(depression_df, col_depression, "Depression symptoms", FALSE)

# -----------------------------
# 4) Plotly conversion + explicit subplot order
# -----------------------------
p_exposure <- ggplotly(exposure_plot, tooltip = "text")
p_anxiety <- ggplotly(anxiety_plot, tooltip = "text")
p_depression <- ggplotly(depression_plot, tooltip = "text")

plot_widget <- subplot(
  p_exposure,
  p_anxiety,
  p_depression,
  nrows = 1,
  shareY = TRUE,
  margin = 0.015
) %>%
  style(
    hovertemplate = "%{text}<extra></extra>",
    hoverlabel = list(bgcolor = "#172A3A", font = list(color = "white", size = 12, family = font_stack))
  ) %>%
  layout(
    paper_bgcolor = bg,
    plot_bgcolor = bg,
    font = list(family = font_stack, size = 14, color = text_col),
    margin = list(l = 170, r = 34, t = 14, b = 24),
    showlegend = FALSE,
    xaxis = list(range = c(0, 100), visible = FALSE),
    xaxis2 = list(range = c(0, 100), visible = FALSE),
    xaxis3 = list(range = c(0, 100), visible = FALSE),
    yaxis2 = list(showticklabels = FALSE),
    yaxis3 = list(showticklabels = FALSE),
    annotations = list(
      list(x = 0.334, y = 0.5, xref = "paper", yref = "paper", ax = 0, ay = -430,
           showarrow = TRUE, arrowhead = 0, arrowsize = 1, arrowwidth = 1, arrowcolor = "rgba(23,42,58,0.14)"),
      list(x = 0.667, y = 0.5, xref = "paper", yref = "paper", ax = 0, ay = -430,
           showarrow = TRUE, arrowhead = 0, arrowsize = 1, arrowwidth = 1, arrowcolor = "rgba(23,42,58,0.14)")
    )
  ) %>%
  config(displayModeBar = FALSE)

# -----------------------------
# 5) HTML wrapper outside plotly
# -----------------------------
page <- browsable(
  tagList(
    tags$style(HTML(paste0(
      "body {background:", bg, "; margin:0; font-family:", font_stack, "; color:", text_col, ";}",
      ".wrap {max-width:1280px; margin:0 auto; padding:40px 56px 40px 56px;}",
      ".kicker {font-size:13px; letter-spacing:0.04em; text-transform:uppercase; color:#4E5D68; margin-bottom:12px;}",
      ".title {font-size:46px; font-weight:800; line-height:1.05; margin:0 0 12px 0;}",
      ".subtitle {font-size:19px; line-height:1.35; margin:0 0 24px 0; max-width:1120px;}",
      ".plot-holder {height:590px;}",
      ".callout {background:#EFE4D6; border-left:5px solid #B85C38; padding:18px 22px; border-radius:10px; font-size:16px; margin-top:16px;}",
      ".fanon {font-size:15px; color:#425563; font-style:italic; margin-top:12px;}",
      ".note {font-size:12px; color:#5F6B73; margin-top:18px;}"
    ))),
    div(class = "wrap",
        div(class = "kicker", "Afghanistan · Neyazi et al. (2024) · n = 2,698"),
        h1(class = "title", "Layers of Oppression"),
        p(class = "subtitle", "In Afghanistan, anxiety and depression symptoms remain high across several social layers — suggesting a widespread mental health burden rather than an isolated risk group."),
        div(class = "plot-holder", plot_widget),
        div(class = "callout", HTML("<b>Key finding:</b> Mental health burden is widespread — across all five layers, around three in four respondents report anxiety or depression symptoms.")),
        div(class = "fanon", "The data shows the layers; Fanon helps explain the oppression behind them — psychological suffering as a product of structural violence, not individual weakness."),
        div(class = "note", "Source: Neyazi et al. (2024), Afghanistan mental health survey, n = 2,698. Percentages show prevalence within groups. Groups may overlap. Not causal.")
    )
  )
)

# -----------------------------
# 6) Export full wrapped HTML
# -----------------------------
save_html(page, file = "layers_of_oppression.html")