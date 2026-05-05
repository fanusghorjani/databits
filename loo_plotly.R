# Layers of Oppression
# Publication-ready editorial visualization with three aligned panels

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(patchwork)
library(scales)
library(htmlwidgets)
library(htmltools)

# -----------------------------
# 1) Data setup
# -----------------------------
base_df <- data.frame(
  factor = c("Low income", "Traumatic event", "Female", "Rural", "Smoking"),
  share = c(81.0, 56.7, 54.3, 33.4, 4.0),
  anxiety = c(75.7, 79.4, 77.6, 78.7, 77.6),
  depression = c(76.1, 80.1, 78.4, 79.3, 79.1),
  stringsAsFactors = FALSE
)

# Order factors by population share for readability
base_df <- base_df %>%
  mutate(factor = factor(factor, levels = rev(c("Low income", "Traumatic event", "Female", "Rural", "Smoking"))))

# Tidy long dataset for potential reuse/inspection
long_df <- base_df %>%
  pivot_longer(cols = c(share, anxiety, depression), names_to = "metric", values_to = "value")

# -----------------------------
# 2) Shared style
# -----------------------------
bg_col <- "#F7F3EE"
text_col <- "#172A3A"
share_col <- "#C9A66B"
anx_col <- "#3C7A89"
dep_col <- "#B85C38"

base_theme <- theme_minimal(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = alpha(text_col, 0.08), linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16, color = text_col),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 18, face = "bold", color = text_col, margin = margin(b = 10)),
    plot.margin = margin(8, 8, 8, 8)
  )

# -----------------------------
# 3) Build three aligned horizontal panels
# -----------------------------
plot_share <- ggplot(base_df, aes(x = share, y = factor)) +
  geom_col(fill = share_col, width = 0.64) +
  geom_text(aes(label = percent(share / 100, accuracy = 0.1)),
            hjust = -0.1, size = 4.7, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Who is exposed?") +
  base_theme

plot_anxiety <- ggplot(base_df, aes(x = anxiety, y = factor)) +
  geom_col(fill = anx_col, width = 0.64) +
  geom_text(aes(label = percent(anxiety / 100, accuracy = 0.1)),
            hjust = -0.1, size = 4.7, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Anxiety symptoms") +
  base_theme +
  theme(axis.text.y = element_blank())

plot_depression <- ggplot(base_df, aes(x = depression, y = factor)) +
  geom_col(fill = dep_col, width = 0.64) +
  geom_text(aes(label = percent(depression / 100, accuracy = 0.1)),
            hjust = -0.1, size = 4.7, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Depression symptoms") +
  base_theme +
  theme(axis.text.y = element_blank())

# Static combined editorial preview (optional reference object)
static_editorial <- plot_share + plot_anxiety + plot_depression +
  plot_layout(ncol = 3, widths = c(1, 1, 1))

# -----------------------------
# 4) Interactive plotly via ggplotly + subplot
# -----------------------------
hover_txt <- base_df %>%
  mutate(
    hover = paste0(
      "<b>", factor, "</b>",
      "<br>% of population: ", percent(share / 100, accuracy = 0.1),
      "<br>% anxiety (within group): ", percent(anxiety / 100, accuracy = 0.1),
      "<br>% depression (within group): ", percent(depression / 100, accuracy = 0.1),
      "<br><i>Values represent prevalence within each group, not causal effects.</i>"
    )
  )

p1 <- ggplot(base_df, aes(x = share, y = factor, text = hover_txt$hover)) +
  geom_col(fill = share_col, width = 0.64) +
  geom_text(aes(label = percent(share / 100, accuracy = 0.1)), hjust = -0.1, size = 4.7, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Who is exposed?") +
  base_theme

p2 <- ggplot(base_df, aes(x = anxiety, y = factor, text = hover_txt$hover)) +
  geom_col(fill = anx_col, width = 0.64) +
  geom_text(aes(label = percent(anxiety / 100, accuracy = 0.1)), hjust = -0.1, size = 4.7, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Anxiety symptoms") +
  base_theme + theme(axis.text.y = element_blank())

p3 <- ggplot(base_df, aes(x = depression, y = factor, text = hover_txt$hover)) +
  geom_col(fill = dep_col, width = 0.64) +
  geom_text(aes(label = percent(depression / 100, accuracy = 0.1)), hjust = -0.1, size = 4.7, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Depression symptoms") +
  base_theme + theme(axis.text.y = element_blank())

pp1 <- ggplotly(p1, tooltip = "text") %>%
  style(hovertemplate = "%{text}<extra></extra>")
pp2 <- ggplotly(p2, tooltip = "text") %>%
  style(hovertemplate = "%{text}<extra></extra>")
pp3 <- ggplotly(p3, tooltip = "text") %>%
  style(hovertemplate = "%{text}<extra></extra>")

interactive_plot <- subplot(pp1, pp2, pp3, nrows = 1, shareY = TRUE, titleX = FALSE, margin = 0.015) %>%
  layout(
    paper_bgcolor = bg_col,
    plot_bgcolor = bg_col,
    showlegend = FALSE,
    font = list(family = "Inter, Segoe UI, Arial, sans-serif", color = text_col, size = 15),
    title = list(
      text = paste0(
        "<span style='font-size:44px;font-weight:800;'>Layers of Oppression</span>",
        "<br><span style='font-size:19px;'>Who is exposed — and how mental health burden differs across groups</span>"
      ),
      x = 0.01,
      xanchor = "left"
    ),
    annotations = list(
      list(
        x = 0.01, y = -0.14,
        xref = "paper", yref = "paper",
        text = "Percentages show share within each group experiencing symptoms. Not causal.",
        showarrow = FALSE,
        xanchor = "left",
        font = list(size = 12, color = alpha(text_col, 0.8))
      )
    ),
    margin = list(l = 160, r = 30, t = 130, b = 95)
  ) %>%
  layout(
    xaxis = list(range = c(0, 100), visible = FALSE),
    xaxis2 = list(range = c(0, 100), visible = FALSE),
    xaxis3 = list(range = c(0, 100), visible = FALSE)
  )

# -----------------------------
# 5) Centered wrapper and save as HTML
# -----------------------------
page <- browsable(tagList(
  tags$style(HTML("body{background:#F7F3EE;margin:0;font-family:\"Inter\",\"Segoe UI\",Arial,sans-serif;} .wrap{max-width:1100px;margin:0 auto;padding:24px 24px 30px 24px;}")),
  div(class="wrap", interactive_plot)
))

save_html(page, file = "layers_of_oppression.html")
