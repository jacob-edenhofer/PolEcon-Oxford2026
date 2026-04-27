
###########################
#  Custom ggplot theme
###########################

# ── Colour palette ────────────────────────────────────────────────────────────
# A curated qualitative palette (colour-blind-safe, print-friendly)
pal_progressivity <- c(
  "#2A6F97",   # deep teal-blue

  "#E07A5F",   # warm terracotta
  "#81B29A",   # sage green
  "#F2CC8F",   # muted gold
  "#3D405B",   # charcoal slate
  "#9B2335",   # crimson
  "#6A994E",   # olive
  "#BC6C25"    # burnt sienna
)

# Convenience wrappers for ggplot scales
scale_colour_progressivity <- function(...) {
  ggplot2::scale_colour_manual(values = pal_progressivity, ...)
}
scale_fill_progressivity <- function(...) {
  ggplot2::scale_fill_manual(values = pal_progressivity, ...)
}

# Sequential / diverging helpers (for continuous measures)
scale_fill_progressivity_c <- function(low = "#2A6F97", high = "#E07A5F", ...) {
  ggplot2::scale_fill_gradient(low = low, high = high, ...)
}
scale_colour_progressivity_c <- function(low = "#2A6F97", high = "#E07A5F", ...) {
  ggplot2::scale_colour_gradient(low = low, high = high, ...)
}
scale_fill_progressivity_div <- function(low = "#2A6F97", mid = "#F5F5F5",
                                         high = "#E07A5F", ...) {
  ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high, ...)
}

# ── Centralised size control ──────────────────────────────────────────────────
theme_sizes <- list(
  title        = 16,
  subtitle     = 12.5,

  axis_title   = 12,
  axis_text    = 10,
  legend_title = 10.5,
  legend_text  = 9.5,
  strip_text   = 10,
  caption_text = 8.5
)

# ── Custom ggplot theme function ──────────────────────────────────────────────
custom_theme_scale <- function(font_family = "sans", scale = 1,
                               grid_y = TRUE, grid_x = FALSE) {
  s <- lapply(theme_sizes, function(x) x * scale)

  base <- theme_minimal(base_family = font_family, base_size = 11 * scale) +
    theme(
      # Title block
      plot.title = element_text(
        hjust = 0, size = s$title, face = "bold",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        hjust = 0, size = s$subtitle, colour = "grey35",
        margin = margin(b = 8)
      ),
      plot.title.position = "plot",

      # Axes
      axis.title   = element_text(size = s$axis_title, colour = "grey25"),
      axis.title.y = element_text(margin = margin(r = 6)),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.text    = element_text(size = s$axis_text, colour = "grey40"),
      axis.text.x  = element_text(angle = 0, vjust = 0.5),
      axis.line    = element_line(colour = "grey70", linewidth = 0.3),
      axis.ticks   = element_line(colour = "grey70", linewidth = 0.25),
      axis.ticks.length = unit(3, "pt"),

      # Facet strips
      strip.text = element_text(
        size = s$strip_text, face = "bold", colour = "grey20",
        margin = margin(b = 4, t = 4)
      ),
      strip.background = element_rect(fill = "grey96", colour = NA),

      # Legend
      legend.title    = element_text(size = s$legend_title, face = "bold"),
      legend.text     = element_text(size = s$legend_text),
      legend.position = "bottom",
      legend.key.height = unit(0.45, "cm"),
      legend.key.width  = unit(0.8, "cm"),
      legend.margin     = margin(t = 2),

      # Caption
      plot.caption = element_text(
        size = s$caption_text, colour = "grey50",
        hjust = 0, lineheight = 1.15,
        margin = margin(t = 8)
      ),
      plot.caption.position = "plot",

      # Panel grid
      panel.grid.major.y = if (grid_y) element_line(colour = "grey92", linewidth = 0.4) else element_blank(),
      panel.grid.major.x = if (grid_x) element_line(colour = "grey92", linewidth = 0.4) else element_blank(),
      panel.grid.minor   = element_blank(),

      # Margins
      plot.margin = margin(14, 14, 10, 14),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )

  base
}
