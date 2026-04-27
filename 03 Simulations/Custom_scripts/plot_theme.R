
###########################
#  Custom ggplot theme
###########################

## Centralise size control for accessibility/scaling
## These are base sizes in pt for the default 9x6 inch plot at 300 DPI.
## showtext is configured to match (showtext_opts(dpi = 300)).
theme_sizes <- list(
  title      = 18,
  subtitle   = 12,
  axis_title = 13,
  axis_text  = 11,
  legend_title = 12,
  legend_text  = 11,
  strip_text   = 12,
  caption_text = 10
)
theme_scale_multiplier <- 1

enable_text_rendering <- function() {
  if (requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)) {
    showtext::showtext_opts(dpi = 300)
    ## Register Chivo from Google Fonts if not already available
    font_list <- tryCatch(sysfonts::font_families(), error = function(e) character())
    if (!("Chivo" %in% font_list)) {
      tryCatch(
        sysfonts::font_add_google("Chivo", "Chivo"),
        error = function(e) NULL
      )
    }
    showtext::showtext_auto(enable = TRUE)
  }
}

## Color palette (Okabe-Ito + muted accents)
custom_palette <- c(
  "#0072B2", # blue
  "#56B4E9", # sky
  "#009E73", # green
  "#E69F00", # orange
  "#D55E00", # red
  "#CC79A7", # purple
  "#F0E442", # yellow
  "#7F7F7F"  # grey
)

default_font_family <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE)) {
    font_list <- tryCatch(sysfonts::font_families(), error = function(e) character())
    if ("Chivo" %in% font_list) {
      return("Chivo")
    }
  }
  "sans"
}

legend_rows_for <- function(n_levels, threshold = 4) {
  if (is.null(n_levels) || !is.finite(n_levels)) return(1)
  if (n_levels > threshold) 2 else 1
}

wrap_plot_title <- function(title, width = 55) {
  if (is.null(title) || !nzchar(title)) return(title)
  width <- min(width, 80)
  stringr::str_wrap(title, width = width)
}

wrap_plot_subtitle <- function(subtitle, width = 65) {
  if (is.null(subtitle) || !nzchar(subtitle)) return(subtitle)
  width <- min(width, 90)
  stringr::str_wrap(subtitle, width = width)
}

## Caption width = approximate character count per line (tune by figure width in inches).
## Upper cap allows fewer, longer lines on wide ggsave() figures without clipping.
wrap_plot_caption <- function(caption, width = 88) {
  if (is.null(caption) || !nzchar(caption)) return(caption)
  width <- max(40L, as.integer(width))
  width <- min(width, 130L)
  stringr::str_wrap(as.character(caption), width = width)
}

wrap_axis_labels <- function(labels, width = 18) {
  if (is.null(labels)) return(labels)
  stringr::str_wrap(labels, width = width)
}

## Convenience wrapper for scale_*_discrete(labels = ...) tick labels
wrap_tick_labels <- function(width = 18) {
  function(x) stringr::str_wrap(x, width = width)
}

save_plot <- function(path, plot, width = 9, height = 6, dpi = 300) {
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi,
                  bg = "white")
}

## ── Human-readable variable labels ──────────────────────────────
## Used by pretty_label() to replace raw column names in titles,
## axis ticks, facet strips, and legends.
.var_labels <- c(
  # DVs
  dv_delta_lr_position        = "\u0394 L-R position",
  dv_delta_lr_position_abs    = "|\u0394 L-R position|",
  dv_delta_lr_position_last   = "\u0394 L-R (last obs.)",
  dv_delta_lr_position_2      = "\u0394 L-R (2-period)",
  dv_delta_lr_position_ma2    = "\u0394 L-R (MA-2)",
  dv_delta_lr_position_elec   = "\u0394 L-R (election lag)",
  dv_delta_lr_manifesto       = "\u0394 L-R Manifesto",
  dv_delta_lr_ches            = "\u0394 L-R CHES",
  delta_lr_position           = "\u0394 L-R position",
  lr_position                 = "L-R position",

  # IVs
  loyalty_smoothed            = "Loyalty (smoothed)",
  loyalty_index               = "Loyalty index",
  n_indicators                = "N loyalty indicators",
  misalign_median_abs         = "Misalignment (abs.)",
  misalign_median_abs_lag1    = "Misalignment (lag 1)",
  misalign_median_abs_lag1_elec = "Misalignment (elec. lag)",
  median_pull                 = "Median voter pull",
  abs_median_gap              = "Abs. median gap",
  competitiveness_closeness   = "Competitiveness",
  competitiveness_lpr         = "Loss probability (LPR)",

  # Organisation
  ppdb_member_count           = "Members (count)",
  ppdb_member_count_log       = "Members (log)",
  ppdb_dues_level             = "Dues level",
  ppdb_member_revenue         = "Member revenue",
  ppdb_member_revenue_log     = "Member revenue (log)",
  ppdb_spending               = "Spending",
  ppdb_spending_log           = "Spending (log)",
  ppdb_income_total_log       = "Income total (log)",
  ppdb_union_links            = "Union links",
  ppdb_corp_links             = "Corporate links",
  ppdb_ipd_aipd               = "IPD: AIPD",
  ppdb_ipd_pipd               = "IPD: PIPD",
  ppdb_ipd_opipd              = "IPD: OPIPD",
  ppdb_org_capacity_index     = "Org. capacity index",
  ppdb_org_capacity_n         = "Org. capacity (N items)",
  ppdb_org_capacity_core      = "Org. core capacity",
  ppdb_org_capacity_core_n    = "Org. core (N items)",
  ppdb_org_capacity_links     = "Org. links",
  ppdb_org_capacity_links_n   = "Org. links (N items)",
  ppdb_org_capacity_ipd       = "Org. IPD",
  ppdb_org_capacity_ipd_n     = "Org. IPD (N items)",

  # Controls
  union_density               = "Union density",
  union_coverage              = "Bargaining coverage",
  union_coverage_adj          = "Bargaining cov. (adj.)",
  vdem_populism               = "Populism (V-Dem)",
  vdem_antiplural             = "Anti-pluralism (V-Dem)",
  vdem_clientelism            = "Clientelism (V-Dem)",
  vdem_vote_share             = "Vote share (V-Dem)",
  vdem_seat_share             = "Seat share (V-Dem)",
  cp_score                    = "Cross-pressuredness",
  valence_econ                = "Valence: economy",
  valence_gov                 = "Valence: government",
  valence_dem                 = "Valence: democracy",
  valence_trust_party         = "Valence: party trust",
  ideological_core_share      = "Ideological core share",
  paged_seat_share            = "Seat share (PAGED)",
  effpar_ele_lag              = "Eff. parties (lag)",
  vturn_lag                   = "Turnout (lag)",
  gov_party_y3                = "Govt. party (3-yr)",
  ud_ipol_y3                  = "UD interpolated (3-yr)",
  openc_y3                    = "Openness (3-yr)",
  realgdpgr_y3                = "Real GDP growth (3-yr)",
  unemp_y3                    = "Unemployment (3-yr)",

  # Interaction / other
  loyalty_median_pull         = "Loyalty \u00d7 Median pull",
  loyalty_x_comp              = "Loyalty \u00d7 Competitiveness",
  loyalty_above               = "Loyalty (above l*)",
  loyalty_below               = "Loyalty (below l*)",
  loyalty_gain                = "Loyalty gain (recovery)",
  loyalty_loss                = "Loyalty loss (depletion)",
  delta_loyalty               = "\u0394 Loyalty",
  loyalty_x_delta_high        = "\u0394 Loyalty \u00d7 High level",
  loyalty_x_delta_low         = "\u0394 Loyalty \u00d7 Low level",
  lag_dv                      = "Lagged \u0394 L-R",
  d_lag_dv                    = "\u0394 Lagged \u0394 L-R",
  d_loyalty_smoothed          = "\u0394 Loyalty (smoothed)",
  d_misalign_median_abs_lag1_elec = "\u0394 Misalignment",
  d_median_pull               = "\u0394 Median voter pull",
  d_competitiveness_closeness = "\u0394 Competitiveness",
  d_realgdpgr_y3              = "\u0394 Real GDP growth",
  d_unemp_y3                  = "\u0394 Unemployment",
  d_effpar_ele_lag            = "\u0394 Eff. parties",
  d_vturn_lag                 = "\u0394 Turnout",

  # Model labels
  election_ordered            = "Election-ordered",
  imp_A                       = "Imputed A (interp.)",
  imp_C                       = "Imputed C (MICE)",
  imp_D                       = "Imputed D (partial pool.)",
  raw                         = "Raw"
)

pretty_label <- function(x, width = NULL) {
  out <- ifelse(x %in% names(.var_labels), .var_labels[x], gsub("_", " ", x))
  if (!is.null(width)) out <- stringr::str_wrap(out, width = width)
  out
}

pretty_labeller <- ggplot2::labeller(.default = function(x) pretty_label(x, width = 25))

normalize_caption_text <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  trimws(x)
}

drop_redundant_caption <- function(title, caption) {
  if (is.null(caption) || !nzchar(caption)) return(NULL)
  if (is.null(title) || !nzchar(title)) return(caption)
  norm_title <- normalize_caption_text(title)
  norm_caption <- normalize_caption_text(caption)
  if (!nzchar(norm_caption)) return(NULL)
  if (!nzchar(norm_title)) return(caption)
  if (identical(norm_caption, norm_title)) return(NULL)
  if (stringr::str_detect(norm_caption, stringr::fixed(norm_title))) return(NULL)
  if (stringr::str_detect(norm_title, stringr::fixed(norm_caption))) return(NULL)
  if (stringr::str_detect(norm_caption, "^outcome\\b")) {
    outcome_text <- trimws(stringr::str_replace(norm_caption, "^outcome\\b\\s*", ""))
    if (nzchar(outcome_text) && stringr::str_detect(norm_title, stringr::fixed(outcome_text))) {
      return(NULL)
    }
  }
  caption
}

facet_labeller_wrap <- ggplot2::labeller(.default = ggplot2::label_wrap_gen(width = 22))

scale_color_custom <- function(..., guide = NULL, n_levels = NULL) {
  dots <- list(...)
  if (!is.null(dots$guide)) {
    guide <- dots$guide
    dots$guide <- NULL
  }
  if (is.null(dots$labels)) {
    dots$labels <- function(x) stringr::str_wrap(x, 20)
  }
  if (is.null(guide) && !is.null(n_levels)) {
    guide <- guide_legend(nrow = legend_rows_for(n_levels), byrow = TRUE)
  }
  do.call(
    scale_color_manual,
    c(list(values = custom_palette, guide = guide), dots)
  )
}

scale_fill_custom <- function(..., guide = NULL, n_levels = NULL) {
  dots <- list(...)
  if (!is.null(dots$guide)) {
    guide <- dots$guide
    dots$guide <- NULL
  }
  if (is.null(dots$labels)) {
    dots$labels <- function(x) stringr::str_wrap(x, 20)
  }
  if (is.null(guide) && !is.null(n_levels)) {
    guide <- guide_legend(nrow = legend_rows_for(n_levels), byrow = TRUE)
  }
  do.call(
    scale_fill_manual,
    c(list(values = custom_palette, guide = guide), dots)
  )
}

add_zero_line <- function(axis = c("x", "y"),
                          color = "grey60",
                          linewidth = 0.4,
                          linetype = "dashed") {
  axis <- match.arg(axis)
  if (axis == "x") {
    geom_vline(xintercept = 0, color = color, linewidth = linewidth, linetype = linetype)
  } else {
    geom_hline(yintercept = 0, color = color, linewidth = linewidth, linetype = linetype)
  }
}

## Helper for angled x-axis labels: ensures proper alignment with vjust
angled_x_labels <- function(angle = 30, size = 9) {
  element_text(size = size, angle = angle, hjust = 1, vjust = 1)
}

## Custom ggplot theme function
custom_theme_scale <- function(font_family = default_font_family(), scale = 1) {
  enable_text_rendering()
  s <- lapply(theme_sizes, function(x) x * scale * theme_scale_multiplier)

  theme_minimal(base_family = font_family, base_size = 14) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = s$title,
        face = "bold",
        lineheight = 1.1,
        family = font_family,
        color = "grey10",
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = s$subtitle,
        lineheight = 1.15,
        color = "grey30",
        margin = margin(b = 10)
      ),

      axis.title = element_text(size = s$axis_title, color = "grey20"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text  = element_text(size = s$axis_text, color = "grey30"),
      axis.text.x = element_text(angle = 0, vjust = 0.5),

      strip.text = element_text(size = s$strip_text, face = "bold",
                                color = "grey20", margin = margin(b = 4)),

      legend.title = element_text(size = s$legend_title, color = "grey20"),
      legend.title.position = "top",
      legend.text  = element_text(size = s$legend_text, color = "grey30"),
      legend.position = "bottom",
      legend.key.height = unit(0.5, "cm"),
      legend.key.width  = unit(1.2, "cm"),
      legend.box.spacing = unit(0.4, "lines"),
      legend.margin = margin(t = 8, r = 0, b = 0, l = 0),

      plot.caption = element_text(
        size = s$caption_text,
        hjust = 0.5,
        color = "grey50",
        family = font_family,
        lineheight = 1.22,
        margin = margin(t = 10, b = 2)
      ),
      plot.caption.position = "plot",

      plot.margin = margin(14, 16, 18, 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),

      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "grey94", linewidth = 0.25),
      panel.grid.minor   = element_blank(),
      axis.ticks = element_line(color = "grey60", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt")
    )
}

custom_theme_results <- function(font_family = default_font_family(), scale = 1, title_size = 19) {
  custom_theme_scale(font_family = font_family, scale = scale) +
    theme(plot.title = element_text(size = title_size))
}
