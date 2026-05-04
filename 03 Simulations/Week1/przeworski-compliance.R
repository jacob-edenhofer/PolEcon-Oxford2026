# =============================================================================
# Przeworski (2015) / Fearon (2011): the compliance condition
# -----------------------------------------------------------------------------
# Companion to the Week 1 slides. Two figures:
#   (A) The compliance gap V_E - V_F as a function of the stakes S.
#   (B) The minimum return-probability pi_min(S) = p - kappa/S, with the
#       institutional ceiling pi-bar and the upper bound on stakes
#       S* = kappa/(p - pi-bar) marked.
# =============================================================================

library(here)

custom_paths <- here::here("03 Simulations", "Custom_scripts")
source(file.path(custom_paths, "custom_packages.R"))
source(file.path(custom_paths, "plot_theme.R"))

enable_text_rendering()
font_family <- default_font_family()

figures_dir <- here::here("03 Simulations", "Week1", "Figures")
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# Parameters (match the slide example)
# -----------------------------------------------------------------------------
delta   <- 0.90   # discount factor
p       <- 0.60   # probability of taking power by force
C       <- 1.00   # one-time cost of fighting
L       <- 1.00   # per-period payoff in opposition
pi_elec <- 0.40   # institutional ceiling on the chance of returning via elections
pi_bar  <- pi_elec

kappa <- (1 - delta) * C   # per-period equivalent of the fighting cost

# Closed-form bound on the stakes of power (only meaningful when p > pi_bar).
S_star <- if (p > pi_bar) kappa / (p - pi_bar) else NA_real_

stakes <- seq(0.01, 2, length.out = 600)

V_E <- (L + pi_bar * stakes) / (1 - delta)
V_F <- (L + p      * stakes) / (1 - delta) - C
gap <- V_E - V_F

# pi_min(S) = p - kappa/S. For S below kappa/p the value is negative, which
# just means "any return-probability >= 0 satisfies compliance" -- the
# constraint does not bind. Clamp at 0 for plotting so the binding region
# is visible.
pi_min_raw <- p - kappa / stakes
pi_min     <- pmax(pi_min_raw, 0)

df <- tibble(
  S          = stakes,
  V_E        = V_E,
  V_F        = V_F,
  gap        = gap,
  pi_min     = pi_min,
  pi_min_raw = pi_min_raw
)

# -----------------------------------------------------------------------------
# Figure A: compliance gap V_E - V_F as a function of S
# -----------------------------------------------------------------------------
gap_plot <- ggplot(df, aes(x = S, y = gap)) +
  geom_ribbon(data = filter(df, gap >= 0),
              aes(ymin = 0, ymax = gap),
              fill = custom_palette[3], alpha = 0.25) +
  geom_ribbon(data = filter(df, gap <  0),
              aes(ymin = gap, ymax = 0),
              fill = custom_palette[5], alpha = 0.25) +
  geom_line(linewidth = 1.1, colour = custom_palette[1]) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  {if (!is.na(S_star)) geom_vline(xintercept = S_star,
                                  linetype = "dotted", colour = "grey30")} +
  {if (!is.na(S_star)) ggplot2::annotate("label", x = S_star, y = max(df$gap) * 0.85,
                                         label = sprintf("S* = %.2f", S_star),
                                         family = font_family, size = 4,
                                         label.size = 0, fill = "white")} +
  labs(
    title    = wrap_plot_title("When do losers prefer to wait rather than fight?", width = 50),
    subtitle = wrap_plot_subtitle(
      sprintf("Compliance gap V_E - V_F as a function of the stakes S (delta = %.2f, p = %.2f, C = %.2f, pi-bar = %.2f)",
              delta, p, C, pi_bar),
      width = 60),
    x = "Stakes of power, S = R - L",
    y = expression(V[E] - V[F]),
    caption = wrap_plot_caption(
      "Green = comply (V_E >= V_F). Red = fight is preferred. Dotted line marks S* = kappa / (p - pi-bar).",
      width = 70)
  ) +
  custom_theme_scale(font_family = font_family)

# -----------------------------------------------------------------------------
# Figure B: pi_min(S) = p - kappa/S, with pi-bar and S* marked
# -----------------------------------------------------------------------------
pi_plot <- ggplot(df, aes(x = S, y = pi_min)) +
  geom_ribbon(data = filter(df, pi_min <= pi_bar),
              aes(ymin = pi_min, ymax = pi_bar),
              fill = custom_palette[3], alpha = 0.25) +
  geom_ribbon(data = filter(df, pi_min >= pi_bar),
              aes(ymin = pi_bar, ymax = pi_min),
              fill = custom_palette[5], alpha = 0.25) +
  geom_line(linewidth = 1.1, colour = custom_palette[1]) +
  geom_hline(yintercept = pi_bar,
             linetype = "dashed", colour = custom_palette[4]) +
  geom_hline(yintercept = p,
             linetype = "dotted", colour = "grey50") +
  {if (!is.na(S_star)) geom_vline(xintercept = S_star,
                                  linetype = "dotted", colour = "grey30")} +
  ggplot2::annotate("label", x = max(df$S) * 0.97, y = pi_bar,
                    label = "bar(pi)", parse = TRUE,
                    hjust = 1, vjust = -0.3,
                    family = font_family, size = 4.5,
                    label.size = 0, fill = "white") +
  ggplot2::annotate("label", x = max(df$S) * 0.97, y = p,
                    label = "p", parse = TRUE,
                    hjust = 1, vjust = -0.3,
                    family = font_family, size = 4.5,
                    label.size = 0, fill = "white") +
  {if (!is.na(S_star)) ggplot2::annotate("label", x = S_star, y = 0.05,
                                         label = sprintf("S* = %.2f", S_star),
                                         family = font_family, size = 4,
                                         label.size = 0, fill = "white")} +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title    = wrap_plot_title("How much electoral hope do losers need?", width = 50),
    subtitle = wrap_plot_subtitle(
      "Required chance of return rises with the stakes; democracy self-enforces below the institutional ceiling",
      width = 60),
    x = "Stakes of power, S = R - L",
    y = expression("Required chance of return, "*pi[min](S)),
    caption = wrap_plot_caption(
      "Green: institutions can credibly deliver the required chance of return. Red: fighting is preferred. Curve clamped at 0 (where the constraint does not bind).",
      width = 70)
  ) +
  custom_theme_scale(font_family = font_family)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------
save_plot(file.path(figures_dir, "compliance_gap.png"),    gap_plot, width = 9, height = 6)
save_plot(file.path(figures_dir, "compliance_pi_min.png"), pi_plot,  width = 9, height = 6)

combined <- gap_plot | pi_plot
save_plot(file.path(figures_dir, "compliance_combined.png"),
          combined, width = 16, height = 6)

message("Saved figures to: ", figures_dir)
message("S* = ", round(S_star, 4))
