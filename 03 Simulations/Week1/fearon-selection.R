# =============================================================================
# Fearon (1999): Three figures for the lecture slides
#   1. Pure selection (Fig 2.1 reproduction): two normal densities + cutoff
#   2. Commitment problem: x_b vs. x_mh as monitoring varies
#   3. Inverted U: efficacy of selection (Pr(good elected) - Pr(bad elected))
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
# Parameters shared across figures
# -----------------------------------------------------------------------------
delta <- 0.9    # discount factor
W     <- 2      # value of office (per period)

# -----------------------------------------------------------------------------
# Figure 1: pure selection (reproduces Fearon's Figure 2.1)
# -----------------------------------------------------------------------------
x_hat_1 <- 0.5
sigma_1 <- 0.15

z_grid_1 <- seq(-1, 0.5, length.out = 600)

df_pure_selection <- bind_rows(
  tibble(z = z_grid_1,
         density = dnorm(z_grid_1, mean = 0,         sd = sigma_1),
         type    = "Good type (centred at 0)"),
  tibble(z = z_grid_1,
         density = dnorm(z_grid_1, mean = -x_hat_1^2, sd = sigma_1),
         type    = sprintf("Bad type (centred at -x̂² = %.2f)", -x_hat_1^2))
)

k_star_select <- -x_hat_1^2 / 2

p1 <- ggplot(df_pure_selection, aes(x = z, y = density, fill = type, colour = type)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = k_star_select, linetype = "dashed", colour = "grey20") +
  ggplot2::annotate(
    "label", x = k_star_select,
    y = max(df_pure_selection$density) * 0.95,
    label = sprintf("k* = -x̂²/2 = %.2f", k_star_select),
    family = font_family, size = 4, label.size = 0, fill = "white"
  ) +
  scale_fill_manual(values = setNames(
    c(custom_palette[3], custom_palette[5]),
    unique(df_pure_selection$type)
  )) +
  scale_colour_manual(values = setNames(
    c(custom_palette[3], custom_palette[5]),
    unique(df_pure_selection$type)
  )) +
  labs(
    title    = wrap_plot_title("Pure selection: voter's cutoff sits at the midpoint", width = 60),
    subtitle = wrap_plot_subtitle(
      sprintf("Good and bad types' welfare distributions (sigma = %.2f). Cutoff k* derived from Bayes' rule.", sigma_1),
      width = 75),
    x = "Observed welfare z = -x² + noise",
    y = "Density",
    caption = wrap_plot_caption(
      "Above k*, the likelihood ratio favours the good type and the voter reelects. Below k*, she replaces.",
      width = 90),
    fill = NULL, colour = NULL
  ) +
  custom_theme_scale(font_family = font_family) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# Figure 2: the commitment problem
#   x_mh: pure-sanctioning policy = 1 / (1 + delta*W*f(0))
#   x_b : mixed-case bad-type policy, solving f(-x_b^2/2) = (1 - x_b)/(x_b * delta * W)
# -----------------------------------------------------------------------------
sigma_grid <- seq(0.10, 1.50, length.out = 300)

# Pure sanctioning
f0_grid   <- dnorm(0, mean = 0, sd = sigma_grid)
x_mh_grid <- 1 / (1 + delta * W * f0_grid)

# Mixed case: solve for x_b on (0, 1) for each sigma
solve_x_b <- function(sigma, delta, W) {
  obj <- function(x) {
    f_val <- dnorm(-x^2 / 2, mean = 0, sd = sigma)
    rhs   <- (1 - x) / (x * delta * W)
    f_val - rhs
  }
  out <- tryCatch(
    uniroot(obj, c(1e-4, 1 - 1e-4), tol = 1e-8)$root,
    error = function(e) NA_real_
  )
  out
}
x_b_grid <- vapply(sigma_grid, solve_x_b, numeric(1), delta = delta, W = W)

# Two-panel layout: top shows both policies and ribbon between them;
# bottom magnifies the gap (commitment loss) on its own scale.
df_commit_wide <- tibble(
  sigma = sigma_grid,
  x_mh  = x_mh_grid,
  x_b   = x_b_grid,
  gap   = x_b_grid - x_mh_grid
)

df_commit_long <- df_commit_wide |>
  pivot_longer(c(x_mh, x_b), names_to = "case", values_to = "policy") |>
  mutate(case = dplyr::case_match(case,
                                  "x_mh" ~ "Pure sanctioning (x*)",
                                  "x_b"  ~ "Mixed case, bad type (x_b)"))

p2_top <- ggplot(df_commit_wide, aes(x = sigma)) +
  geom_ribbon(aes(ymin = x_mh, ymax = x_b),
              fill = custom_palette[5], alpha = 0.25) +
  geom_line(aes(y = x_mh, colour = "Pure sanctioning (x*)"),
            linewidth = 1.1) +
  geom_line(aes(y = x_b, colour = "Mixed case, bad type (x_b)"),
            linewidth = 1.1) +
  scale_colour_manual(values = c(
    "Pure sanctioning (x*)"        = custom_palette[1],
    "Mixed case, bad type (x_b)"   = custom_palette[5]
  )) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = wrap_plot_title("The commitment problem in policy choice", width = 55),
    subtitle = wrap_plot_subtitle(
      sprintf("Bad types shirk more in the mixed case (delta = %.2f, W = %.0f). Shaded band shows the gap.",
              delta, W),
      width = 75),
    x = NULL,
    y = "Bad type's policy choice",
    colour = NULL
  ) +
  custom_theme_scale(font_family = font_family) +
  theme(legend.position = "top")

p2_bottom <- ggplot(df_commit_wide, aes(x = sigma, y = gap)) +
  geom_area(fill = custom_palette[5], alpha = 0.30) +
  geom_line(colour = custom_palette[5], linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(
    subtitle = wrap_plot_subtitle(
      "The commitment loss x_b - x* (zoomed). Strictly positive everywhere -- the result holds for any alpha > 0.",
      width = 80),
    x = "Noise standard deviation (higher = worse monitoring)",
    y = "x_b - x*",
    caption = wrap_plot_caption(
      "x = 0 is the voter's ideal, x = 1 is the bad type's unconstrained ideal. Magnitude is small under normal noise -- the pathology is qualitative, not large.",
      width = 90)
  ) +
  custom_theme_scale(font_family = font_family)

p2 <- p2_top / p2_bottom + patchwork::plot_layout(heights = c(2, 1))

# -----------------------------------------------------------------------------
# Figure 3: inverted U in selection efficacy (mixed case)
# Difference = 2 * Phi(x_b^2 / (2 sigma)) - 1
# Compare with pure selection (fixed x_hat = 0.5)
# -----------------------------------------------------------------------------
sigma_grid_3 <- seq(0.05, 2.0, length.out = 400)

# Pure selection: x_hat is exogenous
diff_pure <- 2 * pnorm(x_hat_1^2 / (2 * sigma_grid_3)) - 1

# Mixed: x_b varies with sigma
x_b_mixed <- vapply(sigma_grid_3, solve_x_b, numeric(1), delta = delta, W = W)
diff_mixed <- 2 * pnorm(x_b_mixed^2 / (2 * sigma_grid_3)) - 1

df_inverted_u <- tibble(
  sigma      = sigma_grid_3,
  diff_pure  = diff_pure,
  diff_mixed = diff_mixed
) |>
  pivot_longer(c(diff_pure, diff_mixed), names_to = "case", values_to = "diff") |>
  mutate(case = dplyr::case_match(case,
                                  "diff_pure"  ~ sprintf("Pure selection (fixed bad-type policy = %.2f)", x_hat_1),
                                  "diff_mixed" ~ "Mixed case (endogenous x_b)"))

p3 <- ggplot(df_inverted_u, aes(x = sigma, y = diff, colour = case)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = setNames(
    c(custom_palette[1], custom_palette[3]),
    unique(df_inverted_u$case)
  )) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = wrap_plot_title("How well can voters select? Two regimes", width = 55),
    subtitle = wrap_plot_subtitle(
      "Pr(good reelected) - Pr(bad reelected) as a function of monitoring quality",
      width = 75),
    x = "Noise standard deviation (higher = worse monitoring)",
    y = "Selection efficacy",
    caption = wrap_plot_caption(
      "Pure selection (constant policy gap) is monotone. Mixed case is hump-shaped: at high monitoring, bad types mimic; at low monitoring, signals are too noisy.",
      width = 90),
    colour = NULL
  ) +
  custom_theme_scale(font_family = font_family) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------
save_plot(file.path(figures_dir, "fearon_pure_selection.png"), p1, width = 9, height = 6)
save_plot(file.path(figures_dir, "fearon_commitment.png"),     p2, width = 9, height = 6)
save_plot(file.path(figures_dir, "fearon_inverted_u.png"),     p3, width = 9, height = 6)

message("Saved Fearon figures to: ", figures_dir)
