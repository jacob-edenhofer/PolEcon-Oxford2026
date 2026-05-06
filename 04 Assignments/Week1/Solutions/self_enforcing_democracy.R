# Week 1 Assignment: self-enforcing democracy simulations
# Covers "A stylised model of self-enforcing democracy", task 2.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  script_path <- sub("^--file=", "", file_arg[1])
  if (!file.exists(script_path)) {
    script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
  }
  dirname(normalizePath(script_path, mustWork = FALSE))
} else {
  getwd()
}
output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
repo_root <- dirname(dirname(dirname(script_dir)))
theme_path <- file.path(repo_root, "03 Simulations", "Custom_scripts", "plot_theme.R")

suppressPackageStartupMessages(library(ggplot2))
if (file.exists(theme_path)) {
  source(theme_path)
  enable_text_rendering <- function() NULL
} else {
  custom_palette <- c("#0072B2", "#56B4E9", "#009E73", "#E69F00", "#D55E00")
  custom_theme_scale <- function(...) theme_minimal(base_size = 12)
}

save_png <- function(filename, plot, width = 8, height = 5.5) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height,
         dpi = 300, bg = "white")
}

delta <- 0.90
p <- 0.60
C <- 1.00
pi_bar <- 0.40
kappa <- (1 - delta) * C
S <- seq(0.1, 2, length.out = 500)

pi_min <- function(S, p = 0.60, kappa = 0.10) {
  p - kappa / S
}

stake_cutoff <- function(p, pi_bar, kappa) {
  if (p <= pi_bar) {
    Inf
  } else {
    kappa / (p - pi_bar)
  }
}

stake_cutoff_gamma <- function(p, pi_bar, kappa, gamma) {
  if (gamma == 0) {
    return(stake_cutoff(p, pi_bar, kappa))
  }
  ((pi_bar - p) + sqrt((pi_bar - p)^2 + 4 * gamma * kappa)) / (2 * gamma)
}

base <- data.frame(
  S = S,
  pi_min = pi_min(S, p, kappa),
  pi_bar = pi_bar
)
base$complies <- base$pi_min <= base$pi_bar
S_max <- stake_cutoff(p, pi_bar, kappa)

cat("\nSelf-enforcing democracy baseline\n")
cat("----------------------------------\n")
cat("delta =", delta, "p =", p, "C =", C, "pi_bar =", pi_bar, "\n")
cat("kappa = (1 - delta) * C =", kappa, "\n")
cat("Analytic S_max = kappa / (p - pi_bar) =", S_max, "\n")
cat("Numerical grid cutoff =", max(base$S[base$complies]), "\n")

C_values <- c(0.5, 1, 1.5)
p_values <- c(0.4, 0.6, 0.8)
gamma_values <- c(0, 0.1, 0.2)

baseline_plot <- ggplot(base, aes(S, pi_min)) +
  annotate("rect", xmin = min(S), xmax = S_max, ymin = -Inf, ymax = Inf,
           fill = custom_palette[3], alpha = 0.10) +
  geom_line(color = custom_palette[1], linewidth = 1) +
  geom_hline(yintercept = pi_bar, color = "grey35", linetype = "dashed") +
  geom_vline(xintercept = S_max, color = custom_palette[3], linetype = "dotted") +
  annotate("label", x = S_max, y = pi_bar + 0.08, label = "Smax = 0.50",
           size = 3.5, label.size = 0, fill = "white") +
  labs(
    title = "Self-Enforcing Democracy: Baseline",
    subtitle = "Shaded values of S satisfy pi_min(S) <= pi_bar",
    x = "Stakes of office S",
    y = "Minimum return probability"
  ) +
  coord_cartesian(ylim = c(-0.45, 0.65), expand = FALSE) +
  custom_theme_scale()
save_png("self_enforcing_baseline.png", baseline_plot)

C_grid <- do.call(rbind, lapply(C_values, function(C_i) {
  data.frame(S = S, C = factor(C_i), pi_min = pi_min(S, p, (1 - delta) * C_i))
}))
C_plot <- ggplot(C_grid, aes(S, pi_min, color = C)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = pi_bar, color = "grey35", linetype = "dashed") +
  scale_color_manual(values = custom_palette[c(1, 3, 5)], name = "Cost C") +
  labs(
    title = "Higher Fighting Costs Expand Compliance",
    subtitle = "C enters through kappa = (1 - delta)C, so the shift is muted when delta is high",
    x = "Stakes of office S",
    y = "Minimum return probability"
  ) +
  coord_cartesian(ylim = c(-0.25, 0.65), expand = FALSE) +
  custom_theme_scale()
save_png("self_enforcing_vary_cost.png", C_plot)

p_grid <- do.call(rbind, lapply(p_values, function(p_i) {
  data.frame(S = S, p = factor(p_i), pi_min = pi_min(S, p_i, kappa))
}))
p_plot <- ggplot(p_grid, aes(S, pi_min, color = p)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = pi_bar, color = "grey35", linetype = "dashed") +
  scale_color_manual(values = custom_palette[c(1, 3, 5)], name = "Force success p") +
  labs(
    title = "Force Success Probability Shifts the Whole Constraint",
    subtitle = "Lower p makes compliance possible over a wider range of stakes",
    x = "Stakes of office S",
    y = "Minimum return probability"
  ) +
  coord_cartesian(ylim = c(-0.25, 0.85), expand = FALSE) +
  custom_theme_scale()
save_png("self_enforcing_vary_force_success.png", p_plot)

gamma_grid <- do.call(rbind, lapply(gamma_values, function(gamma_i) {
  data.frame(
    S = S,
    gamma = paste0("pi(S), gamma = ", gamma_i),
    pi_min = pi_min(S, p, kappa),
    pi_S = pi_bar - gamma_i * S
  )
}))
gamma_cutoffs <- data.frame(
  S = sapply(gamma_values, function(g_i) stake_cutoff_gamma(p, pi_bar, kappa, g_i)),
  gamma = paste0("pi(S), gamma = ", gamma_values)
)
gamma_cutoffs$pi_cross <- pi_min(gamma_cutoffs$S, p, kappa)
gamma_plot <- ggplot(gamma_grid, aes(S)) +
  geom_line(aes(y = pi_min, color = "pi_min(S)"), linewidth = 1.1) +
  geom_line(aes(y = pi_S, color = gamma), linewidth = 1) +
  geom_point(data = gamma_cutoffs, aes(S, pi_cross, color = gamma),
             size = 2.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("pi_min(S)" = custom_palette[1],
               "pi(S), gamma = 0" = custom_palette[3],
               "pi(S), gamma = 0.1" = custom_palette[4],
               "pi(S), gamma = 0.2" = custom_palette[5]),
    name = NULL
  ) +
  labs(
    title = "When Alternation Falls with Stakes",
    subtitle = "Compliance ends where each pi(S) line crosses pi_min(S)",
    x = "Stakes of office S",
    y = "Probability"
  ) +
  coord_cartesian(ylim = c(0, 0.65), expand = FALSE) +
  custom_theme_scale()
save_png("self_enforcing_stake_dependent_pi.png", gamma_plot, width = 8, height = 5.5)

cat("\nComparative statics cutoffs\n")
cat("---------------------------\n")
print(data.frame(
  C = C_values,
  kappa = (1 - delta) * C_values,
  S_max = sapply(C_values, function(C_i) stake_cutoff(p, pi_bar, (1 - delta) * C_i))
), row.names = FALSE)

print(data.frame(
  p = p_values,
  S_max = sapply(p_values, function(p_i) stake_cutoff(p_i, pi_bar, kappa))
), row.names = FALSE)

print(data.frame(
  gamma = gamma_values,
  S_max = sapply(gamma_values, function(g_i) stake_cutoff_gamma(p, pi_bar, kappa, g_i))
), row.names = FALSE)

cat("\nPNG plots saved to:", output_dir, "\n")
