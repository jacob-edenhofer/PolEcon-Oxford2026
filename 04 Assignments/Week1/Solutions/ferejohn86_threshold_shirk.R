# Week 1 Assignment: Ferejohn (1986) threshold-and-shirk simulation
# Covers "Analysing the Ferejohn (1986) model", task 2.

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

save_png <- function(filename, plot, width = 9, height = 6) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height,
         dpi = 300, bg = "white")
}

simulate_ferejohn <- function(theta, K, phi, Delta, label) {
  theta_safe <- pmax(theta, .Machine$double.eps)
  cost_clear <- phi(K / theta_safe)
  reelect <- cost_clear <= Delta
  effort <- ifelse(reelect, K / theta_safe, 0)
  performance <- effort * theta_safe
  data.frame(
    cost = label,
    theta = theta,
    effort = effort,
    performance = performance,
    reelect = reelect
  )
}

summarise_run <- function(dat, Delta, analytic_performance) {
  data.frame(
    Delta = Delta,
    cost = dat$cost[1],
    shirking_rate = mean(!dat$reelect),
    reelection_rate = mean(dat$reelect),
    mean_effort_when_reelected = mean(dat$effort[dat$reelect]),
    mean_performance = mean(dat$performance),
    analytic_mean_performance = analytic_performance
  )
}

set.seed(42)
T <- 1000
theta <- runif(T)

phi_linear <- function(a) a
phi_quadratic <- function(a) 0.5 * a^2

run_for_delta <- function(Delta, theta) {
  K_linear <- Delta / 2
  K_quadratic <- sqrt(Delta / 2)

  sim_linear <- simulate_ferejohn(theta, K_linear, phi_linear, Delta, "linear")
  sim_quadratic <- simulate_ferejohn(theta, K_quadratic, phi_quadratic, Delta, "quadratic")

  rbind(
    summarise_run(sim_linear, Delta, Delta / 4),
    summarise_run(sim_quadratic, Delta, sqrt(Delta / 8))
  )
}

Delta <- 1
K_linear <- Delta / 2
K_quadratic <- sqrt(Delta / 2)
sim_linear <- simulate_ferejohn(theta, K_linear, phi_linear, Delta, "linear")
sim_quadratic <- simulate_ferejohn(theta, K_quadratic, phi_quadratic, Delta, "quadratic")
sim <- rbind(sim_linear, sim_quadratic)

cat("\nFerejohn threshold-and-shirk benchmarks\n")
cat("---------------------------------------\n")
cat("Linear cost: K* = Delta / 2 =", K_linear,
    "; theta* = 1/2; E[u] = Delta / 4 =", Delta / 4, "\n")
cat("Quadratic cost: K* = sqrt(Delta / 2) =", K_quadratic,
    "; theta* = 1/2; E[u] = sqrt(Delta / 8) =", sqrt(Delta / 8), "\n")

cat("\nSimulation check, Delta = 1\n")
print(run_for_delta(Delta, theta), row.names = FALSE)

Delta_values <- c(0.5, 1, 2)
Delta_summary <- do.call(rbind, lapply(Delta_values, run_for_delta, theta = theta))
cat("\nComparative statics in Delta\n")
print(Delta_summary, row.names = FALSE)

sim_long <- rbind(
  data.frame(cost = sim$cost, measure = "Effort", value = sim$effort),
  data.frame(cost = sim$cost, measure = "Performance", value = sim$performance)
)
sim_long$cost <- factor(sim_long$cost, levels = c("linear", "quadratic"),
                        labels = c("Linear cost", "Quadratic cost"))
sim_long$measure <- factor(sim_long$measure, levels = c("Effort", "Performance"))

distribution_plot <- ggplot(sim_long, aes(value, fill = cost)) +
  geom_histogram(bins = 40, color = "white", linewidth = 0.15, alpha = 0.85) +
  facet_grid(cost ~ measure, scales = "free_x") +
  scale_fill_manual(values = custom_palette[c(1, 5)], guide = "none") +
  labs(
    title = "Ferejohn Threshold-and-Shirk Simulation",
    subtitle = "Shirking creates a mass at zero; clearing produces exactly the voter cutoff K",
    x = NULL,
    y = "Number of periods"
  ) +
  custom_theme_scale() +
  theme(strip.background = element_rect(fill = "grey96", color = NA))
save_png("ferejohn86_effort_performance.png", distribution_plot, width = 9, height = 6.5)

Delta_summary$cost <- factor(Delta_summary$cost, levels = c("linear", "quadratic"),
                             labels = c("Linear cost", "Quadratic cost"))
Delta_plot <- ggplot(Delta_summary, aes(Delta, mean_performance, color = cost)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  geom_line(aes(y = analytic_mean_performance), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = custom_palette[c(1, 5)], name = NULL) +
  labs(
    title = "Higher Delta Raises Attainable Performance",
    subtitle = "Solid lines are simulated means; dashed lines are analytic benchmarks",
    x = "Discounted value of office Delta",
    y = "Mean observed performance"
  ) +
  custom_theme_scale()
save_png("ferejohn86_delta_comparative_statics.png", Delta_plot)

cat("\nPNG plots saved to:", output_dir, "\n")
