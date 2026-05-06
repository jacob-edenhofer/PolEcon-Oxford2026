# Week 1 Assignment: Fearon (1999) ignorance, sophistication, and commitment
# Covers "Analysing the Fearon (1999) model", task 4(f)-(g).

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

save_png <- function(filename, plot, width = 8.5, height = 5.5) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height,
         dpi = 300, bg = "white")
}

W <- 1
delta <- 0.9
xgrid <- seq(0, 1, length.out = 1001)
betas <- c(0, 0.25, 0.49, 0.51, 0.75, 1)

effective_cutoff <- function(x_hat, beta) {
  k_ignorant <- -x_hat^2
  k_sophisticated <- -x_hat^2 / 2
  if (beta < 0.5) {
    k_ignorant
  } else {
    k_sophisticated
  }
}

bad_payoff <- function(x, cutoff, sigma, W = 1, delta = 0.9) {
  W - (1 - x)^2 + delta * W * (1 - pnorm((cutoff + x^2) / sigma))
}

best_response <- function(x_hat, beta, sigma, W = 1, delta = 0.9) {
  cutoff <- effective_cutoff(x_hat, beta)
  payoff <- bad_payoff(xgrid, cutoff, sigma, W, delta)
  xgrid[which.max(payoff)]
}

fixed_point <- function(beta, sigma, W = 1, delta = 0.9) {
  br <- sapply(xgrid, best_response, beta = beta, sigma = sigma, W = W, delta = delta)
  idx <- which.min(abs(br - xgrid))
  data.frame(
    beta = beta,
    sigma = sigma,
    effective_rule = ifelse(beta < 0.5, "ignorant-majority sanctioning cutoff", "sophisticated-majority selection cutoff"),
    xb_star = xgrid[idx],
    best_response_gap = br[idx] - xgrid[idx]
  )
}

constant_sigma <- 0.5
constant_results <- do.call(rbind, lapply(betas, fixed_point, sigma = constant_sigma, W = W, delta = delta))
constant_results$scenario <- "constant sigma"

sigma_beta <- function(beta) 1 - 0.5 * beta
improving_results <- do.call(rbind, lapply(betas, function(beta) {
  fixed_point(beta, sigma_beta(beta), W, delta)
}))
improving_results$scenario <- "sigma(beta) = 1 - 0.5 beta"

results <- rbind(constant_results, improving_results)

cat("\nIgnorance, sophistication, and voter commitment\n")
cat("-----------------------------------------------\n")
cat("For beta < 1/2, majority rule implements kI = -xhat^2.\n")
cat("For beta > 1/2, majority rule implements kS = -xhat^2 / 2.\n")
cat("The beta = 1/2 knife-edge is intentionally omitted.\n\n")
print(results[, c("scenario", "beta", "sigma", "effective_rule", "xb_star", "best_response_gap")],
      row.names = FALSE)

results$scenario <- factor(
  results$scenario,
  levels = c("constant sigma", "sigma(beta) = 1 - 0.5 beta"),
  labels = c("Constant signal noise", "Sophistication improves signals")
)

sophistication_plot <- ggplot(results, aes(beta, xb_star, color = scenario)) +
  annotate("rect", xmin = 0, xmax = 0.5, ymin = -Inf, ymax = Inf,
           fill = custom_palette[3], alpha = 0.07) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = custom_palette[5], alpha = 0.06) +
  geom_vline(xintercept = 0.5, color = "grey35", linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  annotate("text", x = 0.25, y = max(results$xb_star) + 0.015,
           label = "ignorant majority", size = 3.4, color = "grey30") +
  annotate("text", x = 0.75, y = max(results$xb_star) + 0.015,
           label = "sophisticated majority", size = 3.4, color = "grey30") +
  scale_color_manual(values = custom_palette[c(5, 1)], name = NULL) +
  labs(
    title = "Sophistication Creates a Commitment Trade-Off",
    subtitle = "At beta = 1/2, majority rule switches from the sanctioning cutoff to the selection cutoff",
    x = "Share sophisticated voters beta",
    y = "Equilibrium bad-type choice xb*"
  ) +
  coord_cartesian(ylim = c(min(results$xb_star) - 0.03, max(results$xb_star) + 0.04),
                  expand = FALSE) +
  custom_theme_scale()
save_png("fearon99_ignorance_sophistication_fixed_points.png", sophistication_plot)

cat("\nPNG plots saved to:", output_dir, "\n")
