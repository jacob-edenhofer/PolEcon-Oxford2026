# Week 1 Assignment: Fearon (1999) selection-sanctioning trade-off
# Covers "Analysing the Fearon (1999) model", task 2.

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

W <- 1
delta <- 0.9
n <- 1000
xgrid <- seq(0, 1, length.out = 1001)

bad_payoff <- function(x, cutoff, sigma, W = 1, delta = 0.9) {
  W - (1 - x)^2 + delta * W * (1 - pnorm((cutoff + x^2) / sigma))
}

best_response <- function(x_hat, sigma, W = 1, delta = 0.9) {
  cutoff <- -x_hat^2 / 2
  payoff <- bad_payoff(xgrid, cutoff, sigma, W, delta)
  xgrid[which.max(payoff)]
}

mixed_fixed_point <- function(sigma, W = 1, delta = 0.9) {
  br <- sapply(xgrid, best_response, sigma = sigma, W = W, delta = delta)
  idx <- which.min(abs(br - xgrid))
  list(x = xgrid[idx], br = br)
}

simulate_types <- function(xb, cutoff, sigma, n = 1000) {
  good_signal <- rnorm(n, mean = 0, sd = sigma)
  bad_signal <- rnorm(n, mean = -xb^2, sd = sigma)
  good_reelected <- good_signal >= cutoff
  bad_reelected <- bad_signal >= cutoff
  data.frame(
    xb = xb,
    cutoff = cutoff,
    sigma = sigma,
    good_reelection_rate = mean(good_reelected),
    bad_reelection_rate = mean(bad_reelected),
    share_bad_among_reelected = sum(bad_reelected) / (sum(good_reelected) + sum(bad_reelected))
  )
}

set.seed(7)
sigma <- 0.5
f0 <- dnorm(0, mean = 0, sd = sigma)
xS <- 1 / (1 + delta * W * f0)
kS <- -xS^2

cat("\nFearon pure-sanctioning benchmark\n")
cat("---------------------------------\n")
cat("sigma =", sigma, "f(0) =", f0, "\n")
cat("xS = 1 / (1 + delta * W * f(0)) =", xS, "\n")
cat("kS = -xS^2 =", kS, "\n")

xb <- 0.5
kP <- -xb^2 / 2
pure_selection <- simulate_types(xb, kP, sigma, n)
cat("\nPure selection simulation, xb = 0.5 and kP = -xb^2 / 2\n")
print(pure_selection, row.names = FALSE)

mixed <- mixed_fixed_point(sigma, W, delta)
xM <- mixed$x
kM <- -xM^2 / 2
mixed_selection <- simulate_types(xM, kM, sigma, n)
cat("\nMixed-case fixed point\n")
cat("xM =", xM, "kM =", kM, "xM - xS =", xM - xS, "\n")
print(mixed_selection, row.names = FALSE)

sigma_values <- c(0.25, 0.5, 1)
sigma_summary <- do.call(rbind, lapply(sigma_values, function(s) {
  f0_s <- dnorm(0, mean = 0, sd = s)
  xS_s <- 1 / (1 + delta * W * f0_s)
  mixed_s <- mixed_fixed_point(s, W, delta)
  data.frame(
    sigma = s,
    f0 = f0_s,
    xS = xS_s,
    xM = mixed_s$x,
    gap_xM_minus_xS = mixed_s$x - xS_s
  )
}))

cat("\nRepeating benchmark and mixed case across signal noise\n")
print(sigma_summary, row.names = FALSE)

br_plot_data <- data.frame(x_hat = xgrid, x_br = mixed$br)
br_plot <- ggplot(br_plot_data, aes(x_hat, x_br)) +
  geom_abline(slope = 1, intercept = 0, color = "grey45", linetype = "dashed") +
  geom_line(color = custom_palette[1], linewidth = 1) +
  geom_vline(xintercept = xM, color = custom_palette[3], linetype = "dotted") +
  geom_hline(yintercept = xM, color = custom_palette[3], linetype = "dotted") +
  annotate("label", x = xM + 0.12, y = xM - 0.07,
           label = sprintf("fixed point = %.3f", xM),
           size = 3.5, label.size = 0, fill = "white") +
  labs(
    title = "Mixed Selection-Sanctioning Fixed Point",
    subtitle = "The fixed point is where the bad type's best response crosses the 45-degree line",
    x = "Anticipated bad-type choice xhat",
    y = "Best response xBR(xhat)"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  custom_theme_scale()
save_png("fearon99_mixed_best_response.png", br_plot)

sigma_long <- rbind(
  data.frame(sigma = sigma_summary$sigma, benchmark = "Pure sanctioning", x = sigma_summary$xS),
  data.frame(sigma = sigma_summary$sigma, benchmark = "Mixed case", x = sigma_summary$xM)
)
sigma_plot <- ggplot(sigma_long, aes(sigma, x, color = benchmark)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  scale_color_manual(values = custom_palette[c(5, 1)], name = NULL) +
  labs(
    title = "Selection Weakens Moderation Relative to Pure Sanctioning",
    subtitle = "For each noise level, the mixed-case policy is further from the voter's ideal",
    x = "Signal noise sigma",
    y = "Bad-type policy x"
  ) +
  coord_cartesian(ylim = c(0.35, 0.8), expand = FALSE) +
  custom_theme_scale()
save_png("fearon99_sanctioning_vs_mixed_noise.png", sigma_plot)

cat("\nPNG plots saved to:", output_dir, "\n")
