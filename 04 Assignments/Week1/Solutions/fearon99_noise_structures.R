# Week 1 Assignment: Fearon (1999) noise structures and voter mistakes
# Covers "Analysing the Fearon (1999) model", task 3.

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

set.seed(11)
n <- 1000
xb <- 0.5
cutoff <- -xb^2 / 2
sigma_values <- c(0.25, 0.5, 1)

classification_summary <- function(good_signal, bad_signal, cutoff, label, sigma = NA_real_) {
  data.frame(
    noise = label,
    sigma = sigma,
    cutoff = cutoff,
    bad_reelected_false_positive = mean(bad_signal >= cutoff),
    good_removed_false_negative = mean(good_signal < cutoff),
    share_bad_among_reelected = sum(bad_signal >= cutoff) /
      (sum(good_signal >= cutoff) + sum(bad_signal >= cutoff))
  )
}

standard_good <- rnorm(n)
standard_bad <- rnorm(n)

normal_results <- do.call(rbind, lapply(sigma_values, function(s) {
  good_signal <- s * standard_good
  bad_signal <- -xb^2 + s * standard_bad
  classification_summary(good_signal, bad_signal, cutoff, "additive normal", s)
}))

cat("\nAdditive symmetric normal noise\n")
cat("-------------------------------\n")
cat("With symmetric additive noise, the midpoint cutoff is k = -xb^2 / 2 =", cutoff, "\n")
print(normal_results, row.names = FALSE)

set.seed(12)
shape <- 0.75
raw_sd <- sqrt(exp(shape^2) - 1)
target_sd <- 0.5
lognormal_scale <- target_sd / raw_sd
shifted_lognormal <- function(n) {
  # rlnorm with meanlog = -shape^2 / 2 has mean one; subtract one for mean-zero noise.
  lognormal_scale * (rlnorm(n, meanlog = -shape^2 / 2, sdlog = shape) - 1)
}

good_skew <- shifted_lognormal(n)
bad_skew <- -xb^2 + shifted_lognormal(n)
skew_result <- classification_summary(good_skew, bad_skew, cutoff, "additive shifted lognormal", target_sd)

cat("\nAdditive skewed noise\n")
cat("---------------------\n")
cat("The same midpoint cutoff need not balance errors once symmetry is removed.\n")
print(skew_result, row.names = FALSE)

set.seed(13)
epsilon_mult <- rlnorm(n, meanlog = -shape^2 / 2, sdlog = shape)
good_mult <- rep(0, n)
bad_mult <- -xb^2 * epsilon_mult
mult_result <- classification_summary(good_mult, bad_mult, cutoff, "multiplicative lognormal", NA_real_)

cat("\nMultiplicative lognormal noise\n")
cat("------------------------------\n")
cat("With x_good = 0, good signals are degenerate at zero; bad signals are scaled, not translated.\n")
print(mult_result, row.names = FALSE)

all_results <- rbind(normal_results, skew_result, mult_result)
density_df <- function(x, type, structure) {
  d <- density(x)
  data.frame(z = d$x, density = d$y, type = type, structure = structure)
}

normal_density <- rbind(
  density_df(0.5 * standard_good, "Good type", "Additive normal"),
  density_df(-xb^2 + 0.5 * standard_bad, "Bad type", "Additive normal")
)
normal_density_plot <- ggplot(normal_density, aes(z, density, color = type)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = cutoff, color = "grey35", linetype = "dashed") +
  annotate("label", x = cutoff - 0.45, y = max(normal_density$density) * 0.85,
           label = "midpoint cutoff", size = 3.4, label.size = 0, fill = "white") +
  scale_color_manual(values = custom_palette[c(3, 5)], name = NULL) +
  labs(
    title = "Symmetric Additive Noise Supports the Midpoint Rule",
    subtitle = "Good and bad signal distributions are translations of one another",
    x = "Signal z",
    y = "Density"
  ) +
  custom_theme_scale()
save_png("fearon99_noise_additive_normal_density.png", normal_density_plot)

normal_errors <- rbind(
  data.frame(sigma = factor(normal_results$sigma), error = "Bad reelected",
             rate = normal_results$bad_reelected_false_positive),
  data.frame(sigma = factor(normal_results$sigma), error = "Good removed",
             rate = normal_results$good_removed_false_negative)
)
error_plot <- ggplot(normal_errors, aes(sigma, rate, fill = error)) +
  geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = custom_palette[c(5, 3)], name = NULL) +
  labs(
    title = "Normal-Noise Mistakes Rise with Noise",
    subtitle = "The midpoint cutoff remains fixed, but overlap increases as sigma grows",
    x = "Signal noise sigma",
    y = "Error rate"
  ) +
  coord_cartesian(ylim = c(0, 0.55), expand = FALSE) +
  custom_theme_scale()
save_png("fearon99_noise_normal_error_rates.png", error_plot)

skew_density <- rbind(
  density_df(good_skew, "Good type", "Additive shifted lognormal"),
  density_df(bad_skew, "Bad type", "Additive shifted lognormal")
)
skew_density_plot <- ggplot(skew_density, aes(z, density, color = type)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = cutoff, color = "grey35", linetype = "dashed") +
  scale_color_manual(values = custom_palette[c(3, 5)], name = NULL) +
  labs(
    title = "Skewed Additive Noise Breaks Error Balance",
    subtitle = "The midpoint cutoff no longer equalises false positives and false negatives",
    x = "Signal z",
    y = "Density"
  ) +
  custom_theme_scale()
save_png("fearon99_noise_skewed_density.png", skew_density_plot)

mult_density <- density_df(bad_mult, "Bad type", "Multiplicative lognormal")
mult_density_plot <- ggplot(mult_density, aes(z, density)) +
  geom_line(aes(color = "Bad-type density"), linewidth = 1) +
  geom_vline(aes(xintercept = 0, color = "Good type: z = 0"), linewidth = 1) +
  geom_vline(xintercept = cutoff, color = "grey35", linetype = "dashed") +
  annotate("label", x = cutoff - 0.28, y = max(mult_density$density) * 0.82,
           label = "midpoint cutoff", size = 3.4, label.size = 0, fill = "white") +
  scale_color_manual(values = c("Bad-type density" = custom_palette[5],
                                "Good type: z = 0" = custom_palette[3]),
                     name = NULL) +
  labs(
    title = "Multiplicative Noise Scales Bad-Type Signals",
    subtitle = "With x_good = 0, good signals are degenerate rather than translated",
    x = "Signal z",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(-1.25, 0.10), expand = FALSE) +
  custom_theme_scale()
save_png("fearon99_noise_multiplicative_density.png", mult_density_plot)

cat("\nPNG plots saved to:", output_dir, "\n")
