# for each query (prompt), generate k responses (runs)
# get a semantic repeatability score for each query
# semantic repeatability score = average of pairwise cosine similarity across runs for a query, scaled to be [0, 1]
# source: https://doi.org/10.1101/2025.08.06.25333170


# libraries
library(ggplot2)
library(tidyverse)
library(MKpower)

# Power calculation for multiple null and alternative hypotheses
set.seed(88) # for simulation reproducibility

# Define parameters
upper_bound <- 0.99
lower_bound <- 0.80
expected_mean <- 0.95




#---------------#
#---- LOGIT ----#
#---------------#


# Null hypotheses and alternative hypotheses to test
null_hypotheses <- c(0.90, 0.925, 0.950)
alt_hypotheses <- c(0.925, 0.950, 0.975)

# Function to calculate logit SD based on bounds and mean
calc_logit_sd <- function(mean_val, upper_bound, lower_bound) {
  logit_mean <- qlogis(mean_val)
  logit_upper <- qlogis(upper_bound)
  logit_lower <- qlogis(lower_bound)
  logit_sd1 <- (logit_upper - logit_mean) / 2
  logit_sd2 <- (logit_mean - logit_lower) / 2
  return(mean(c(logit_sd1, logit_sd2)))
}

# Sample sizes to test
ns <- seq(5, 80, by = 5)

# Create a results data frame to store all combinations
results <- expand.grid(
  n = ns,
  null_hyp = null_hypotheses,
  alt_hyp = alt_hypotheses,   # expected score not alternative hypothesis
  power_ttest = NA,
  power_wilcox = NA,
  power_backt = NA,
  mean_CI_width = NA,
  max_CI_width = NA,
  mean_estimate = NA
)

# Main simulation loop
boots <- 10000

for (row in 1:nrow(results)) {
  n_queries <- results$n[row]
  null_val <- results$null_hyp[row]
  alt_val <- results$alt_hyp[row]

  # Calculate logit SD based on expected score
  logit_expected_sd <- calc_logit_sd(alt_val, upper_bound, lower_bound)
  logit_expected_mean <- qlogis(alt_val)
  logit_null <- qlogis(null_val)

  # Initialize storage for this combination
  sig <- rep(NA, boots)
  CI_width <- rep(NA, boots)
  CI_points <- rep(NA, boots)
  wils <- rep(NA, boots)
  backts <- rep(NA, boots)

  for (i in 1:boots) {
    # Sample from the distribution
    logit_scores <- rnorm(n_queries, logit_expected_mean, logit_expected_sd)

    # One-sided t-test (logit scale)
    test <- t.test(logit_scores, alternative = "greater", mu = logit_null)
    sig[i] <- test$p.value < 0.05

    # Two-sided confidence interval
    test <- t.test(logit_scores, mu = logit_null)
    logit_CI_lower <- test$conf.int[1]
    logit_CI_upper <- test$conf.int[2]
    logit_point <- test$estimate

    # Back-transform to probability space
    CI_lower <- plogis(logit_CI_lower)
    CI_upper <- plogis(logit_CI_upper)
    CI_point <- plogis(logit_point)

    # CI width
    CI_width[i] <- CI_upper - CI_lower
    CI_points[i] <- CI_point

    # Back-transformed scores
    scores <- plogis(logit_scores)

    # Wilcoxon test
    wil <- wilcox.test(scores, alternative = "greater", mu = null_val)
    wils[i] <- wil$p.value < 0.05

    # T-test on back-transformed data
    backt <- t.test(scores, alternative = "greater", mu = null_val)
    backts[i] <- backt$p.value < 0.05
  }

  # Store results
  results$power_ttest[row] <- mean(sig)
  results$power_wilcox[row] <- mean(wils)
  results$power_backt[row] <- mean(backts)
  results$mean_CI_width[row] <- mean(CI_width)
  results$max_CI_width[row] <- quantile(CI_width, 0.99)
  results$mean_estimate[row] <- mean(CI_points)

  # Print progress
  cat("Completed: H0 =", null_val, ", H1 =", alt_val, ", n =", n_queries, "\n")
}

# View results
head(results, 15)


# get CI width and power
results |>
  filter(n == 20, null_hyp == 0.9, alt_hyp == 0.95)


# Create plots for each null hypothesis
# t-test in logit space (power_ttest)
for (null_val in null_hypotheses) {
  p <- results %>%
    filter(null_hyp == null_val) %>%
    ggplot(aes(x = n, y = power_ttest, color = as.factor(alt_hyp))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(limits = c(0, max(ns) + 5)) +
    labs(
      title = paste0("H0: score ≤ ", null_val, "; H1: score > ", null_val),
      x = "Sample Size",
      y = "Power",
      color = "Expected Semantic Repeatability Score"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  print(p)
}

# Define the four scenarios
scenarios <- list(
  list(null = 0.90, alt = 0.95),
  list(null = 0.925, alt = 0.95),
  list(null = 0.925, alt = 0.975),
  list(null = 0.95, alt = 0.975)
)

# Create plots for each scenario
plot_list <- list()

for (i in seq_along(scenarios)) {
  null_val <- scenarios[[i]]$null
  alt_val <- scenarios[[i]]$alt

  p <- results %>%
    filter(null_hyp == null_val, alt_hyp == alt_val) %>%
    pivot_longer(cols = starts_with("power_"),
                 names_to = "method",
                 values_to = "power") %>%
    mutate(method = case_when(
      method == "power_ttest" ~ "t-test (logit)",
      method == "power_wilcox" ~ "Wilcoxon",
      method == "power_backt" ~ "t-test (back-transformed)"
    )) %>%
    ggplot(aes(x = n, y = power, color = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(limits = c(0, max(ns) + 5)) +
    labs(
      title = paste0("H0: score ≤ ", null_val, "; H1: score > ", null_val, "; expected score = ", alt_val),
      x = "Sample Size",
      y = "Power",
      color = "Test Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  plot_list[[i]] <- p
}

# View all four plots
plot_list[[1]]
plot_list[[2]]
plot_list[[3]]
plot_list[[4]]
