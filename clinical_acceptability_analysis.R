###############################################################################
# Clinical validation study — final analysis
#
# Data: raw_ratings_long.csv (long format)
#   query_idx, rater, rating, stratum, is_third_rater, is_in_sensitivity_analysis
#
# Analyses (run for each stratum: with_query_id, without_query_id, and combined):
#   (1) Inter-rater reliability: ICC(A,2), two-way, absolute agreement,
#       average measures, on the first two raters (is_third_rater == FALSE).
#   (2) Final score per query = median of the 2 or 3 ratings. Publication-ready
#       histogram.
#   (3) Dichotomize (acceptable = score > 3 
#       vs. indeterminate or unacceptable (i.e., not acceptable) = score <= 3).
#       One-sided single-proportion test, H0: p <= 0.90, alpha = 0.05.
#       Two-sided 95% Wilson score CI for the proportion acceptable.
#   (4) Sensitivity analysis: repeat (2)-(3) excluding the queries whose median
#       is exactly 3 (i.e., keep is_in_sensitivity_analysis == TRUE).
###############################################################################

# ---- Libraries ----
library(tidyverse)   # dplyr, tidyr, ggplot2, readr
library(irr)         # icc()  -> ICC(A,2)
library(binom)       # binom.confint() -> Wilson score interval

# ---- Configuration ----
INPUT_CSV     <- "raw_ratings_long.csv"
OUTPUT_DIR    <- "outputs"
OUTPUT_MD     <- file.path(OUTPUT_DIR, "analysis_results.md")
NULL_P        <- 0.90    # H0 threshold for proportion test
ALPHA         <- 0.05
CONF_ICC      <- 0.95    # two-sided CI for ICC
CONF_WILSON   <- 0.95    # two-sided CI for the proportion

EXPECTED_N <- c(with_query_id = 184L, without_query_id = 39L)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---- Start markdown logging (cat()/print() text) ----
report_con <- file(OUTPUT_MD, open = "wt")
sink(report_con, split = TRUE)

# also capture warnings/messages to the same file.
sink(report_con, type = "message")

on.exit({
  # Close message sink first (if active), then output sink.
  while (sink.number(type = "message") > 0) sink(type = "message")
  while (sink.number() > 0) sink()
  close(report_con)
}, add = TRUE)

cat("# Clinical Validation Final Analysis\n\n")
cat("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", sep = "")
cat("Input file: ", INPUT_CSV, "\n\n", sep = "")

# ---- Load data ----
ratings <- read_csv(INPUT_CSV, show_col_types = FALSE) %>%
  mutate(
    is_third_rater             = as.logical(is_third_rater),
    is_in_sensitivity_analysis = as.logical(is_in_sensitivity_analysis)
  )

# Small helper for markdown section headers
hr <- function(title) {
  cat("\n## ", title, "\n\n", sep = "")
}

###############################################################################
# (1) ICC(A,2) — two-way, absolute agreement, average measures
#     Uses only the first two raters (is_third_rater == FALSE).
###############################################################################
compute_icc <- function(df_stratum, stratum_name) {
  hr(paste0("[", stratum_name, "] (1) Inter-rater reliability: ICC(A,2)"))

  # Reshape to one row per query, two columns (the two non-third raters).
  # Raters differ across queries, so we assign them to generic positions 1/2
  # (ordered by rater id for reproducibility).
  icc_wide <- df_stratum %>%
    filter(!is_third_rater) %>%
    group_by(query_idx) %>%
    arrange(rater, .by_group = TRUE) %>%
    mutate(rater_pos = row_number()) %>%
    ungroup() %>%
    filter(rater_pos <= 2) %>%
    select(query_idx, rater_pos, rating) %>%
    pivot_wider(names_from = rater_pos, values_from = rating,
                names_prefix = "rater_")

  # Sanity check: every query must contribute exactly two ratings.
  incomplete <- icc_wide %>% filter(is.na(rater_1) | is.na(rater_2))
  if (nrow(incomplete) > 0) {
    warning(sprintf("%d queries do not have exactly two non-third-rater ratings.",
                    nrow(incomplete)))
  }

  icc_mat <- icc_wide %>% select(rater_1, rater_2) %>% as.data.frame()

  res <- icc(icc_mat,
             model = "twoway",
             type  = "agreement",
             unit  = "average",
             conf.level = CONF_ICC)

  cat(sprintf("N queries (pairs of ratings): %d\n", nrow(icc_mat)))
  cat(sprintf("ICC(A,2) = %.3f  (%.0f%% CI: %.3f to %.3f)\n",
              res$value, 100 * CONF_ICC, res$lbound, res$ubound))
  cat(sprintf("F(%d, %.1f) = %.2f, p = %.4g\n",
              res$df1, res$df2, res$Fvalue, res$p.value))
  print(res)

  invisible(res)
}

###############################################################################
# (2)+(3) Final scores, histogram, dichotomization, proportion test, Wilson CI
#         Reused for both the primary and the sensitivity analyses.
###############################################################################
score_and_test <- function(df_subset, stratum_name, label) {
  hr(paste0("[", stratum_name, "] ", label,
            " - (2) final scores  (3) dichotomization & proportion test"))

  # --- (2) Final score per query = median of all ratings ---
  scores <- df_subset %>%
    group_by(query_idx) %>%
    summarise(
      n_ratings  = n(),
      raw_median = median(rating),
      is_sens    = first(is_in_sensitivity_analysis),
      .groups    = "drop"
    ) %>%
    mutate(
      median_is_3 = raw_median == 3,
      final_score = raw_median
    )

  n_median_3 <- sum(scores$median_is_3)
  cat(sprintf("Queries analysed: %d\n", nrow(scores)))
  cat(sprintf("Ratings per query: %s\n",
              paste(sprintf("%d x %d",
                            table(scores$n_ratings),
                            as.integer(names(table(scores$n_ratings)))),
                    collapse = ", ")))
  cat(sprintf("Median == 3 (not acceptable): %d query(ies)\n", n_median_3))

  # Consistency check against the pre-computed flag (primary analysis only).
  if (label == "Primary") {
    flag_false <- scores %>% filter(!is_sens) %>% pull(query_idx)
    median3_ids <- scores %>% filter(median_is_3) %>% pull(query_idx)
    if (setequal(flag_false, median3_ids)) {
      cat("Check OK: median == 3 queries match is_in_sensitivity_analysis == FALSE.\n")
    } else {
      warning("Mismatch: median == 3 queries do NOT match is_in_sensitivity_analysis == FALSE.")
    }
  }

  # Distribution of final scores
  cat("\nFinal-score distribution:\n")
  print(scores %>% count(final_score) %>%
          mutate(pct = round(100 * n / sum(n), 1)))

  # --- Publication-ready histogram (ggplot) ---
  score_levels <- sort(unique(scores$final_score))
  plot_df <- scores %>%
    mutate(
      final_score = factor(final_score, levels = score_levels),
      category    = if_else(as.numeric(as.character(final_score)) > 3,
                            "Acceptable (> 3)", "Not acceptable (<= 3)")
    )

  p <- ggplot(plot_df, aes(x = final_score, fill = category)) +
    geom_bar(colour = "grey20", width = 0.85) +
    geom_text(stat = "count", aes(label = after_stat(count)),
              vjust = -0.4, size = 3.5) +
    scale_fill_manual(values = c("Acceptable (> 3)"    = "#2c7fb8",
                                 "Not Acceptable (<= 3)" = "#d95f0e")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    labs(
      title    = sprintf("Distribution of final scores (%s)", tolower(label)),
      subtitle = sprintf("N = %d queries", nrow(scores)),
      x = "Final score",
      y = "Number of queries",
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )

  out_png <- file.path(
    OUTPUT_DIR,
    sprintf("histogram_%s_%s.png", stratum_name, tolower(label))
  )
  ggsave(out_png, p, width = 7, height = 5, dpi = 300)
  cat(sprintf("Histogram saved to: %s\n", out_png))

  # --- (3) Dichotomize + one-sided proportion test + Wilson CI ---
  scores <- scores %>% mutate(acceptable = final_score > 3)

  n_total  <- nrow(scores)
  n_accept <- sum(scores$acceptable)
  p_hat    <- n_accept / n_total

  # One-sided single-proportion SCORE test (H0: p <= 0.90; H1: p > 0.90),
  # without continuity correction.
  pt <- prop.test(x = n_accept, n = n_total, p = NULL_P,
                  alternative = "greater", correct = FALSE)

  # Two-sided 95% Wilson score interval for the proportion acceptable.
  wil <- binom.confint(x = n_accept, n = n_total,
                       conf.level = CONF_WILSON, methods = "wilson")

  cat("\n--- Dichotomized outcome (acceptable = score > 3) ---\n")
  cat(sprintf("Acceptable: %d / %d = %.1f%%\n",
              n_accept, n_total, 100 * p_hat))
  cat(sprintf("Not Acceptable: %d / %d = %.1f%%\n",
              n_total - n_accept, n_total, 100 * (1 - p_hat)))
  cat(sprintf("\nOne-sided proportion test (H0: p <= %.2f, alpha = %.2f):\n",
              NULL_P, ALPHA))
  cat(sprintf("  chi-squared = %.3f, df = %d, one-sided p = %.4g  -> %s\n",
              pt$statistic, pt$parameter, pt$p.value,
              ifelse(pt$p.value < ALPHA,
                     "reject H0 (proportion significantly > 0.90)",
                     "fail to reject H0")))
  cat(sprintf("  95%% Wilson score CI (two-sided): %.3f to %.3f\n",
              wil$lower, wil$upper))

  invisible(list(scores = scores, prop_test = pt, wilson = wil, plot = p))
}

###############################################################################
# Main: loop over strata
###############################################################################
strata <- c("with_query_id", "without_query_id")
results <- list()

for (s in strata) {
  df_s <- ratings %>% filter(stratum == s)
  n_q  <- n_distinct(df_s$query_idx)

  hr(sprintf("STRATUM: %s", s))
  cat(sprintf("Queries observed: %d (expected: %d)\n", n_q, EXPECTED_N[[s]]))
  if (n_q != EXPECTED_N[[s]]) {
    warning(sprintf("Stratum '%s' has %d queries; expected %d.",
                    s, n_q, EXPECTED_N[[s]]))
  }

  # (1) ICC
  icc_res <- compute_icc(df_s, s)

  # (2)-(3) Primary analysis (all queries)
  primary <- score_and_test(df_s, s, "Primary")

  # (4) Sensitivity analysis: exclude the median == 3 queries
  df_sens <- df_s %>% filter(is_in_sensitivity_analysis)
  sensitivity <- score_and_test(df_sens, s, "Sensitivity")

  results[[s]] <- list(icc = icc_res,
                       primary = primary,
                       sensitivity = sensitivity)
}