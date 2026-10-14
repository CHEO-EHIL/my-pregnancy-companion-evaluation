# libraries
library(pwr)
library(tidyverse)
library(binom)


#####################
#### Background  ####
#####################


# Study design: pairs of reviewers rate on 5-point scale.
# Disagreements resolved via input from third reviewer.
# End result is a single gold-standard rating on 5-point scale
# This rating is then dichotomized (acceptable vs. unacceptable)




#####################
####   Power     ####
#####################

# we use one sample proportions test (pwr.p.test)
# map over multiple parameters
params <- expand.grid(
  expected_p = seq(0.91, 0.99, by=0.01),
  threshold_p = seq(0.90, 0.95, by=0.01),
  power = seq(0.75, 0.99, by = 0.01),
  alpha = 0.05,
  alt = c("greater", "two.sided")
)

# function
pwr_calc <- function(exp_p, thresh_p, pow, sig_level, alte) {

  if (exp_p > thresh_p) {
    effect_size <- ES.h(exp_p, thresh_p)

    pwr_res <- tryCatch({
       pwr.p.test(
        h = effect_size,
        sig.level = sig_level,
        power = pow,
        alternative = paste(alte)
      )
    }, error = function(e) {
      return(NA)
    })

    if (is.list(pwr_res)) {
      temp_n <- pwr_res$n
    } else {
      temp_n <- NA
    }

  } else {
    temp_n <- NA
  }

  return(temp_n)
}

# Calculate effect sizes and sample sizes
results <- mapply(
  pwr_calc
  , params$expected_p
  , params$threshold_p
  , params$power
  , params$alpha
  , params$alt
)
params$sample_size <- results


# power curves, one-sided
params |>
  filter(threshold_p == 0.90, expected_p > 0.93, alt == "greater") |>
  ggplot(aes(x = sample_size, y = power, color = as.factor(expected_p))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 500)) +
  # facet_wrap(~threshold_p, scales = "free") +
  labs(
    title = "H0: p ≤ 0.90; H1: p > 0.90",
    x = "Sample Size",
    y = "Power",
    color = "Expected Proportion (p)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

