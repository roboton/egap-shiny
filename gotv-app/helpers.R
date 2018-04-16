# Helper functions for GOTV app
library(estimatr)
library(knitr)
library(ggplot2)
library(dplyr)
library(shiny)

# function that takes inputs and makes data
make_data <- function(n_Z1,
                      n_Z0,
                      n_Z1_contacted,
                      n_Z0_contacted,
                      n_Z1_voted,
                      n_Z0_voted) {
  Y <- rep(
    c(0, 1, 0, 1),
    times = c(
      n_Z1 - n_Z1_voted,
      n_Z1_voted,
      n_Z0 - n_Z0_voted,
      n_Z0_voted
    )
  )
  D <- rep(
    c(1, 0, 0, 1),
    times = c(
      n_Z1_contacted,
      n_Z1 - n_Z1_contacted,
      n_Z0 - n_Z0_contacted,
      n_Z0_contacted
    )
  )
  Z <- rep(c(1, 0), times = c(n_Z1, n_Z0))
  treatment <- rep(c("Treatment", "Control"), times = c(n_Z1, n_Z0))

  data.frame(Y, D, Z, treatment)
}


# Function that returns the summary table
table_maker <- function(dat) {
  dat %>%
    group_by(treatment) %>%
    summarize(
      `Average Outcome` = mean(Y),
      `Percent Contacted` = mean(D) * 100
    )
}

# Function that returns the statistical results
statistical_results <- function(Y, D, Z) {
  itt_fit <- lm_robust(Y ~ Z)
  cace_fit <- iv_robust(Y ~ D | Z)

  contact_rate <- mean(D[Z == 1]) - mean(D[Z == 0])
  itt_hat <- itt_fit$coefficients[2] * 100
  se_itt <- itt_fit$std.error[2] * 100
  cace <- cace_fit$coefficients[2] * 100
  se_cace <- cace_fit$std.error[2] * 100
  ui <- cace_fit$ci.upper[2] * 100
  li <- cace_fit$ci.lower[2] * 100

  p <- itt_fit$p.value[2]

  mu_t <- mean(Y[Z == 1])
  mu_c <- mean(Y[Z == 0])

  power <- power_calculator(
    mu_t = mu_t,
    mu_c = mu_c,
    sigma = sqrt(mu_t * (1 - mu_t)),
    alpha = 0.1,
    N = length(Y)
  )


  data.frame(
    Statistic = c(
      "Contact Rate",
      "Estimated Intent-to-treat Effect",
      "Standard Error of Estimated Intent-to-treat Effect",
      "Estimated Complier Average Causal Effect (CACE)",
      "Standard Error of Estimated CACE",
      "95% Confidence Interval for Estimated CACE",
      "One-tailed p-value",
      "Statistical Power of this Experiment"
    ),
    Value = c(
      sprintf("%.3f", contact_rate),
      sprintf("%.3f", itt_hat),
      sprintf("%.3f", se_itt),
      sprintf("%.3f", cace),
      sprintf("%.3f", se_cace),
      paste0(sprintf("%.3f", li), ", ", sprintf("%.3f", ui)),
      sprintf("%.3f", p),
      sprintf("%.3f", power)
    )
  )
}

# Function that determines statistical power
power_calculator <- function(mu_t, mu_c, sigma, alpha = 0.05, N) {
  lowertail <- (abs(mu_t - mu_c) * sqrt(N)) / (2 * sigma)
  uppertail <- -1 * lowertail
  beta <- pnorm(lowertail - qnorm(1 - alpha / 2), lower.tail = TRUE) +
    1 - pnorm(uppertail - qnorm(1 - alpha / 2), lower.tail = FALSE)
  return(beta)
}

# Helper for checking the datatype
is_binary <- function(x) {
  return(all(x %in% c(0, 1)))
}