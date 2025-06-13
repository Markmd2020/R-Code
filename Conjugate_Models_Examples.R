### Bayesian Sports Models in R: Chapter 6 - Conjugate Priors | Andrew Mack | @Gingfacekillah

# Load libraries | Install required packages prior to loading!
library(ggplot2)        # ggplot: plotting functions
library(tidyverse)      # data wrangling functions


#### 1. Beta-Binomial Model
# Beta-Binomial updating function
update_beta <- function(prior_alpha, prior_beta, successes, trials) {
    posterior_alpha <- prior_alpha + successes
    posterior_beta <- prior_beta + (trials - successes)
    return(list(alpha = posterior_alpha,
                beta = posterior_beta))}

# Flat prior parameters
prior_alpha_flat <- 1
prior_beta_flat <- 1

# Informed prior parameters
prior_alpha_inf <- 4
prior_beta_inf <- 4

# Observed data
successes <- 6
trials <- 10

# Update the distributions
posterior_flat <- update_beta(
    prior_alpha_flat, prior_beta_flat, successes, trials)
posterior_inf <- update_beta(
    prior_alpha_inf, prior_beta_inf, successes, trials)

# Print the updated parameters
cat("Posterior with Flat Priors: Alpha =", posterior_flat$alpha,
    "Beta =", posterior_flat$beta, "\n")
cat("Posterior with Informed Priors: Alpha =", posterior_inf$alpha,
    "Beta =", posterior_inf$beta, "\n")

# Function to generate data for Beta distribution
generate_beta_data <- function(alpha, beta, n = 1000) {
    x <- seq(0, 1, length.out = n)
    y <- dbeta(x, alpha, beta)
    data.frame(x = x, y = y)
}

# Generate data for both distributions
data_flat <- generate_beta_data(posterior_flat$alpha, posterior_flat$beta) %>%
    mutate(type = "Flat Priors")

data_inf <- generate_beta_data(posterior_inf$alpha, posterior_inf$beta) %>%
    mutate(type = "Informed Priors")

# Combine the data
data_combined <- bind_rows(data_flat, data_inf)

# Plot the data
ggplot(data_combined, aes(x = x, y = y, color = type)) +
    geom_line(size = 1) +
    labs(title = "Posterior Beta Distributions",
         x = "Probability",
         y = "Density",
         color = "Prior Type") +
    theme_minimal()


#### 2. Gamma-Poisson Model
# Gamma-Poisson updating function
update_gamma <- function(prior_alpha, prior_beta, observed_sum, n) {
    posterior_alpha <- prior_alpha + observed_sum
    posterior_beta <- prior_beta + n
    return(list(alpha = posterior_alpha,
                beta = posterior_beta))}

# Flat prior parameters
prior_alpha_flat <- 1
prior_beta_flat <- 1

# Informed prior parameters
prior_alpha_inf <- 6
prior_beta_inf <- 2

# Observed data
observed_goals <- c(3, 5, 2, 1, 6, 4, 3)
sum_goals <- sum(observed_goals)
n <- length(observed_goals)

# Update the distributions
posterior_flat <- update_gamma(prior_alpha_flat,
                               prior_beta_flat, sum_goals, n)
posterior_inf <- update_gamma(prior_alpha_inf,
                              prior_beta_inf, sum_goals, n)

# Print the updated parameters
cat("Posterior with Flat Priors: Alpha =", posterior_flat$alpha,
    "Beta =", posterior_flat$beta, "\n")
cat("Posterior with Informed Priors: Alpha =", posterior_inf$alpha,
    "Beta =", posterior_inf$beta, "\n")

# Function to generate data for Gamma distribution
generate_gamma_data <- function(alpha, beta, n = 1000) {
    x <- seq(0, max(alpha/beta + 3 * sqrt(alpha/beta^2)), length.out = n)
    y <- dgamma(x, shape = alpha, rate = beta)
    data.frame(x = x, y = y)
}

# Generate data for both distributions
data_flat <- generate_gamma_data(posterior_flat$alpha, posterior_flat$beta) %>%
    mutate(type = "Flat Priors")

data_inf <- generate_gamma_data(posterior_inf$alpha, posterior_inf$beta) %>%
    mutate(type = "Informed Priors")

# Combine the data
data_combined <- bind_rows(data_flat, data_inf)

# Plot the data
ggplot(data_combined, aes(x = x, y = y, color = type)) +
    geom_line(size = 1) +
    labs(title = "Posterior Gamma Distributions",
         x = "Rate",
         y = "Density",
         color = "Prior Type") +
    theme_minimal()
