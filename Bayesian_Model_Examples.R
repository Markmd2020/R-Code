#Bayesian Models Examples#
#Load libraries
library(rstan)
library(lme4)
library(brms)
library(rstanarm)
library(tidybayes)
library(loo)
library(bayesplot)
library(ggdist)
library(ggplot2)

# Simulate some data
set.seed(135)
N <- 50; x <- rnorm(N); y <- 2 + 1.5 * x + rnorm(N)
stan_data <- list(N = N, x = x, y = y)

stan_code <- "
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  // Weakly informative priors
  alpha ~ normal(0, 10);
  beta  ~ normal(0, 10);
  sigma ~ exponential(1);
  
  // Likelihood
  y ~ normal(alpha + beta * x, sigma);
}
"


# Fit model
fit <- stan(model_code = stan_code,
            data = stan_data,
            chains = 4,
            iter = 2000,
            warmup = 1000)
print(fit, pars = c("alpha", "beta", "sigma"))


#brms Example
# Using the classic sleepstudy dataset (reaction times, sleep deprivation)
data("sleepstudy", package = "lme4")
fit_brms <- brm(
  Reaction ~ Days + (Days | Subject),  # random slopes and intercepts
  data = sleepstudy,
  family = gaussian(),
  prior = c(
    prior(normal(250, 50), class = Intercept),
    prior(normal(10, 10),  class = b),
    prior(exponential(1),  class = sigma)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 135
)
summary(fit_brms)
plot(fit_brms)
# Posterior predictive check
pp_check(fit_brms, ndraws = 50)

#Model Evaluation
#Calculate Gelman Rubin Statistic 
summary(fit_brms)$fixed #This is given by Rhat
#Values of 1 indicate convergence and 1.1 is a warning sign

#Effective sample size accounts for the effect of autocorrelation
#Bulk_ESS and Tail_ESS should be higher than 400

#Trace Plots should look like fuzzy caterpillars
plot(fit_brms, type = "trace")

# Bayesian logistic regression — drop-in for glm()
fit_rstanarm <- stan_glm(
  am ~ wt + hp,
  data = mtcars,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),       # weakly informative default
  prior_intercept = normal(0, 10),
  chains = 4,
  iter = 2000,
  seed = 135
)
posterior_interval(fit_rstanarm, prob = 0.95)

#Clinical Trial Example
# Beta-Binomial conjugate: analytical posterior
# Prior: Beta(5, 5); Likelihood: Binomial(72 successes, 120 trials)
# Posterior: Beta(5 + 72, 5 + 48) = Beta(77, 53)
alpha_post <- 5 + 72
beta_post  <- 5 + 48
# Posterior mean and 95% credible interval
post_mean <- alpha_post / (alpha_post + beta_post)
ci_95 <- qbeta(c(0.025, 0.975), alpha_post, beta_post)
prob_better_than_50 <- 1 - pbeta(0.5, alpha_post, beta_post)
cat("Posterior mean:", round(post_mean, 3), "\n")
cat("95% Credible Interval:", round(ci_95, 3), "\n")
cat("P(response rate > 0.5):", round(prob_better_than_50, 4), "\n")

#Black-Litterman Model
# Example: model time-varying volatility with a Student-t likelihood
# (heavy-tailed to accommodate financial return outliers)
set.seed(135)
returns <- c(rnorm(200, 0, 0.01), rnorm(50, 0, 0.04))  # simulated
fit_vol <- brm(
  returns ~ 1,
  family = student(),            # Student-t handles fat tails
  prior = c(
    prior(normal(0, 0.05), class = Intercept),
    prior(exponential(100), class = sigma),
    prior(gamma(2, 0.1), class = nu)   # degrees of freedom
  ),
  data = data.frame(returns = returns),
  chains = 4, iter = 2000, seed = 135
)
summary(fit_vol)

#Multilevel model example
# Exam scores across 30 schools (simulated)
set.seed(135)
n_schools <- 30; n_students <- 20
schools <- rep(1:n_schools, each = n_students)
ses <- rnorm(n_schools * n_students)
score <- 60 + 5 * ses + rnorm(n_schools)[schools] * 3 + rnorm(n_schools * n_students, sd = 8)
df <- data.frame(score = score, ses = ses, school = factor(schools))
fit_school <- brm(
  score ~ ses + (1 | school),
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(60, 20), class = Intercept),
    prior(normal(0, 10),  class = b),
    prior(exponential(1), class = sigma),
    prior(exponential(1), class = sd)
  ),
  chains = 4, iter = 2000, cores = 4, seed = 42
)

# Extract school-level random effects with uncertainty
ranef(fit_school)

#Model Comparison
fit1 <- brm(Reaction ~ Days, data = sleepstudy, chains = 4, seed = 135)
fit2 <- brm(Reaction ~ Days + (Days | Subject), data = sleepstudy,chains = 4, seed = 135)
loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo_compare(loo1, loo2) 
#A large positive ELPD (log likelihood) difference would indicate that the complex model
#improves predictions

#Another approach for Trace Plots
mcmc_trace(as.array(fit_brms), pars = c("b_Intercept", "b_Days"))
mcmc_rank_overlay(as.array(fit_brms))  # rank plots: more sensitive than raw traces

#Posterior Predictive Checks
pp_check(fit_brms, ndraws = 100)            # density overlay
pp_check(fit_brms, type = "stat", stat = "mean")   # check mean
pp_check(fit_brms, type = "scatter_avg")    # observed vs predicted

# Tidy draws from brms model
fit_brms |>
  spread_draws(b_Days) |>
  ggplot(aes(x = b_Days)) +
  stat_halfeye(.width = c(0.80, 0.95)) +
  labs(title = "Posterior distribution: effect of sleep deprivation on reaction time",
       x = "Change in reaction time per day (ms)",
       y = "Posterior density") 

## 7. A practical Bayesian workflow checklist

#1. Define your estimand: what exactly are you trying to learn?
#2. Choose a prior: start with weakly informative; document your reasoning.
#3. Specify the likelihood: select an appropriate sampling distribution.
#4. Fit the model (brms / rstan / rstanarm / rjags).
#5. Check convergence: R̂ < 1.01, ESS > 400, trace plots look stable.
#6. Posterior predictive check: does the model reproduce key data features?
#7. Interpret the posterior: means, credible intervals, probability statements.
#8. Perform sensitivity analysis: how much do results change under different priors?
#9. Communicate with uncertainty: never report just a point estimate
