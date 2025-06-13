##############Bayes Rules Examples##############

#Load libraries
library(dplyr)
library(ggplot2)
library(janitor)
library(rstan)
library(rstanarm)
library(bayesrules)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)
library(tidyverse)
library(e1071)

#Set seed to ensure reproducibility
set.seed(135)

###Chapter 2: Bayes Model Example###
# Define possible win probabilities
chess <- data.frame(pi = c(0.2, 0.5, 0.8))

# Define the prior model
prior <- c(0.10, 0.25, 0.65)

#Simulate 1000 values from prior
chess_sim <- sample_n(chess, size = 10000, weight = prior, replace = TRUE)
head(chess_sim)

#Use Binomial Distribution to Simulate Match#
chess_sim1 <- chess_sim %>% 
  mutate(y = rbinom(10000, size = 6, prob = pi))
head(chess_sim1)

#Summarise prior
chess_sim1 %>% 
  tabyl(pi) %>% 
  adorn_totals("row")

# Plot y by pi
ggplot(chess_sim1, aes(x = y)) + 
  stat_count(aes(y = ..prop..)) + 
  facet_wrap(~ pi)

# Focus on simulations with y = 1
win_one <- chess_sim1 %>% 
  filter(y == 1)

# Summarize the posterior approximation
win_one %>% 
  tabyl(pi) %>% 
  adorn_totals("row")

# Plot the posterior approximation
ggplot(win_one, aes(x = pi)) + 
  geom_bar()

###Chapter 3: Beta Binomial Model Examples###

# Plot the Beta(45, 55) prior
plot_beta(45, 55)

#Summarise beta binomial
summarize_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)

#Simulate election outcome with a beta-binomial model
michelle_sim <- data.frame(pi = rbeta(10000, 45, 55)) %>% 
  mutate(y = rbinom(10000, size = 50, prob = pi))

head(michelle_sim)

#Plotting simulation
ggplot(michelle_sim, aes(x = pi, y = y)) + 
  geom_point(aes(color = (y == 30)), size = 0.1)

# Keep only the simulated pairs that match our data
michelle_posterior <- michelle_sim %>% 
  filter(y == 30)

# Plot the remaining pi values
ggplot(michelle_posterior, aes(x = pi)) + 
  geom_density()

#Summarise posterior distribution
michelle_posterior %>% 
  summarize(mean(pi), sd(pi))

###Chapter 4: Sequential Model Examples###

#Retrieve data
data(bechdel, package = "bayesrules")

#Take a sample of 20 movies
bechdel_20 <- bechdel %>% 
  sample_n(20)

#Explore data
bechdel_20 %>% 
  head(3)

#Summarise data
bechdel_20 %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

#Trying different data and examine posteriors
bechdel %>% 
  filter(year == 1991) %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

#Filter for year  2000
bechdel %>% 
  filter(year == 2000) %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

#Filter for year 2013
bechdel %>% 
  filter(year == 2013) %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

##Chapter 5:Conjugate Models##

#Plot beta (1,2)
plot_beta(alpha = 1, beta = 2)

# Plot the Gamma(10, 2) prior
plot_gamma(shape = 10, rate = 2)

#Plot Poisson likelihood
plot_poisson_likelihood(y = c(6, 2, 2, 1), lambda_upper_bound = 10)

#Plot Gamma-Poisson Conjugate Model
plot_gamma_poisson(shape = 10, rate = 2, sum_y = 11, n = 4)

#Summarise Gamma-Poisson Model
summarize_gamma_poisson(shape = 10, rate = 2, sum_y = 11, n = 4)

#Plot nomral prior
plot_normal(mean = 6.5, sd = 0.4)

#Normal-Normal Conjugate Model Example
# Load the data
data(football)
concussion_subjects <- football %>%
  filter(group == "fb_concuss")

#Summarise data
concussion_subjects %>%
  summarize(mean(volume))

#Density Plot
ggplot(concussion_subjects, aes(x = volume)) + 
  geom_density()

#Plot normal likelihood function
plot_normal_likelihood(y = concussion_subjects$volume, sigma = 0.5)

#Plot normal-normal model
plot_normal_normal(mean = 6.5, sd = 0.4, sigma = 0.5,
                   y_bar = 5.735, n = 25)

#Summarise normal-normal model
summarize_normal_normal(mean = 6.5, sd = 0.4, sigma = 0.5,
                        y_bar = 5.735, n = 25)

#Chapter 6:Approximating the Posterior#

#Using  Grid Approximation

# Step 1: Define a grid of 6 pi values
grid_data <- data.frame(pi_grid = seq(from = 0, to = 1, length = 6))

# Step 2: Evaluate the prior & likelihood at each pi
grid_data <- grid_data %>% 
  mutate(prior = dbeta(pi_grid, 2, 2),
         likelihood = dbinom(9, 10, pi_grid))

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = likelihood * prior,
         posterior = unnormalized / sum(unnormalized))

# Confirm that the posterior approximation sums to 1
grid_data %>% 
  summarize(sum(unnormalized), sum(posterior))

# Examine the grid approximated posterior
round(grid_data, 2)

# Plot the grid approximated posterior
ggplot(grid_data, aes(x = pi_grid, y = posterior)) + 
  geom_point() + 
  geom_segment(aes(x = pi_grid, xend = pi_grid, y = 0, yend = posterior))

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)

# A table of the 10000 sample values
post_sample %>% 
  tabyl(pi_grid) %>% 
  adorn_totals("row")

# Histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x = pi_grid)) + 
  geom_histogram(aes(y = ..density..), color = "white") + 
  stat_function(fun = dbeta, args = list(11, 3)) + 
  lims(x = c(0, 1))

#Increase the grid to 101 pi values#
# Step 1: Define a grid of 101 pi values
grid_data  <- data.frame(pi_grid = seq(from = 0, to = 1, length = 101))

# Step 2: Evaluate the prior & likelihood at each pi
grid_data <- grid_data %>% 
  mutate(prior = dbeta(pi_grid, 2, 2),
         likelihood = dbinom(9, 10, pi_grid))

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = likelihood * prior,
         posterior = unnormalized / sum(unnormalized))

ggplot(grid_data, aes(x = pi_grid, y = posterior)) + 
  geom_point() + 
  geom_segment(aes(x = pi_grid, xend = pi_grid, y = 0, yend = posterior))

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)

ggplot(post_sample, aes(x = pi_grid)) + 
  geom_histogram(aes(y = ..density..), color = "white", binwidth = 0.05) + 
  stat_function(fun = dbeta, args = list(11, 3)) + 
  lims(x = c(0, 1))

#Gamma-Poisson Example
plot_gamma_poisson(s = 3, r = 1, sum_y = 10, n = 2, posterior = FALSE)

#Gamma-Poisson Model With Grid Approximation
# Step 1: Define a grid of 501 lambda values
grid_data   <- data.frame(lambda_grid = seq(from = 0, to = 15, length = 501))

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data <- grid_data %>% 
  mutate(prior = dgamma(lambda_grid, 3, 1),
         likelihood = dpois(2, lambda_grid) * dpois(8, lambda_grid))

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = likelihood * prior,
         posterior = unnormalized / sum(unnormalized))


# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)

# Histogram of the grid simulation with posterior pdf 
ggplot(post_sample, aes(x = lambda_grid)) + 
  geom_histogram(aes(y = ..density..), color = "white") + 
  stat_function(fun = dgamma, args = list(13, 3)) + 
  lims(x = c(0, 15))

#Markov Chain For Beta-Binomial Model
# STEP 1: DEFINE the model
bb_model <- "
  data {
    int<lower = 0, upper = 10> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(10, pi);
    pi ~ beta(2, 2);
  }
"

# STEP 2: SIMULATE the posterior
bb_sim <- stan(model_code = bb_model, data = list(Y = 9), 
               chains = 4, iter = 5000*2, seed = 135)

#Extract the first four values of pi from the chain
as.array(bb_sim, pars = "pi") %>% 
  head(4)

#Model diagnostics: Trace plots 
mcmc_trace(bb_sim, pars = "pi", size = 0.1)

# Histogram of the Markov chain values
mcmc_hist(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + 
  ylab("density")

#Gamma-Poisson Example

# STEP 1: DEFINE the model
gp_model <- "
  data {
    int<lower = 0> Y[2];
  }
  parameters {
    real<lower = 0> lambda;
  }
  model {
    Y ~ poisson(lambda);
    lambda ~ gamma(3, 1);
  }
"

# STEP 2: SIMULATE the posterior
gp_sim <- stan(model_code = gp_model, data = list(Y = c(2,8)), 
               chains = 4, iter = 5000*2, seed = 135)

# Trace plots of the 4 Markov chains
mcmc_trace(gp_sim, pars = "lambda", size = 0.1)

# Histogram of the Markov chain values
mcmc_hist(gp_sim, pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Calculate the effective sample size ratio
neff_ratio(bb_sim, pars = c("pi"))

#Trace & Autocorrelation plot
mcmc_trace(bb_sim, pars = "pi")
mcmc_acf(bb_sim, pars = "pi")

# Simulate a thinned MCMC sample
thinned_sim <- stan(model_code = bb_model, data = list(Y = 9), 
                    chains = 4, iter = 5000*2, seed = 135, thin = 10)

# Check out the results
mcmc_trace(thinned_sim, pars = "pi")
mcmc_acf(thinned_sim, pars = "pi")

#Calculate R-Hat ratio for simulation
#Values greater than 1.05 raise some red flag about the stability of the simulation
rhat(bb_sim, pars = "pi") 

#Chapter 7:MCMC under the hood#
mc_tour <- data.frame(mu = rnorm(5000, mean = 4, sd = 0.6))
ggplot(mc_tour, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 15) + 
  stat_function(fun = dnorm, args = list(4, 0.6), color = "blue")

#Metropolis Hastings Algorithm Example#

#Step 1: Create current location
current <- 3

#Step 2:Generate proposal distribution
proposal <- runif(1, min = current - 1, max = current + 1)

#Step 3: Create unormalised proposal plausible distribution
proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)

#Step 4: Generate alpha probability of accepting or moving to the new proposed loccation 
alpha <- min(1, proposal_plaus / current_plaus)

#Step 5: Simulate the next step
next_stop <- sample(c(proposal, current),size = 1, prob = c(alpha, 1-alpha))

#Wrap the algorithm into a function
one_mh_iteration <- function(w, current){
  # STEP 1: Propose the next chain location
  proposal <- runif(1, min = current - w, max = current + w)
  
  # STEP 2: Decide whether or not to go there
  proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
  current_plaus  <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
  alpha <- min(1, proposal_plaus / current_plaus)
  next_stop <- sample(c(proposal, current), 
                      size = 1, prob = c(alpha, 1-alpha))
  
  # Return the results
  return(data.frame(proposal, alpha, next_stop))
}

#Test the function
one_mh_iteration(w = 1, current = 3)

#That function was for one iteration of the MH function
#Do it now for a N iterations
mh_tour <- function(N, w){
  # 1. Start the chain at location 3
  current <- 3
  
  # 2. Initialize the simulation
  mu <- rep(0, N)
  
  # 3. Simulate N Markov chain stops
  for(i in 1:N){    
    # Simulate one iteration
    sim <- one_mh_iteration(w = w, current = current)
    
    # Record next location
    mu[i] <- sim$next_stop
    
    # Reset the current location
    current <- sim$next_stop
  }
  
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), mu))
}

#Implementing the algorithm
mh_simulation_1 <- mh_tour(N = 5000, w = 1)

#Plot outputs of simulations
ggplot(mh_simulation_1, aes(x = iteration, y = mu)) + 
  geom_line()

ggplot(mh_simulation_1, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20) + 
  stat_function(fun = dnorm, args = list(4,0.6), color = "blue")

#Using Metropolis Hastings For Beta Binomial Model
one_iteration <- function(a, b, current){
  # STEP 1: Propose the next chain location
  proposal <- rbeta(1, a, b)
  
  # STEP 2: Decide whether or not to go there
  proposal_plaus <- dbeta(proposal, 2, 3) * dbinom(1, 2, proposal)
  proposal_q     <- dbeta(proposal, a, b)
  current_plaus  <- dbeta(current, 2, 3) * dbinom(1, 2, current)
  current_q      <- dbeta(current, a, b)
  alpha <- min(1, proposal_plaus / current_plaus * current_q / proposal_q)
  next_stop <- sample(c(proposal, current), 
                      size = 1, prob = c(alpha, 1-alpha))
  
  return(data.frame(proposal, alpha, next_stop))
}

#Run N iterations
betabin_tour <- function(N, a, b){
  # 1. Start the chain at location 0.5
  current <- 0.5
  
  # 2. Initialize the simulation
  pi <- rep(0, N)
  
  # 3. Simulate N Markov chain stops
  for(i in 1:N){    
    # Simulate one iteration
    sim <- one_iteration(a = a, b = b, current = current)
    
    # Record next location
    pi[i] <- sim$next_stop
    
    # Reset the current location
    current <- sim$next_stop
  }
  
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), pi))
}

#Implement the MCMC algorithm
betabin_sim <- betabin_tour(N = 5000, a = 1, b = 1)

# Plot the results
ggplot(betabin_sim, aes(x = iteration, y = pi)) + 
  geom_line()
ggplot(betabin_sim, aes(x = pi)) + 
  geom_histogram(aes(y = ..density..), color = "white") + 
  stat_function(fun = dbeta, args = list(3, 4), color = "blue")

#Chapter 8:Posterior Inference & Prediction#

# Load data
data("moma_sample")

#Explore Data
moma_sample %>% 
  group_by(genx) %>% 
  tally()

#Plot beta-binomial model
plot_beta_binomial(alpha = 4, beta = 6, y = 14, n = 100)

#Posterior estimation
# 0.025th & 0.975th quantiles of the Beta(18,92) posterior
qbeta(c(0.025, 0.975), 18, 92)

# 0.25th & 0.75th quantiles of the Beta(18,92) posterior
qbeta(c(0.25, 0.75), 18, 92)

# 0.005th & 0.995th quantiles of the Beta(18,92) posterior
qbeta(c(0.005, 0.995), 18, 92)

# Posterior probability that pi < 0.20
post_prob <- pbeta(0.20, 18, 92)

# Posterior odds
post_odds <- post_prob / (1 - post_prob)

# Prior probability that pi < 0.2
prior_prob <- pbeta(0.20, 4, 6)

# Prior odds
prior_odds <- prior_prob / (1 - prior_prob)

#Using Bayes Factor for inference
BF <- post_odds / prior_odds

#Posterior Simulation

# STEP 1: DEFINE the model
art_model <- "
  data {
    int<lower = 0, upper = 100> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(100, pi);
    pi ~ beta(4, 6);
  }
"

# STEP 2: SIMULATE the posterior
art_sim <- stan(model_code = art_model, data = list(Y = 14), 
                chains = 4, iter = 5000*2, seed = 135)

# Parallel trace plots & density plots
mcmc_trace(art_sim, pars = "pi", size = 0.5) + 
  xlab("iteration")
mcmc_dens_overlay(art_sim, pars = "pi")

# Autocorrelation plot
mcmc_acf(art_sim, pars = "pi")

# Markov chain diagnostics
rhat(art_sim, pars = "pi")
neff_ratio(art_sim, pars = "pi")

# The actual Beta(18, 92) posterior
plot_beta(alpha = 18, beta = 92) + 
  lims(x = c(0, 0.35))

# MCMC posterior approximation
mcmc_dens(art_sim, pars = "pi") + 
  lims(x = c(0,0.35))

tidy(art_sim, conf.int = TRUE, conf.level = 0.95)

# Shade in the middle 95% interval
mcmc_areas(art_sim, pars = "pi", prob = 0.95)

# Store the 4 chains in 1 data frame
art_chains_df <- as.data.frame(art_sim, pars = "lp__", include = FALSE)
dim(art_chains_df)

# Calculate posterior summaries of pi
art_chains_df %>% 
  summarize(post_mean = mean(pi), 
            post_median = median(pi),
            post_mode = sample_mode(pi),
            lower_95 = quantile(pi, 0.025),
            upper_95 = quantile(pi, 0.975))

# Tabulate pi values that are below 0.20
art_chains_df %>% 
  mutate(exceeds = pi < 0.20) %>% 
  tabyl(exceeds)

# Predict a value of Y' for each pi value in the chain
art_chains_df <- art_chains_df %>% 
  mutate(y_predict = rbinom(length(pi), size = 20, prob = pi))

# Check it out
art_chains_df %>% 
  head(3)

# Plot the 20,000 predictions
ggplot(art_chains_df, aes(x = y_predict)) + 
  stat_count()

#Summarise posterior distribution
art_chains_df %>% 
  summarize(mean = mean(y_predict),
            lower_80 = quantile(y_predict, 0.1),
            upper_80 = quantile(y_predict, 0.9))

#Chapter 9:Simple Normal Regression#

#Plotting priors
plot_normal(mean = 5000, sd = 1000) + 
  labs(x = "beta_0c", y = "pdf")
plot_normal(mean = 100, sd = 40) + 
  labs(x = "beta_1", y = "pdf")
plot_gamma(shape = 1, rate = 0.0008) + 
  labs(x = "sigma", y = "pdf")

#Posterior Simulation
# Load and plot data
data(bikes)
ggplot(bikes, aes(x = temp_feel, y = rides)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

#Simulate using Stan GLM
bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40), 
                       prior_aux = exponential(0.0008),
                       chains = 4, iter = 5000*2, seed = 135)

# Effective sample size ratio and Rhat
neff_ratio(bike_model)
rhat(bike_model)

# Trace plots of parallel chains
mcmc_trace(bike_model, size = 0.1)

# Density plots of parallel chains
mcmc_dens_overlay(bike_model)

#Simulation using Stan 

# STEP 1: DEFINE the model
stan_bike_model <- "
  data {
    int<lower = 0> n;
    vector[n] Y;
    vector[n] X;
  }
  parameters {
    real beta0;
    real beta1;
    real<lower = 0> sigma;
  }
  model {
    Y ~ normal(beta0 + beta1 * X, sigma);
    beta0 ~ normal(-2000, 1000);
    beta1 ~ normal(100, 40);
    sigma ~ exponential(0.0008);
  }
"

# STEP 2: SIMULATE the posterior
stan_bike_sim <- 
  stan(model_code = stan_bike_model, 
       data = list(n = nrow(bikes), Y = bikes$rides, X = bikes$temp_feel), 
       chains = 4, iter = 5000*2, seed = 84735)

# Posterior summary statistics
tidy(bike_model, effects = c("fixed", "aux"),conf.int = TRUE, conf.level = 0.80)

# Store the 4 chains for each parameter in 1 data frame
bike_model_df <- as.data.frame(bike_model)

# Check it out
nrow(bike_model_df)

head(bike_model_df, 3)

# 50 simulated model lines
bikes %>%
  add_fitted_draws(bike_model, n = 50) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.15) + 
  geom_point(data = bikes, size = 0.05)

# Tabulate the beta_1 values that exceed 0
bike_model_df %>% 
  mutate(exceeds_0 = temp_feel > 0) %>% 
  tabyl(exceeds_0)

# Simulate four sets of data
bikes %>%
  add_predicted_draws(bike_model, n = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw), size = 0.2) + 
  facet_wrap(~ .draw)

#Posterior predictive model
first_set <- head(bike_model_df, 1)

mu <- first_set$`(Intercept)` + first_set$temp_feel * 75

#Assess sampling variability
y_new <- rnorm(1, mean = mu, sd = first_set$sigma)

# Predict rides for each parameter set in the chain
predict_75 <- bike_model_df %>% 
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))

head(predict_75, 3)

# Construct 80% posterior credible intervals
predict_75 %>% 
  summarize(lower_mu = quantile(mu, 0.025),
            upper_mu = quantile(mu, 0.975),
            lower_new = quantile(y_new, 0.025),
            upper_new = quantile(y_new, 0.975))

# Plot the posterior model of the typical ridership on 75 degree days
ggplot(predict_75, aes(x = mu)) + 
  geom_density()

# Plot the posterior predictive model of tomorrow's ridership
ggplot(predict_75, aes(x = y_new)) + 
  geom_density()

# Simulate a set of predictions with rstanarm built in function
shortcut_prediction <- posterior_predict(bike_model, newdata = data.frame(temp_feel = 75))

# Construct a 95% posterior credible interval
posterior_interval(shortcut_prediction, prob = 0.95)

# Plot the approximate predictive model
mcmc_dens(shortcut_prediction) + 
  xlab("predicted ridership on a 75 degree day")

#Sequential Regression Modelling
#Data is fed into the model at different phases
bikes %>% 
  select(date, temp_feel, rides) %>% 
  head(3)

#Define phases
phase_1 <- bikes[1:30, ]
phase_2 <- bikes[1:60, ]
phase_3 <- bikes

#Create model
bike_model_default <- stan_glm(
  rides ~ temp_feel, data = bikes, 
  family = gaussian,
  prior_intercept = normal(5000, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

#Prior Summary
prior_summary(bike_model_default)

# Perform a prior simulation 
bike_default_priors <- update(bike_model_default, prior_PD = TRUE)

# 200 prior model lines
bikes %>%
  add_fitted_draws(bike_default_priors, n = 200) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.15)

# 4 prior simulated datasets
bikes %>%
  add_predicted_draws(bike_default_priors, n = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw)) + 
  facet_wrap(~ .draw) 

#Chapter 10: Evaluating Regression Models#

#Visualise data
ggplot(bikes, aes(y = rides, x = temp_feel)) + 
  geom_point(size = 0.2) + 
  geom_smooth(method = "lm", se = FALSE)

#Extract first parameter set from model simulations dataframe
first_set <- head(bike_model_df, 1)

#Simulate one ridership outcome
beta_0 <- first_set$`(Intercept)`
beta_1 <- first_set$temp_feel
sigma  <- first_set$sigma

one_simulation <- bikes %>% 
  mutate(mu = beta_0 + beta_1 * temp_feel,
         simulated_rides = rnorm(500, mean = mu, sd = sigma)) %>% 
  select(temp_feel, rides, simulated_rides)

#Explore simulation
head(one_simulation, 2)

#Plot simulation
ggplot(one_simulation, aes(x = simulated_rides)) + 
  geom_density(color = "lightblue") + 
  geom_density(aes(x = rides), color = "darkblue")

# Examine 50 of the 20000 simulated samples
pp_check(bike_model, nreps = 50) + 
  xlab("rides")

#Posterior Predictive Summaries
bikes %>% 
  filter(date == "2012-10-22") %>% 
  select(temp_feel, rides)

#Simulate the posterior predictive model
predict_75 <- bike_model_df %>% 
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))

# Plot the posterior predictive model
ggplot(predict_75, aes(x = y_new)) + 
  geom_density()

#Calculate prediction error
predict_75 %>% 
  summarize(mean = mean(y_new), error = 6228 - mean(y_new))

#Scale standard error
predict_75 %>% 
  summarize(sd = sd(y_new), error = 6228 - mean(y_new),
            error_scaled = error / sd(y_new))

#Summarise prediction
predict_75 %>% 
  summarize(lower_95 = quantile(y_new, 0.025),
            lower_50 = quantile(y_new, 0.25),
            upper_50 = quantile(y_new, 0.75),
            upper_95 = quantile(y_new, 0.975))

#Compute predictions
predictions <- posterior_predict(bike_model, newdata = bikes)
dim(predictions)

#Plot prediction intervals
ppc_intervals(bikes$rides, yrep = predictions, x = bikes$temp_feel, prob = 0.5, prob_outer = 0.95)

#Posterior predictive summaries
prediction_summary(bike_model, data = bikes)
 
#Using CV to fit models
cv_procedure <- prediction_summary_cv(model = bike_model, data = bikes, k = 10)

#Examine model training folds
cv_procedure$folds

#Display model performance metrics for the 10 training folds
cv_procedure$cv

# Posterior predictive summaries for original data
prediction_summary(bike_model, data = bikes)

#Expected log predictive density
model_elpd <- loo(bike_model)
model_elpd$estimates

#Chapter 11: Extending the Normal Regression Model#

#Load weather data
data(weather_WU)
weather_WU %>% 
  group_by(location) %>% 
  tally()

#Select variables needed for the model
weather_WU <- weather_WU %>% 
  select(location, windspeed9am, humidity9am, pressure9am, temp9am, temp3pm)

#Plot data
ggplot(weather_WU, aes(x = temp9am, y = temp3pm)) +
  geom_point(size = 0.2)

#Build model to predict temperature
weather_model_1 <- stan_glm(
  temp3pm ~ temp9am, 
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Prior specification
prior_summary(weather_model_1)

# MCMC diagnostics
mcmc_trace(weather_model_1, size = 0.1)
mcmc_dens_overlay(weather_model_1)
mcmc_acf(weather_model_1)
neff_ratio(weather_model_1)
rhat(weather_model_1)

# Posterior credible intervals
posterior_interval(weather_model_1, prob = 0.80)

#Check how well the model posterior captures the data
pp_check(weather_model_1)

#Adding a categorical predictor
ggplot(weather_WU, aes(x = temp3pm, fill = location)) + 
  geom_density(alpha = 0.5)

#Simulate the posterior
weather_model_2 <- stan_glm(
  temp3pm ~ location,
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# MCMC diagnostics
mcmc_trace(weather_model_2, size = 0.1)
mcmc_dens_overlay(weather_model_2)
mcmc_acf(weather_model_2)

# Posterior summary statistics
tidy(weather_model_2, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.80) %>% 
  select(-std.error)

#Visualise posterior by area
as.data.frame(weather_model_2) %>% 
  mutate(uluru = `(Intercept)`, 
         wollongong = `(Intercept)` + locationWollongong) %>% 
  mcmc_areas(pars = c("uluru", "wollongong"))

#Using two covariates
ggplot(weather_WU, aes(y = temp3pm, x = temp9am, color = location)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#Build model with two covariates
weather_model_3_prior <- stan_glm(
  temp3pm ~ temp9am + location,
  data = weather_WU, family = gaussian, 
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135,
  prior_PD = TRUE)

#Visualise prior assumptions
weather_WU %>%
  add_predicted_draws(weather_model_3_prior, n = 100) %>%
  ggplot(aes(x = .prediction, group = .draw)) +
  geom_density() + 
  xlab("temp3pm")

weather_WU %>%
  add_fitted_draws(weather_model_3_prior, n = 100) %>%
  ggplot(aes(x = temp9am, y = temp3pm, color = location)) +
  geom_line(aes(y = .value, group = paste(location, .draw)))

#Simulate from the posterior
weather_model_3 <- update(weather_model_3_prior, prior_PD = FALSE)
head(as.data.frame(weather_model_3), 3)

#Plotting posterior
weather_WU %>%
  add_fitted_draws(weather_model_3, n = 100) %>%
  ggplot(aes(x = temp9am, y = temp3pm, color = location)) +
  geom_line(aes(y = .value, group = paste(location, .draw)), alpha = .1) +
  geom_point(data = weather_WU, size = 0.1)

# Posterior summaries
posterior_interval(weather_model_3, prob = 0.80, pars = c("temp9am", "locationWollongong"))

# Simulate a set of predictions
temp3pm_prediction <- posterior_predict(
  weather_model_3,
  newdata = data.frame(temp9am = c(10, 10), 
                       location = c("Uluru", "Wollongong")))

# Plot the posterior predictive models
mcmc_areas(temp3pm_prediction) +
  ggplot2::scale_y_discrete(labels = c("Uluru", "Wollongong")) + 
  xlab("temp3pm")  

#Adding an interaction term

#Plotting the data
ggplot(weather_WU, aes(y = temp3pm, x = humidity9am, color = location)) +
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

#Simulating the posterior
interaction_model <- stan_glm(
  temp3pm ~ location + humidity9am + location:humidity9am, 
  data = weather_WU, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Posterior summary statistics
tidy(interaction_model, effects = c("fixed", "aux"))
posterior_interval(interaction_model, prob = 0.80, pars = "locationWollongong:humidity9am")

#Plotting posterior sample draws
weather_WU %>%
  add_fitted_draws(interaction_model, n = 200) %>%
  ggplot(aes(x = humidity9am, y = temp3pm, color = location)) +
  geom_line(aes(y = .value, group = paste(location, .draw)), alpha = 0.1)

#Explore bike ridership data
data(bike_users)
bike_users %>% 
  group_by(user) %>% 
  tally()

#Subset data
bike_casual <- bike_users %>% 
  filter(user == "casual")
bike_registered <- bike_users %>% 
  filter(user == "registered")

#Plot interaction terms
ggplot(bike_casual, aes(y = rides, x = temp_actual, color = weekend)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "casual riders")

ggplot(bike_casual, aes(y = rides, x = temp_actual, color = humidity)) + 
  geom_point()

ggplot(bike_casual, 
       aes(y = rides, x = temp_actual, 
           color = cut(humidity, 2, labels = c("low","high")))) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(color = "humidity_level") + 
  lims(y = c(0, 2500))

#Data Exploration Visualisation
ggplot(bike_users, aes(y = rides, x = user, fill = weather_cat)) + 
  geom_boxplot() 

#Adding additional predictors
weather_WU %>% 
  names()

#Build model
weather_model_4 <- stan_glm(
  temp3pm ~ .,
  data = weather_WU, family = gaussian, 
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Confirm prior specification
prior_summary(weather_model_4)

# Check MCMC diagnostics
mcmc_trace(weather_model_4)
mcmc_dens_overlay(weather_model_4)
mcmc_acf(weather_model_4)

# Posterior summaries
posterior_interval(weather_model_4, prob = 0.95)

#Posterior predictive checks
pp_check(weather_model_1)

#Evaluating posterior predictive accuracy
predictions_1 <- posterior_predict(weather_model_1, newdata = weather_WU)

# Posterior predictive models for weather_model_1
ppc_intervals(weather_WU$temp3pm, yrep = predictions_1, 
              x = weather_WU$temp9am, prob = 0.5, prob_outer = 0.95) + 
  labs(x = "temp9am", y = "temp3pm")

#Evaluating predictive accuracy using cross validation
prediction_summary_cv(model = weather_model_1, data = weather_WU, k = 10)

#Evaluating preditive accuracy using ELPD
loo_1 <- loo(weather_model_1)
loo_4 <- loo(weather_model_4)

# Results
c(loo_1$estimates[1],loo_4$estimates[1])

#Compare model performance
loo_compare(loo_1,loo_4)

#Evaluate bias-variance trade off

#Take 2 separate samples
weather_shuffle <- weather_australia %>% 
  filter(temp3pm < 30, location == "Wollongong") %>% 
  sample_n(nrow(.))
sample_1 <- weather_shuffle %>% head(40)
sample_2 <- weather_shuffle %>% tail(40)

#Plot of temperature by time of the year
g <- ggplot(sample_1, aes(y = temp3pm, x = day_of_year)) + 
  geom_point()

#Build separate models with different degrees of complexity
g + geom_smooth(method = "lm", se = FALSE)
g + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2))
g + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 12))

model_1 <- stan_glm(
  temp3pm ~ day_of_year,
  data = sample_1, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

model_2 <- stan_glm(
  temp3pm ~ poly(day_of_year, 2),
  data = sample_1, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

model_3 <- stan_glm(
  temp3pm ~ poly(day_of_year, 12),
  data = sample_1, family = gaussian,
  prior_intercept = normal(25, 5),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

#Evaluate model performance using CV
prediction_summary(model = model_1, data = sample_1)
prediction_summary_cv(model = model_1, data = sample_1, k = 10)$cv 

#Chapter 12:Poisson & Negative Binomial Regression#

# Load data
data(equality_index)
equality <- equality_index

#Exploratory Data Visualisation
ggplot(equality, aes(x = laws)) + 
  geom_histogram(color = "white", breaks = seq(0, 160, by = 10))

# Identify the outlier
equality %>% 
  filter(laws == max(laws))

# Remove the outlier
equality <- equality %>% 
  filter(state != "california")

#Explore impact of percent_urban
ggplot(equality, aes(y = laws, x = percent_urban, color = historical)) + 
  geom_point()

# Simulate the Normal model
equality_normal_sim <- stan_glm(laws ~ percent_urban + historical, 
                                data = equality, 
                                family = gaussian,
                                prior_intercept = normal(7, 1.5),
                                prior = normal(0, 2.5, autoscale = TRUE),
                                prior_aux = exponential(1, autoscale = TRUE),
                                chains = 4, iter = 5000*2, seed = 135)

# Posterior predictive check
pp_check(equality_normal_sim, plotfun = "hist", nreps = 5) + 
  geom_vline(xintercept = 0) + 
  xlab("laws")

#Build Poisson Regression Model
equality_model_prior <- stan_glm(laws ~ percent_urban + historical, 
                                 data = equality, 
                                 family = poisson,
                                 prior_intercept = normal(2, 0.5),
                                 prior = normal(0, 2.5, autoscale = TRUE), 
                                 chains = 4, iter = 5000*2, seed = 135, 
                                 prior_PD = TRUE)

prior_summary(equality_model_prior)

equality %>% 
  add_fitted_draws(equality_model_prior, n = 100) %>%
  ggplot(aes(x = percent_urban, y = laws, color = historical)) +
  geom_line(aes(y = .value, group = paste(historical, .draw))) + 
  ylim(0, 100)

#Simulate posterior Poisson regression
equality_model <- update(equality_model_prior, prior_PD = FALSE)

#MCMC model diagnostics
mcmc_trace(equality_model)
mcmc_dens_overlay(equality_model)
mcmc_acf(equality_model)

#Posterior predictive checks
pp_check(equality_model, plotfun = "hist", nreps = 5) + 
  xlab("laws")
pp_check(equality_model) + 
  xlab("laws")

#Interpret the posterior
equality %>%
  add_fitted_draws(equality_model, n = 50) %>%
  ggplot(aes(x = percent_urban, y = laws, color = historical)) +
  geom_line(aes(y = .value, group = paste(historical, .draw)), 
            alpha = .1) +
  geom_point(data = equality, size = 0.1)

tidy(equality_model, conf.int = TRUE, conf.level = 0.80)

#Posterior predictive checks
equality %>% 
  filter(state == "minnesota")

# Calculate posterior predictions
mn_prediction <- posterior_predict(
  equality_model, newdata = data.frame(percent_urban = 73.3, historical = "dem"))

head(mn_prediction, 3)

mcmc_hist(mn_prediction, binwidth = 1) + 
  geom_vline(xintercept = 4) + 
  xlab("Predicted number of laws in Minnesota")

# Predict number of laws for each parameter set in the chain
as.data.frame(equality_model) %>% 
  mutate(log_lambda = `(Intercept)` + percent_urban*73.3 + 
           historicalgop*0 + historicalswing*0,
         lambda = exp(log_lambda),
         y_new = rpois(20000, lambda = lambda)) %>% 
  ggplot(aes(x = y_new)) + 
  stat_count()

#Model evaluation
# Simulate posterior predictive models for each state
poisson_predictions <- posterior_predict(equality_model, newdata = equality)
# Plot the posterior predictive models for each state
ppc_intervals_grouped(equality$laws, yrep = poisson_predictions, 
                      x = equality$percent_urban, 
                      group = equality$historical,
                      prob = 0.5, prob_outer = 0.95,
                      facet_args = list(scales = "fixed"))

prediction_summary(model = equality_model, data = equality)

#Using CV to evaluate Poisson model performance
poisson_cv <- prediction_summary_cv(model = equality_model, data = equality, k = 10)
poisson_cv$cv 

#Negative Binomial Regression For Overdispersed Counts

# Load data
data(pulse_of_the_nation)
pulse <- pulse_of_the_nation %>% 
  filter(books < 100)

#Plot data
ggplot(pulse, aes(x = books)) + 
  geom_histogram(color = "white")
ggplot(pulse, aes(y = books, x = age)) + 
  geom_point()
ggplot(pulse, aes(y = books, x = wise_unwise)) + 
  geom_boxplot()

#Build a Poisson model first for reference
books_poisson_sim <- stan_glm(
  books ~ age + wise_unwise, 
  data = pulse, family = poisson,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

#Evaluate model
pp_check(books_poisson_sim) + 
  xlab("books")

# Mean and variability in readership across all subjects since it is a key assumption
pulse %>% 
  summarize(mean = mean(books), var = var(books))

# Mean and variability in readership 
# among subjects of similar age and wise_unwise response
pulse %>% 
  group_by(cut(age,3), wise_unwise) %>% 
  summarize(mean = mean(books), var = var(books))

#Build negative binomial model
books_negbin_sim <- stan_glm(
  books ~ age + wise_unwise, 
  data = pulse, family = neg_binomial_2,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Check out the priors
prior_summary(books_negbin_sim)

pp_check(books_negbin_sim) + 
  xlim(0, 75) + 
  xlab("books")

# Numerical summaries
tidy(books_negbin_sim, conf.int = TRUE, conf.level = 0.80)

#Chapter 13:Logistic Regression#

# Load and process the data
data(weather_perth)
weather <- weather_perth %>% 
  select(day_of_year, raintomorrow, humidity9am, humidity3pm, raintoday)

# Run a prior simulation
rain_model_prior <- stan_glm(raintomorrow ~ humidity9am,
                             data = weather, family = binomial,
                             prior_intercept = normal(-1.4, 0.7),
                             prior = normal(0.07, 0.035),
                             chains = 4, iter = 5000*2, seed = 135,
                             prior_PD = TRUE)

# Plot 100 prior models with humidity
weather %>% 
  add_fitted_draws(rain_model_prior, n = 100) %>% 
  ggplot(aes(x = humidity9am, y = raintomorrow)) +
  geom_line(aes(y = .value, group = .draw), size = 0.1)

# Plot the observed proportion of rain in 100 prior datasets
weather %>% 
  add_predicted_draws(rain_model_prior, n = 100) %>% 
  group_by(.draw) %>% 
  summarize(proportion_rain = mean(.prediction == 1)) %>% 
  ggplot(aes(x = proportion_rain)) +
  geom_histogram(color = "white")

#Plot humidity versus probability of rain
ggplot(weather, aes(x = humidity9am, y = raintomorrow)) + 
  geom_jitter(size = 0.2)

# Calculate & plot the rain rate by humidity bracket
weather %>% 
  mutate(humidity_bracket = 
           cut(humidity9am, breaks = seq(10, 100, by = 10))) %>% 
  group_by(humidity_bracket) %>% 
  summarize(rain_rate = mean(raintomorrow == "Yes")) %>% 
  ggplot(aes(x = humidity_bracket, y = rain_rate)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Simulate the model
rain_model_1 <- update(rain_model_prior, prior_PD = FALSE)

# MCMC trace, density, & autocorrelation plots
mcmc_trace(rain_model_1)
mcmc_dens_overlay(rain_model_1)
mcmc_acf(rain_model_1)

#Plot 100 plausible posterior models
weather %>%
  add_fitted_draws(rain_model_1, n = 100) %>%
  ggplot(aes(x = humidity9am, y = raintomorrow)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.15) + 
  labs(y = "probability of rain")

# Posterior summaries on the log(odds) scale
posterior_interval(rain_model_1, prob = 0.80)

# Posterior summaries on the odds scale
exp(posterior_interval(rain_model_1, prob = 0.80))

# Posterior predictions of binary outcome
binary_prediction <- posterior_predict(
  rain_model_1, newdata = data.frame(humidity9am = 99))

#Implement predictions using the logistic formula
rain_model_1_df <- as.data.frame(rain_model_1) %>% 
  mutate(log_odds = `(Intercept)` + humidity9am*99,
         odds = exp(log_odds),
         prob = odds / (1 + odds),
         Y = rbinom(20000, size = 1, prob = prob))

#Explore outputs
head(rain_model_1_df)

mcmc_hist(binary_prediction) + 
  labs(x = "Y")
ggplot(rain_model_1_df, aes(x = Y)) + 
  stat_count()

# Summarize the posterior predictions of Y
table(binary_prediction)
colMeans(binary_prediction)

#Model Evaluation
proportion_rain <- function(x){mean(x == 1)}
pp_check(rain_model_1, nreps = 100,
         plotfun = "stat", stat = "proportion_rain") + 
  xlab("probability of rain")

# Posterior predictive models for each day in dataset
rain_pred_1 <- posterior_predict(rain_model_1, newdata = weather)
dim(rain_pred_1)

#Make predictions with a cut off score of 0.5
weather_classifications <- weather %>% 
  mutate(rain_prob = colMeans(rain_pred_1),
         rain_class_1 = as.numeric(rain_prob >= 0.5)) %>% 
  select(humidity9am, rain_prob, rain_class_1, raintomorrow)
head(weather_classifications, 3)

# Confusion matrix
weather_classifications %>% 
  tabyl(raintomorrow, rain_class_1) %>% 
  adorn_totals(c("row", "col"))

classification_summary(model = rain_model_1, data = weather, cutoff = 0.5)

#Apply a different cutoff
classification_summary(model = rain_model_1, data = weather, cutoff = 0.2)

#Use cross-validation to assess the model
cv_accuracy_1 <- classification_summary_cv(
  model = rain_model_1, data = weather, cutoff = 0.2, k = 10)
cv_accuracy_1$cv

#Extending the model
rain_model_2 <- stan_glm(
  raintomorrow ~ humidity9am + humidity3pm + raintoday, 
  data = weather, family = binomial,
  prior_intercept = normal(-1.4, 0.7),
  prior = normal(0, 2.5, autoscale = TRUE), 
  chains = 4, iter = 5000*2, seed = 135)

# Obtain prior model specifications
prior_summary(rain_model_2)

# Numerical summaries
tidy(rain_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)
cv_accuracy_2 <- classification_summary_cv(
  model = rain_model_2, data = weather, cutoff = 0.2, k = 10)
cv_accuracy_2$cv

# Calculate ELPD for the models
loo_1 <- loo(rain_model_1)
loo_2 <- loo(rain_model_2)

# Compare the ELPD for the 2 models
loo_compare(loo_1, loo_2)

#Chapter 14:Naive Bayes#

# Load data
data(penguins_bayes)
penguins <- penguins_bayes

#Explore data
penguins %>% 
  tabyl(species)

#Remove missing values from plot
ggplot(penguins %>% drop_na(above_average_weight), 
       aes(fill = above_average_weight, x = species)) + 
  geom_bar(position = "fill")

#Crosstab of species against above average weight variable
penguins %>% 
  select(species, above_average_weight) %>% 
  na.omit() %>% 
  tabyl(species, above_average_weight) %>% 
  adorn_totals(c("row", "col"))

#Incorporate one continuous predictor
ggplot(penguins, aes(x = bill_length_mm, fill = species)) + 
  geom_density(alpha = 0.7) + 
  geom_vline(xintercept = 50, linetype = "dashed")

# Calculate sample mean and sd for each species
penguins %>% 
  group_by(species) %>% 
  summarize(mean = mean(bill_length_mm, na.rm = TRUE), sd = sd(bill_length_mm, na.rm = TRUE))

#Plotting densities for each specie
ggplot(penguins, aes(x = bill_length_mm, color = species)) + 
  stat_function(fun = dnorm, args = list(mean = 38.8, sd = 2.66), 
                aes(color = "Adelie")) +
  stat_function(fun = dnorm, args = list(mean = 48.8, sd = 3.34),
                aes(color = "Chinstrap")) +
  stat_function(fun = dnorm, args = list(mean = 47.5, sd = 3.08),
                aes(color = "Gentoo")) + 
  geom_vline(xintercept = 50, linetype = "dashed")

#Calculate likelihoods
dnorm(50, mean = 38.8, sd = 2.66)
dnorm(50, mean = 48.8, sd = 3.34)
dnorm(50, mean = 47.5, sd = 3.08)

#Adding two continuous predictors
ggplot(penguins, aes(x = bill_length_mm, fill = species)) + 
  geom_density(alpha = 0.6)
ggplot(penguins, aes(x = flipper_length_mm, fill = species)) + 
  geom_density(alpha = 0.6)

#Scatterplot to check correlation between predictors
ggplot(penguins,
       aes(x = flipper_length_mm, y = bill_length_mm, color = species)) + 
  geom_point()

# Calculate sample mean and sd for each specie
penguins %>% 
  group_by(species) %>% 
  summarize(mean = mean(flipper_length_mm, na.rm = TRUE), sd = sd(flipper_length_mm, na.rm = TRUE))

#Calculate likelihoods
dnorm(195, mean = 190, sd = 6.54)
dnorm(195, mean = 196, sd = 7.13)
dnorm(195, mean = 217, sd = 6.48)

#Implementing Naive Bayes Model
naive_model_1 <- naiveBayes(species ~ bill_length_mm, data = penguins)
naive_model_2 <- naiveBayes(species ~ bill_length_mm + flipper_length_mm, data = penguins)

#Create dataset for predictions
our_penguin <- data.frame(bill_length_mm = 50, flipper_length_mm = 195)

#Make predictions with Naive Bayes model
predict(naive_model_1, newdata = our_penguin, type = "raw")
predict(naive_model_1, newdata = our_penguin)
predict(naive_model_2, newdata = our_penguin, type = "raw")
predict(naive_model_2, newdata = our_penguin)

#Add predictions to a dataframe
penguins <- penguins %>% 
  mutate(class_1 = predict(naive_model_1, newdata = .),
         class_2 = predict(naive_model_2, newdata = .))

#Sample 4 observations and examine predictions
penguins %>% 
  sample_n(4) %>% 
  select(bill_length_mm, flipper_length_mm, species, class_1, class_2) %>% 
  rename(bill = bill_length_mm, flipper = flipper_length_mm)

# Confusion matrix for naive_model_1
penguins %>% 
  tabyl(species, class_1) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Confusion matrix for naive_model_2
penguins %>% 
  tabyl(species, class_2) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

#Using Cross validation to evaluate model performance
cv_model_2 <- naive_classification_summary_cv(model = naive_model_2, data = penguins, y = "species", k = 10)
cv_model_2$cv

#Chapter 15:Hierachical Models Overview#

# Load data
data(cherry_blossom_sample)
running <- cherry_blossom_sample %>% 
  select(runner, age, net)
nrow(running)

#Plot data around variability of running times
ggplot(running, aes(x = runner, y = net)) + 
  geom_boxplot()

head(running, 2)

#Explore pooling versus non-pooling concepts
ggplot(running, aes(y = net, x = age)) + 
  geom_point()

#Build a regression model with complete pooling
complete_pooled_model <- stan_glm(
  net ~ age, 
  data = running, family = gaussian, 
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Posterior summary statistics
tidy(complete_pooled_model, conf.int = TRUE, conf.level = 0.80)

# Plot of the posterior median model
ggplot(running, aes(x = age, y = net, group = runner)) + 
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  geom_abline(aes(intercept = 75.2, slope = 0.268), color = "blue")

# Select an example subset
examples <- running %>% 
  filter(runner %in% c("1", "20", "22"))

ggplot(examples, aes(x = age, y = net)) + 
  geom_point() + 
  facet_wrap(~ runner) + 
  geom_abline(aes(intercept = 75.2242, slope = 0.2678), 
              color = "blue")

#Model with no pooling
ggplot(examples, aes(x = age, y = net)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) + 
  facet_wrap(~ runner) + 
  xlim(52, 62)

#Chapter 16: Normal Hierarchical Models Without Predictors#

# Load data
data(spotify)

#Select only data important to the analysis and reorder according to popularity
spotify <- spotify %>% 
  select(artist, title, popularity) %>% 
  mutate(artist = fct_reorder(artist, popularity, .fun = 'mean'))

# First few rows
head(spotify, 3)

# First few rows
nrow(spotify)

# Number of artists
nlevels(spotify$artist)

#Summary of number of songs per artist
artist_means <- spotify %>% 
  group_by(artist) %>% 
  summarize(count = n(), popularity = mean(popularity))

artist_means %>%
  slice(1:2, 43:44)

#Build a complete pooled model
head(artist_means, 2)

artist_means %>% 
  summarize(min(count), max(count))

#Plotting popularity density
ggplot(spotify, aes(x = popularity)) + 
  geom_density() 

#Build a normal complete pooled model
spotify_complete_pooled <- stan_glm(
  popularity ~ 1, 
  data = spotify, family = gaussian, 
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Get prior specifications
prior_summary(spotify_complete_pooled)

complete_summary <- tidy(spotify_complete_pooled, 
                         effects = c("fixed", "aux"), 
                         conf.int = TRUE, conf.level = 0.80)

predictions_complete <- posterior_predict(spotify_complete_pooled,
                                          newdata = artist_means)

ppc_intervals(artist_means$popularity, yrep = predictions_complete,
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist,
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1)

#No pooled model
ggplot(spotify, aes(x = popularity, group = artist)) + 
  geom_density()

spotify_no_pooled <- stan_glm(
  popularity ~ artist - 1, 
  data = spotify, family = gaussian, 
  prior = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 135)

# Simulate the posterior predictive models
set.seed(135)
predictions_no <- posterior_predict(spotify_no_pooled, newdata = artist_means)

# Plot the posterior predictive intervals
ppc_intervals(artist_means$popularity, yrep = predictions_no, 
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist, 
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1)

#Building a  hierarchy model
ggplot(artist_means, aes(x = popularity)) + 
  geom_density()

#Posterior analysis
spotify_hierarchical <- stan_glmer(
  popularity ~ (1 | artist), 
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135)

# Confirm the prior tunings
prior_summary(spotify_hierarchical)

#MCMC model diagnostics
mcmc_trace(spotify_hierarchical)
mcmc_dens_overlay(spotify_hierarchical)
mcmc_acf(spotify_hierarchical)
neff_ratio(spotify_hierarchical)
rhat(spotify_hierarchical)

#Further posterior predictive check
pp_check(spotify_hierarchical) + 
  xlab("popularity")

# Store the simulation in a data frame
spotify_hierarchical_df <- as.data.frame(spotify_hierarchical)

# Check out the first 3 and last 3 parameter labels
spotify_hierarchical_df %>% 
  colnames() %>% 
  as.data.frame() %>% 
  slice(1:3, 45:47)

#Posterior analysis of global parameters
tidy(spotify_hierarchical, effects = "fixed", 
     conf.int = TRUE, conf.level = 0.80)

#Examine posterior medians
tidy(spotify_hierarchical, effects = "ran_pars")

#Posterior analysis of group specific parameters
artist_summary <- tidy(spotify_hierarchical, effects = "ran_vals", 
                       conf.int = TRUE, conf.level = 0.80)

# Check out the results for the first & last 2 artists
artist_summary %>% 
  select(level, conf.low, conf.high) %>% 
  slice(1:2, 43:44)

# Get MCMC chains for each mu_j
artist_chains <- spotify_hierarchical %>%
  spread_draws(`(Intercept)`, b[,artist]) %>% 
  mutate(mu_j = `(Intercept)` + b)

# Check it out
artist_chains %>% 
  select(artist, `(Intercept)`, b, mu_j) %>% 
  head(4)

# Get posterior summaries for mu_j
artist_summary_scaled <- artist_chains %>% 
  select(-`(Intercept)`, -b) %>% 
  mean_qi(.width = 0.80) %>% 
  mutate(artist = fct_reorder(artist, mu_j))

# Check out the results
artist_summary_scaled %>% 
  select(artist, mu_j, .lower, .upper) %>% 
  head(4)

ggplot(artist_summary_scaled, 
       aes(x = artist, y = mu_j, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  xaxis_text(angle = 90, hjust = 1)

#Compare artists posterior predictions
artist_means %>% 
  filter(artist %in% c("Frank Ocean", "Lil Skies"))

#Posterior Prediction#

#Make predictions for new band without historical data
mohsen_chains <- spotify_hierarchical_df %>%
  mutate(sigma_mu = sqrt(`Sigma[artist:(Intercept),(Intercept)]`),
         mu_mohsen = rnorm(20000, `(Intercept)`, sigma_mu),
         y_mohsen = rnorm(20000, mu_mohsen, sigma)) 

# Posterior predictive summaries
mohsen_chains %>% 
  mean_qi(y_mohsen, .width = 0.80)

prediction_shortcut <- posterior_predict(spotify_hierarchical,
                      newdata = data.frame(artist = c("Frank Ocean", "Mohsen Beats")))

# Posterior predictive model plots
mcmc_areas(prediction_shortcut, prob = 0.8) +
  ggplot2::scale_y_discrete(labels = c("Frank Ocean", "Mohsen Beats"))

predictions_hierarchical <- posterior_predict(spotify_hierarchical, newdata = artist_means)

# Posterior predictive plots
ppc_intervals(artist_means$popularity, yrep = predictions_hierarchical, 
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist, 
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1) + 
  geom_hline(yintercept = 58.4, linetype = "dashed")

#Check effect of shrinkage
artist_means %>% 
  filter(artist %in% c("Camila Cabello", "Lil Skies"))

#Chapter 17: Normal Hierarchical Models With Predictors#

# Load data
data(cherry_blossom_sample)
running <- cherry_blossom_sample

# Remove NAs
running <- running %>% 
  select(runner, age, net) %>% 
  na.omit() 

#Build hierarchical model with varying intercepts
running_model_1_prior <- stan_glmer(
  net ~ age + (1 | runner), 
  data = running, family = gaussian,
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135, 
  prior_PD = TRUE)

#Tuning the prior
running %>% 
  add_fitted_draws(running_model_1_prior, n = 4) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(aes(y = .value, group = paste(runner, .draw))) + 
  facet_wrap(~ .draw)

running %>%
  add_predicted_draws(running_model_1_prior, n = 100) %>%
  ggplot(aes(x = net)) +
  geom_density(aes(x = .prediction, group = .draw)) +
  xlim(-100,300)

#Posterior simulation & analysis
ggplot(running, aes(x = age, y = net)) + 
  geom_point() + 
  facet_wrap(~ runner)

# Simulate the posterior
running_model_1 <- update(running_model_1_prior, prior_PD = FALSE)

# Check the prior specifications
prior_summary(running_model_1)

# Markov chain diagnostics
mcmc_trace(running_model_1)
mcmc_dens_overlay(running_model_1)
mcmc_acf(running_model_1)
neff_ratio(running_model_1)
rhat(running_model_1)

#Posterior analysis of global relationship
tidy_summary_1 <- tidy(running_model_1, effects = "fixed",
                       conf.int = TRUE, conf.level = 0.80)

B0 <- tidy_summary_1$estimate[1]
B1 <- tidy_summary_1$estimate[2]

#Plot posterior simulation
running %>%
  add_fitted_draws(running_model_1, n = 200, re_formula = NA) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.1) +
  geom_abline(intercept = B0, slope = B1, color = "blue") +
  lims(y = c(75, 110))

# Posterior summaries of runner-specific intercepts
runner_summaries_1 <- running_model_1 %>%
  spread_draws(`(Intercept)`, b[,runner]) %>% 
  mutate(runner_intercept = `(Intercept)` + b) %>% 
  select(-`(Intercept)`, -b) %>% 
  median_qi(.width = 0.80) %>% 
  select(runner, runner_intercept, .lower, .upper)

#Examine posterior parameters for runners 4 and 5
runner_summaries_1 %>% 
  filter(runner %in% c("runner:4", "runner:5"))

# 100 posterior plausible models for runners 4 & 5
running %>%
  filter(runner %in% c("4", "5")) %>% 
  add_fitted_draws(running_model_1, n = 100) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(
    aes(y = .value, group = paste(runner, .draw), color = runner),
    alpha = 0.1) +
  geom_point(aes(color = runner))

# Plot runner-specific models with the global model
ggplot(running, aes(y = net, x = age, group = runner)) + 
  geom_abline(data = runner_summaries_1, color = "gray",
              aes(intercept = runner_intercept, slope = B1)) + 
  geom_abline(intercept = B0, slope = B1, color = "blue") + 
  lims(x = c(50, 61), y = c(50, 135))

tidy_sigma <- tidy(running_model_1, effects = "ran_pars")

#Build model with varying intercepts and slopes

# Plot runner-specific models in the data
running %>% 
  filter(runner %in% c("4", "5", "20", "29")) %>% 
  ggplot(., aes(x = age, y = net)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_grid(~ runner)

ggplot(running, aes(x = age, y = net, group = runner)) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5)

running_model_2 <- stan_glmer(
  net ~ age + (age | runner),
  data = running, family = gaussian,
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135, adapt_delta = 0.99999
)

# Confirm the prior model specifications
prior_summary(running_model_2)

# Quick summary of global regression parameters
tidy(running_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)

# Get MCMC chains for the runner-specific intercepts & slopes
runner_chains_2 <- running_model_2 %>%
  spread_draws(`(Intercept)`, b[term, runner], `age`) %>% 
  pivot_wider(names_from = term, names_glue = "b_{term}",
              values_from = b) %>% 
  mutate(runner_intercept = `(Intercept)` + `b_(Intercept)`,
         runner_age = age + b_age)

# Posterior medians of runner-specific models
runner_summaries_2 <- runner_chains_2 %>% 
  group_by(runner) %>% 
  summarize(runner_intercept = median(runner_intercept),
            runner_age = median(runner_age))

# Check out posterior summaries
head(runner_summaries_2, 3)

ggplot(running, aes(y = net, x = age, group = runner)) + 
  geom_abline(data = runner_summaries_2, color = "gray",
              aes(intercept = runner_intercept, slope = runner_age)) + 
  lims(x = c(50, 61), y = c(50, 135))

#Examine runner 10n to see the effect of shrinkage
runner_summaries_2 %>% 
  filter(runner %in% c("runner:1", "runner:10"))

#Posterior analysis of with-within variability
tidy(running_model_2, effects = "ran_pars")

#Model evaluation and selection
pp_check(complete_pooled_model) + 
  labs(x = "net", title = "complete pooled model")
pp_check(running_model_1) + 
  labs(x = "net", title = "running model 1")
pp_check(running_model_2) + 
  labs(x = "net", title = "running model 2")

# Calculate prediction summaries
prediction_summary(model = running_model_1, data = running)
prediction_summary(model = running_model_2, data = running)

# Calculate ELPD for the 2 models
elpd_hierarchical_1 <- loo(running_model_1)
elpd_hierarchical_2 <- loo(running_model_2)

# Compare the ELPD
loo_compare(elpd_hierarchical_1, elpd_hierarchical_2)

#Posterior Predition

# Plot runner-specific trends for runners 1 & 10
running %>% 
  filter(runner %in% c("1", "10")) %>% 
  ggplot(., aes(x = age, y = net)) + 
  geom_point() + 
  facet_grid(~ runner) + 
  lims(x = c(54, 61))

# Simulate posterior predictive models for 3 runners
predict_next_race <- posterior_predict(
  running_model_1, 
  newdata = data.frame(runner = c("1", "Miles", "10"),
                       age = c(61, 61, 61)))

# Posterior predictive model plots
mcmc_areas(predict_next_race, prob = 0.8) +
  ggplot2::scale_y_discrete(labels = c("runner 1", "Miles", "runner 10"))

# Example with danceability data
data(spotify)
spotify <- spotify %>% 
  select(artist, title, danceability, valence, genre)

#Data visualisation of model variables
ggplot(spotify, aes(y = danceability, x = genre)) + 
  geom_boxplot()
ggplot(spotify, aes(y = danceability, x = valence)) + 
  geom_point()
ggplot(spotify, aes(y = danceability, x = valence, group = artist)) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5)

#Build hierarchical models
spotify_model_1 <- stan_glmer(
  danceability ~ valence + genre + (1 | artist),
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135)

spotify_model_2 <- stan_glmer(
  danceability ~ valence + genre + (valence | artist), 
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135)

# Check out the prior specifications
prior_summary(spotify_model_1)
prior_summary(spotify_model_2)

#Posterior predictive checks
pp_check(spotify_model_1) +
  xlab("danceability")
pp_check(spotify_model_2) +
  xlab("danceability")

# Calculate ELPD for the 2 models
elpd_spotify_1 <- loo(spotify_model_1)
elpd_spotify_2 <- loo(spotify_model_2)

# Compare the ELPD
loo_compare(elpd_spotify_1, elpd_spotify_2)

tidy(spotify_model_1, effects = "fixed",
     conf.int = TRUE, conf.level = 0.80)

# Plot the posterior models of the genre coefficients
mcmc_areas(spotify_model_1, pars = vars(starts_with("genre")), prob = 0.8) + 
  geom_vline(xintercept = 0)

tidy(spotify_model_1, effects = "ran_vals",
     conf.int = TRUE, conf.level = 0.80) %>% 
  filter(level %in% c("Camilo", "Missy_Elliott")) %>% 
  select(level, estimate, conf.low, conf.high)

# Simulate posterior predictive models for the 3 artists
predict_next_song <- posterior_predict(
  spotify_model_1,
  newdata = data.frame(
    artist = c("Camilo", "Mohsen Beats", "Missy Elliott"), 
    valence = c(80, 60, 90), genre = c("latin", "rock", "rap")))

# Posterior predictive model plots
mcmc_areas(predict_next_song, prob = 0.8) +
  ggplot2::scale_y_discrete(
    labels = c("Camilo", "Mohsen Beats", "Missy Elliott"))

#Chapter 18:Non Normal Hierarchical Regression & Classification#

# Import, rename, & clean data
data(climbers_sub)
climbers <- climbers_sub %>% 
  select(expedition_id, member_id, success, year, season,
         age, expedition_role, oxygen_used)

#Explore climbers data
nrow(climbers)

climbers %>% 
  tabyl(success)

#Calculate Size per expedition
climbers_per_expedition <- climbers %>% 
  group_by(expedition_id) %>% 
  summarize(count = n())

# Number of expeditions
nrow(climbers_per_expedition)

climbers_per_expedition %>% 
  head(3)

# Calculate the success rate for each exhibition
expedition_success <- climbers %>% 
  group_by(expedition_id) %>% 
  summarize(success_rate = mean(success))

# Plot the success rates across exhibitions
ggplot(expedition_success, aes(x = success_rate)) + 
  geom_histogram(color = "white")

# Calculate the success rate by age and oxygen use
data_by_age_oxygen <- climbers %>% 
  group_by(age, oxygen_used) %>% 
  summarize(success_rate = mean(success))

# Plot this relationship
ggplot(data_by_age_oxygen, aes(x = age, y = success_rate, 
                               color = oxygen_used)) + 
  geom_point()

#Build intercept only model
climb_model <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id), 
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135
)

# Confirm prior specifications
prior_summary(climb_model)

# MCMC diagnostics
mcmc_trace(climb_model, size = 0.1)
mcmc_dens_overlay(climb_model)
mcmc_acf(climb_model)
neff_ratio(climb_model)
rhat(climb_model)

# Define success rate function
success_rate <- function(x){mean(x == 1)}

# Posterior predictive check
pp_check(climb_model, nreps = 100,
         plotfun = "stat", stat = "success_rate") + 
  xlab("success rate") 

#Posterior analysis
tidy(climb_model, effects = "fixed", conf.int = TRUE, conf.level = 0.80)

climbers %>%
  add_fitted_draws(climb_model, n = 100, re_formula = NA) %>%
  ggplot(aes(x = age, y = success, color = oxygen_used)) +
  geom_line(aes(y = .value, group = paste(oxygen_used, .draw)), 
            alpha = 0.1) + 
  labs(y = "probability of success")

#Posterior classification

# New expedition
new_expedition <- data.frame(
  age = c(20, 20, 60, 60), oxygen_used = c(FALSE, TRUE, FALSE, TRUE), 
  expedition_id = rep("new", 4))

# Posterior predictions of binary outcome
binary_prediction <- posterior_predict(climb_model, newdata = new_expedition)

# First 3 prediction sets
head(binary_prediction, 3)

# Summarize the posterior predictions of Y
colMeans(binary_prediction)

#Model Evaluation
classification_summary(data = climbers, model = climb_model, cutoff = 0.5)

#Change cut off to increase specifity
classification_summary(data = climbers, model = climb_model, cutoff = 0.65)

#Hierarchical Poisson & Negative Binomial Regression

# Load data
data(airbnb)

# Number of listings
nrow(airbnb)

# Number of neighborhoods
airbnb %>% 
  summarize(nlevels(neighborhood))

#Model building and simulation
ggplot(airbnb, aes(x = reviews)) + 
  geom_histogram(color = "white", breaks = seq(0, 200, by = 10))
ggplot(airbnb, aes(y = reviews, x = rating)) + 
  geom_jitter()
ggplot(airbnb, aes(y = reviews, x = room_type)) + 
  geom_violin()

#Focus on three neighbourhoods
airbnb %>% 
  filter(neighborhood %in% 
           c("Albany Park", "East Garfield Park", "The Loop")) %>% 
  ggplot(aes(y = reviews, x = rating, color = room_type)) + 
  geom_jitter() + 
  facet_wrap(~ neighborhood)

#Build Poisson model
airbnb_model_1 <- stan_glmer(
  reviews ~ rating + room_type + (1 | neighborhood), 
  data = airbnb, family = poisson,
  prior_intercept = normal(3, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135
)

#Posterior predictive check
pp_check(airbnb_model_1) + 
  xlim(0, 200) + 
  xlab("reviews") 

#Build negative binomial model
airbnb_model_2 <- stan_glmer(
  reviews ~ rating + room_type + (1 | neighborhood), 
  data = airbnb, family = neg_binomial_2,
  prior_intercept = normal(3, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)

#Posterior predictive check
pp_check(airbnb_model_2) + 
  xlim(0, 200) + 
  xlab("reviews")

#Posterior analysis
tidy(airbnb_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)

tidy(airbnb_model_2, effects = "ran_vals", 
     conf.int = TRUE, conf.level = 0.80) %>% 
  select(level, estimate, conf.low, conf.high) %>% 
  filter(level %in% c("Albany_Park", "East_Garfield_Park", "The_Loop"))

# Posterior predictions of reviews
predicted_reviews <- posterior_predict(
  airbnb_model_2, 
  newdata = data.frame(
    rating = rep(5, 3), 
    room_type = rep("Entire home/apt", 3), 
    neighborhood = c("Albany Park", "East Garfield Park", "The Loop")))

mcmc_areas(predicted_reviews, prob = 0.8) +
  ggplot2::scale_y_discrete(
    labels = c("Albany Park", "East Garfield Park", "The Loop")) + 
  xlim(0, 150) + 
  xlab("reviews")

#Model evaluation
prediction_summary(model = airbnb_model_2, data = airbnb)

#Chapter 19:Adding more layers to the hierarchical model#

#Load data
data(airbnb)

# Number of listings
nrow(airbnb)

# Number of neighborhoods & other summaries
airbnb %>% 
  summarize(nlevels(neighborhood), min(price), max(price))

#Plotting data
ggplot(airbnb, aes(x = price)) + 
  geom_histogram(color = "white", breaks = seq(0, 500, by = 20))
ggplot(airbnb, aes(x = log(price))) + 
  geom_histogram(color = "white", binwidth = 0.5) 

#Using a model with individual only predictors
ggplot(airbnb, aes(y = log(price), x = bedrooms)) + 
  geom_jitter()
ggplot(airbnb, aes(y = log(price), x = rating)) + 
  geom_jitter()
ggplot(airbnb, aes(y = log(price), x = room_type)) + 
  geom_boxplot()

ggplot(airbnb, aes(y = log(price), x = neighborhood)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c(1:44))

#Build model with random specific intercepts to predict log price
airbnb_model_1 <- stan_glmer(
  log(price) ~ bedrooms + rating + room_type + (1 | neighborhood), 
  data = airbnb, family = gaussian,
  prior_intercept = normal(4.6, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135)
  
prior_summary(airbnb_model_1)

#Posterior predictive checks
pp_check(airbnb_model_1) + 
  labs(title = "airbnb_model_1 of log(price)") + 
  xlab("log(price)")

#Incorporate group level predictors
airbnb %>% 
  select(price, neighborhood, bedrooms, rating, room_type) %>% 
  head(3)

#Adding walkability score to the model
airbnb %>% 
  select(price, neighborhood, walk_score, transit_score) %>% 
  head(3)

# Calculate mean log(price) by neighborhood
nbhd_features <- airbnb %>% 
  group_by(neighborhood, walk_score) %>% 
  summarize(mean_log_price = mean(log(price)), n_listings = n()) %>% 
  ungroup()

# Plot mean log(price) vs walkability
ggplot(nbhd_features, aes(y = mean_log_price, x = walk_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Incorporate group level score predictors in the model
airbnb_model_2 <- stan_glmer(
  log(price) ~ walk_score + bedrooms + rating + room_type +
    (1 | neighborhood), 
  data = airbnb, family = gaussian,
  prior_intercept = normal(4.6, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135
)

# Don't forget to check the prior specifications!
prior_summary(airbnb_model_2)

# Get relationship summaries for both models
model_1_mean <- tidy(airbnb_model_1, effects = "fixed")
model_2_mean <- tidy(airbnb_model_2, effects = "fixed")

# Combine the summaries for both models
combined_summaries <- model_1_mean %>% 
  right_join(., model_2_mean, by = "term",
             suffix = c("_model_1", "_model_2")) %>% 
  select(-starts_with("std.error"))

# Get variance summaries for both models
model_1_var <- tidy(airbnb_model_1, effects = "ran_pars")
model_2_var <- tidy(airbnb_model_2, effects = "ran_pars")

# Combine the summaries for both models
model_1_var %>% 
  right_join(., model_2_var, by = "term",
             suffix = c("_model_1", "_model_2")) %>% 
  select(-starts_with("group"))

#Posterior group level analysis
nbhd_features %>% 
  filter(neighborhood %in% c("Edgewater", "Pullman"))

combined_summaries %>% 
  filter(term %in% c("(Intercept)", "walk_score"))

# Get neighborhood summaries from both models
model_1_nbhd <- tidy(airbnb_model_1, effects = "ran_vals")
model_2_nbhd <- tidy(airbnb_model_2, effects = "ran_vals")

# Combine the summaries for both models
model_1_nbhd %>% 
  right_join(., model_2_nbhd, by = "level",
             suffix = c("_model_1", "_model_2")) %>% 
  select(-starts_with(c("group", "term", "std.error"))) %>% 
  filter(level %in% c("Edgewater", "Pullman"))

nbhd_features %>% 
  filter(neighborhood %in% c("Edgewater", "Pullman"))

# Import, rename, & clean data
data(climbers_sub)
climbers <- climbers_sub %>% 
  select(peak_name, expedition_id, member_id, success,
         year, season, age, expedition_role, oxygen_used)

# Summarize expeditions
expeditions <- climbers %>% 
  group_by(peak_name, expedition_id) %>% 
  summarize(n_climbers = n())

head(expeditions, 2)

# Summarize peaks
peaks <- expeditions %>% 
  group_by(peak_name) %>% 
  summarize(n_expeditions = n(), n_climbers = sum(n_climbers))

head(peaks, 2)
  
#Build a model with 2 grouping variables
climbers %>% 
  group_by(peak_name) %>% 
  summarize(p_success = mean(success)) %>% 
  ggplot(., aes(x = p_success)) + 
  geom_histogram(color = "white")

#Build model with varying intercept for expedition
climb_model_1 <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id), 
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135)

#Adding a second model with a varying intercept for peak
climb_model_2 <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id) + (1 | peak_name), 
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 135)

# Get trend summaries for both models
climb_model_1_mean <- tidy(climb_model_1, effects = "fixed")
climb_model_2_mean <- tidy(climb_model_2, effects = "fixed")

# Combine the summaries for both models
climb_model_1_mean %>%
  right_join(., climb_model_2_mean, by ="term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("std.error"))

# Get variance summaries for both models
climb_model_1_var <- tidy(climb_model_1, effects = "ran_pars")
climb_model_2_var <- tidy(climb_model_2, effects = "ran_pars")

# Combine the summaries for both models
climb_model_1_var %>% 
  right_join(., climb_model_2_var, by = "term",
             suffix =c("_model_1", "_model_2")) %>%
  select(-starts_with("group"))

# Global regression parameters
climb_model_2_mean %>% 
  select(term, estimate)

# Group-level terms
group_levels_2 <- tidy(climb_model_2, effects = "ran_vals") %>% 
  select(level, group, estimate)

group_levels_2 %>% 
  filter(group == "peak_name") %>% 
  head(2) 

group_levels_2 %>% 
  filter(group == "expedition_id") %>% 
  head(2)
