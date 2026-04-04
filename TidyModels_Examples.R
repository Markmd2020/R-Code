##########Tidy Models Examples#####

#Load libraries
library(tidymodels)
library(rstanarm)
library(tidyposterior)
library(baguette)
library(discrim)
library(beans)
library(embed)
library(DALEXtra)
library(forcats)
library(stacks)
library(finetune)
library(rules)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pscl)
library(infer)
library(poissonreg)
library(rsample)
library(purrr)
library(generics)
library(lmtest)

#Set seed for reproducibility
set.seed(135)

#Load data
data(ames)

#Prepare Data
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

#Split data into train and test
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

#Resample to evaluate model performance
rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

rf_fit <- rf_wflow %>% fit(data = ames_train)
lm_fit <- rf_wflow %>% fit(data = ames_train)

estimate_perf <- function(model, dat) {
  # Capture the names of the `model` and `dat` objects
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(Sale_Price)) %>%
    reg_metrics(Sale_Price, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}

#Train Data Evaluation
estimate_perf(rf_fit, ames_train)
estimate_perf(lm_fit, ames_train)

#Test Data Evaluation
estimate_perf(rf_fit, ames_test)
estimate_perf(lm_fit, ames_test)

#Using resampling to evaluate models
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <-  
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(linear_reg() %>% set_engine("lm")) 

lm_fit <- lm_wflow %>% fit(data = ames_train)


# Select the recipe: 
extract_recipe(lm_fit, estimated = TRUE)

#Retrieve coefficients with function
get_model <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

get_model(lm_fit)

ctrl <- control_resamples(extract = get_model)
ames_folds <- vfold_cv(ames_train, v = 10)

lm_res <- lm_wflow %>%  fit_resamples(resamples = ames_folds, control = ctrl)

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)

#Comparing model with resampling

#Compare diifferent preprocessing steps
basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_")) 

spline_rec <- 
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, list(lm = linear_reg()), cross = FALSE)
lm_models

lm_models <- 
  lm_models %>% 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               seed = 1101, verbose = TRUE,
               # Options to `fit_resamples()`: 
               resamples = ames_folds, control = keep_pred)

collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")

#Adding a random forest to the workflow
four_models <- 
  as_workflow_set(random_forest = rf_res) %>% 
  bind_rows(lm_models)

#Bayesian Method For Evaluation
rsq_anova <- perf_mod(
    four_models,
    metric = "rsq",
    prior_intercept = rstanarm::student_t(df = 1),
    chains = 4,
    iter = 5000,
    seed = 135
  )

model_post <- 
  rsq_anova %>% 
  # Take a random sample from the posterior distribution
  # so set the seed again to be reproducible. 
  tidy(seed = 135) 

glimpse(model_post)

#Iterative Search Examples

#Load data
data(cells)
cells <- cells %>% select(-case)
cell_folds <- vfold_cv(cells)
roc_res <- metric_set(roc_auc)

svm_rec <- 
  recipe(class ~ ., data = cells) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

svm_wflow <- 
  workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(svm_rec)

#Check default parameters
cost()
rbf_sigma()

svm_param <- 
  svm_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(rbf_sigma = rbf_sigma(c(-7, -1)))

#Hyperparameter tuning
start_grid <- 
  svm_param %>% 
  update(
    cost = cost(c(-6, 1)),
    rbf_sigma = rbf_sigma(c(-6, -4))
  ) %>% 
  grid_regular(levels = 2)

svm_initial <- 
  svm_wflow %>% 
  tune_grid(resamples = cell_folds, grid = start_grid, metrics = roc_res)

collect_metrics(svm_initial)

#Bayesian HyperParameter Tuning
ctrl <- control_bayes(verbose = TRUE)

svm_bo <-
  svm_wflow %>%
  tune_bayes(
    resamples = cell_folds,
    metrics = roc_res,
    initial = svm_initial,
    param_info = svm_param,
    iter = 25,
    control = ctrl
  )

show_best(svm_bo)
autoplot(svm_bo, type = "performance")

#Comparing combination of models and preprocessing steps performance

bean_split <- initial_validation_split(beans, strata = class, 
                                       prop = c(0.75, 0.125))

# Return data frames for model development
bean_train <- training(bean_split)
bean_test <- testing(bean_split)
bean_validation <- validation(bean_split)

# Return an 'rset' object to use with the tune functions:
bean_val <- validation_set(bean_split)

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')

bagging_spec <-
  bag_tree() %>%
  set_engine('rpart') %>%
  set_mode('classification')

fda_spec <-
  discrim_flexible(
    prod_degree = tune()
  ) %>%
  set_engine('earth')

rda_spec <-
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
  set_engine('klaR')

#Recipes
bean_rec <-
  recipe(class ~ ., data = bean_train) %>%
  step_zv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())


umap_rec <-
  bean_rec %>%
  step_umap(
    all_numeric_predictors(),
    outcome = "class",
    num_comp = tune(),
    neighbors = tune(),
    min_dist = tune()
  )


ctrl <- control_grid(parallel_over = "everything")
bean_res <- 
  workflow_set(
    preproc = list(basic = class ~.,umap = umap_rec), 
    models = list(fda = fda_spec,
                  rda = rda_spec, bag = bagging_spec,
                  mlp = mlp_spec)
  ) %>% 
  workflow_map(
    verbose = TRUE,
    seed = 135,
    resamples = bean_val,
    grid = 10,
    metrics = metric_set(roc_auc),
    control = ctrl
  )

rankings <- 
  rank_results(bean_res, select_best = TRUE) %>% 
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1])) 

filter(rankings, rank <= 3) %>% dplyr::select(rank, mean, model, method)

rda_res <- 
  bean_res %>% 
  extract_workflow("umap_fda") %>% 
  finalize_workflow(
    bean_res %>% 
      extract_workflow_set_result("umap_fda") %>% 
      select_best(metric = "roc_auc")
  ) %>% 
  last_fit(split = bean_split, metrics = metric_set(roc_auc))

rda_wflow_fit <- extract_workflow(rda_res)

#Encoding Categorical Variables
#Use GLM to estimate the effect of each categorical level
ames_glm <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_lencode_glm(Neighborhood, outcome = vars(Sale_Price)) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

glm_estimates <-
  prep(ames_glm) %>%
  tidy(number = 2)

glm_estimates %>%
  filter(level == "..new")

#Use Partial Pooling to estimate the effect of each categorical level
ames_mixed <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_lencode_mixed(Neighborhood, outcome = vars(Sale_Price)) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

mixed_estimates <-
  prep(ames_mixed) %>%
  tidy(number = 2)

mixed_estimates %>%
  filter(level == "..new")

glm_estimates %>%
  rename(`no pooling` = value) %>%
  left_join(
    mixed_estimates %>%
      rename(`partial pooling` = value), by = "level"
  ) %>%
  left_join(
    ames_train %>% 
      count(Neighborhood) %>% 
      mutate(level = as.character(Neighborhood))
  ) %>%
  ggplot(aes(`no pooling`, `partial pooling`, size = sqrt(n))) +
  geom_abline(color = "gray50", lty = 2) +
  geom_point(alpha = 0.7) +
  coord_fixed()


#Model  Explanations
vip_features <- c("Neighborhood", "Gr_Liv_Area", "Year_Built", 
                  "Bldg_Type", "Latitude", "Longitude")

vip_train <- 
  ames_train %>% 
  select(all_of(vip_features))

explainer_lm <- 
  explain_tidymodels(
    lm_fit, 
    data = vip_train, 
    y = ames_train$Sale_Price,
    label = "lm + interactions",
    verbose = FALSE
  )

explainer_rf <- 
  explain_tidymodels(
    rf_fit, 
    data = vip_train, 
    y = ames_train$Sale_Price,
    label = "random forest",
    verbose = FALSE
  )

#Local Explanations
duplex <- vip_train[120,]
lm_breakdown <- predict_parts(explainer = explainer_lm, 
                              new_observation = duplex)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = duplex)

#Global Explanations
vip_lm <- model_parts(explainer_lm, loss_function = loss_root_mean_square)
vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)

#Plotting Global Explanations
ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable)) 
  if(length(obj) > 1) {
    p <- p + 
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}

ggplot_imp(vip_lm, vip_rf)

#Ensemble of Models
glimpse(concrete)

#Data Preprocessing
concrete <- 
  concrete %>% 
  group_by(across(-compressive_strength)) %>% 
  summarize(compressive_strength = mean(compressive_strength),
            .groups = "drop")
nrow(concrete)

concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test  <- testing(concrete_split)

concrete_folds <- 
  vfold_cv(concrete_train, strata = compressive_strength, repeats = 5)

#Create two recipes
normalized_rec <- 
  recipe(compressive_strength ~ ., data = concrete_train) %>% 
  step_normalize(all_predictors()) 

poly_recipe <- 
  normalized_rec %>% 
  step_poly(all_predictors()) %>% 
  step_interact(~ all_predictors():all_predictors())

#Setup Model Specifications
linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("regression")

mars_spec <- 
  mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
  set_engine("earth") %>% 
  set_mode("regression")

svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

knn_spec <- 
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

bag_cart_spec <- 
  bag_tree() %>% 
  set_engine("rpart", times = 50L) %>% 
  set_mode("regression")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

cubist_spec <- 
  cubist_rules(committees = tune(), neighbors = tune()) %>% 
  set_engine("Cubist") 

nnet_param <- 
  nnet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(1, 27)))

normalized <- 
  workflow_set(
    preproc = list(normalized = normalized_rec), 
    models = list(SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, 
                  KNN = knn_spec, neural_network = nnet_spec)
  )

normalized %>% extract_workflow(id = "normalized_KNN")

normalized <- 
  normalized %>% 
  option_add(param_info = nnet_param, id = "normalized_neural_network")

model_vars <- 
  workflow_variables(outcomes = compressive_strength, 
                     predictors = everything())

no_pre_proc <- 
  workflow_set(
    preproc = list(simple = model_vars), 
    models = list(MARS = mars_spec, CART = cart_spec, CART_bagged = bag_cart_spec,
                  RF = rf_spec, boosting = xgb_spec, Cubist = cubist_spec)
  )

with_features <- 
  workflow_set(
    preproc = list(full_quad = poly_recipe), 
    models = list(linear_reg = linear_reg_spec, KNN = knn_spec)
  )

all_workflows <- 
  bind_rows(no_pre_proc, normalized, with_features) %>% 
  # Make the workflow ID's a little more simple: 
  mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))

#Run multiple models by racing
race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_results <-
  all_workflows %>%
  workflow_map(
    "tune_race_anova",
    seed = 135,
    resamples = concrete_folds,
    grid = 25,
    control = race_ctrl
  )

#Model Stacking
concrete_stack <- 
  stacks() %>% 
  add_candidates(race_results)
ens <- blend_predictions(concrete_stack)
autoplot(ens)

#Evaluate the model complexity penalty
ens <- blend_predictions(concrete_stack, penalty = 10^seq(-2, -0.5, length = 20))
autoplot(ens)

autoplot(ens, "weights") +
  geom_text(aes(x = weight + 0.01, label = model), hjust = 0) + 
  theme(legend.position = "none") +
  lims(x = c(-0.01, 0.8))

#Print details of metalearner
ens 

autoplot(ens, "weights") +
  geom_text(aes(x = weight + 0.01, label = model), hjust = 0) + 
  theme(legend.position = "none") +
  lims(x = c(-0.01, 0.8))

#Fit ensemble of models

reg_metrics <- metric_set(rmse, rsq)
ens_test_pred <- 
  predict(ens, concrete_test) %>% 
  bind_cols(concrete_test)

ens_test_pred %>% 
  reg_metrics(compressive_strength, .pred)

#Inferential Analysis
ggplot(bioChemists, aes(x = art)) + 
  geom_histogram(binwidth = 1, color = "white") + 
  labs(x = "Number of articles within 3y of graduation")

bioChemists %>% 
  group_by(fem) %>% 
  summarise(counts = sum(art), n = length(art))

poisson.test(c(930, 619), T = 3)

observed <- 
  bioChemists %>%
  infer::specify(art ~ fem) %>%
  infer::calculate(stat = "diff in means", order = c("Men", "Women"))

bootstrapped <- 
  bioChemists %>%
  infer::specify(art ~ fem)  %>%
  infer::generate(reps = 2000, type = "bootstrap") %>%
  infer::calculate(stat = "diff in means", order = c("Men", "Women"))

percentile_ci <- get_ci(bootstrapped)

infer::visualize(bootstrapped) +
  shade_confidence_interval(endpoints = percentile_ci)

#Using permutation for hypothesis testing
permuted <- 
  bioChemists %>%
  infer::specify(art ~ fem)  %>%
  infer::hypothesize(null = "independence") %>%
  infer::generate(reps = 2000, type = "permute") %>%
  infer::calculate(stat = "diff in means", order = c("Men", "Women"))

infer::visualize(permuted) +
  shade_p_value(obs_stat = observed, direction = "two-sided")

permuted %>%
  get_p_value(obs_stat = observed, direction = "two-sided")

#Building a poisson regression
log_lin_spec <- poisson_reg()

log_lin_fit <- 
  log_lin_spec %>% 
  fit(art ~ ., data = bioChemists)

#Calculate bootstrap confidence intervals
glm_boot <- reg_intervals(art ~ ., data = bioChemists, 
                          model_fn = "glm", family = poisson)

#Use Likelihood Ratio Test To Compare Model Complexity
log_lin_reduced <- 
  log_lin_spec %>% 
  fit(art ~ ment + kid5 + fem + mar, data = bioChemists)

anova(
  extract_fit_engine(log_lin_reduced),
  extract_fit_engine(log_lin_fit),
  test = "LRT"
)

#Build a Zero Inflated Poisson model
zero_inflated_spec <- poisson_reg() %>% set_engine("zeroinfl")

zero_inflated_fit <- 
  zero_inflated_spec %>% 
  fit(art ~ fem + mar + kid5 + ment | fem + mar + kid5 + phd + ment,
      data = bioChemists)

anova(
  extract_fit_engine(zero_inflated_fit),
  extract_fit_engine(log_lin_reduced),
  test = "LRT"
)

#Other approach for calculating LRT
lrtest(
  extract_fit_engine(zero_inflated_fit),
  extract_fit_engine(log_lin_reduced)
)

#Compare models AIC
zero_inflated_fit %>% extract_fit_engine() %>% AIC()
log_lin_reduced   %>% extract_fit_engine() %>% AIC()

#Fit models through bootsrap models
zip_form <- art ~ fem + mar + kid5 + ment | fem + mar + kid5 + phd + ment
glm_form <- art ~ fem + mar + kid5 + ment

bootstrap_models <-
  bootstraps(bioChemists, times = 2000, apparent = TRUE) %>%
  mutate(
    glm = map(splits, ~ fit(log_lin_spec,       glm_form, data = analysis(.x))),
    zip = map(splits, ~ fit(zero_inflated_spec, zip_form, data = analysis(.x)))
  )


bootstrap_models <-
  bootstrap_models %>%
  mutate(
    glm_aic = map_dbl(glm, ~ extract_fit_engine(.x) %>% AIC()),
    zip_aic = map_dbl(zip, ~ extract_fit_engine(.x) %>% AIC())
  )
mean(bootstrap_models$zip_aic < bootstrap_models$glm_aic)

#Obtain coefficients for zero estimates
bootstrap_models <-
  bootstrap_models %>%
  mutate(zero_coefs  = map(zip, ~ tidy(.x, type = "zero")))

bootstrap_models$zero_coefs 

bootstrap_models %>% 
  unnest(zero_coefs) %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 25, color = "white") + 
  facet_wrap(~ term, scales = "free_x") + 
  geom_vline(xintercept = 0, lty = 2, color = "gray70")

bootstrap_models %>% int_pctl(zero_coefs)
bootstrap_models %>% int_t(zero_coefs)