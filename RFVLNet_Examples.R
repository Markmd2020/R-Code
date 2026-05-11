#Random Vector Functional Link (RVFL) Networks


#Load libraries
library(survival)
library(glmnet)
library(rvflnet)

#Regression Example
set.seed(135)
data(Boston, package = "MASS")

# -------------------------
# Data
# -------------------------
X <- as.matrix(Boston[, -14])
y <- Boston$medv

n <- nrow(X)
idx <- sample(1:n, size = round(0.8 * n))

X_train <- X[idx, ]
y_train <- y[idx]

X_test <- X[-idx, ]
y_test <- y[-idx]

# -------------------------
# Grid
# -------------------------
grid <- expand.grid(
  n_hidden = c(175, 200, 225, 250),
  alpha = seq(0.1, 0.5, by=0.2),
  include_original = c(TRUE, FALSE),
  seed = 1,
  stringsAsFactors = FALSE
)

results <- vector("list", nrow(grid))

# -------------------------
# Loop
# -------------------------
for (i in seq_len(nrow(grid))) {
  
  params <- grid[i, ]
  
  #cat("\n========================================\n")
  #cat(sprintf("Run %d / %d\n", i, nrow(grid)))
  #print(params)
  
  # -------------------------
  # Fit model
  # -------------------------
  fit <- rvflnet(
    X_train, y_train,
    n_hidden = params$n_hidden,
    activation = "sigmoid",
    W_type = "gaussian",
    seed = params$seed,
    include_original = params$include_original, # direct link, skip connection or not
    alpha = params$alpha
  )
  
  # -------------------------
  # Evaluate full lambda path
  # -------------------------
  lambdas <- fit$fit$lambda
  
  preds <- predict(fit, newx = X_test, s = lambdas)
  
  rmse_path <- sqrt(colMeans((preds - y_test)^2))
  
  best_idx <- which.min(rmse_path)
  
  best_rmse <- rmse_path[best_idx]
  best_lambda <- lambdas[best_idx]
  
  # -------------------------
  # Sparsity
  # -------------------------
  coef_mat <- coef(fit, s = best_lambda)
  nonzero <- sum(coef_mat[-1, 1] != 0)
  
  # -------------------------
  # Verbose output
  # -------------------------
  #cat(sprintf("Best RMSE: %.4f\n", best_rmse))
  #cat(sprintf("Best lambda: %.6f\n", best_lambda))
  #cat(sprintf("Non-zero coeffs: %d\n", nonzero))
  
  # -------------------------
  # Store
  # -------------------------
  results[[i]] <- data.frame(
    n_hidden = params$n_hidden,
    alpha = params$alpha,
    include_original = params$include_original,
    seed = params$seed,
    rmse = best_rmse,
    lambda = best_lambda,
    nonzero = nonzero
  )
}

# -------------------------
# Aggregate
# -------------------------
results_df <- do.call(rbind, results)
results_df <- results_df[order(results_df$rmse), ]
print(head(results_df))

set.seed(123)


#Binary Classification
data(iris)

# Binary classification: setosa vs others
y <- ifelse(iris$Species == "setosa", 1, 0)
X <- as.matrix(iris[, 1:4])

# Train/test split
n <- nrow(X)
idx <- sample(1:n, size = round(0.8 * n))

X_train <- X[idx, ]
y_train <- y[idx]

X_test <- X[-idx, ]
y_test <- y[-idx]

# -------------------------
# Fit model
# -------------------------
cv_model <- cv.rvflnet(
  X_train, y_train,
  n_hidden = 50,
  activation = "relu",
  W_type = "gaussian",
  family = "binomial",
  nfolds = 5
)

# -------------------------
# Predictions (probabilities)
# -------------------------
(probs <- predict(cv_model, X_test, type = "response"))

# Convert to class
y_pred <- ifelse(probs > 0.5, 1, 0)

all.equal(as.numeric(y_pred), as.numeric(predict(cv_model, X_test, type="class")))

# -------------------------
# Diagnostics
# -------------------------

# Accuracy
acc <- mean(drop(y_pred) == y_test)
cat("Accuracy:", acc, "\n")

# Confusion matrix
table(Predicted = y_pred, Actual = y_test)

#MultiClass Classification
y <- as.numeric(iris$Species)
X <- as.matrix(iris[, 1:4])
# Train/test split
n <- nrow(X)
idx <- sample(1:n, size = round(0.8 * n))
X_train <- X[idx, ]
y_train <- y[idx]
X_test <- X[-idx, ]
y_test <- y[-idx]
# -------------------------
# Fit model
# -------------------------
cv_model <- rvflnet(
  X_train, y_train,
  n_hidden = 50,
  activation = "relu",
  W_type = "gaussian",
  family = "multinomial",
  nlambda = 25,
  nfolds = 5
)
# -------------------------
# Diagnostics
# -------------------------
# Accuracy
acc <- colMeans(predict(cv_model, X_test, type="class") == y_test)
cat("Accuracies:", acc, "\n") # consider other metrics

####Non linear Cox Survival Model###
data(ovarian)
X <- as.matrix(ovarian[, c("age", "resid.ds", "rx", "ecog.ps")])
y <- Surv(ovarian$futime, ovarian$fustat)
set.seed(123)
n <- nrow(X)
train_idx <- sample(1:n, size = round(0.8 * n))
X_train <- X[train_idx, ]
X_test  <- X[-train_idx, ]
y_train <- y[train_idx]
y_test  <- y[-train_idx]
# -------------------------
# Fit model
# -------------------------
cv_fit <- cv.rvflnet(
  X_train, y_train,
  family = "cox",
  nfolds = 5,
  type.measure = "C"
)
plot(cv_fit)
# Out-of-sample C-index
print(Cindex(pred = predict(cv_fit, X_test), y = y_test))

#Another Cox Model Example
data(pbc)
pbc2       <- pbc[!is.na(pbc$trt), ]
pbc2$event <- as.integer(pbc$status[!is.na(pbc$trt)] == 2)
pbc2$sex_n <- as.integer(pbc2$sex == "f")
feat_cols <- c("trt","age","sex_n","ascites","hepato","spiders","edema",
               "bili","chol","albumin","copper","alk.phos","ast",
               "trig","platelet","protime","stage")
df <- pbc2[, c("time", "event", feat_cols)]
for (col in feat_cols)
  if (any(is.na(df[[col]])))
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
set.seed(135)
idx_train <- sample(nrow(df), floor(0.75 * nrow(df)))
train <- df[idx_train, ]; test <- df[-idx_train, ]
X_tr  <- as.matrix(train[, feat_cols])
X_te  <- as.matrix(test[,  feat_cols])
y_tr   <- Surv(train$time, train$event)
fit <- rvflnet(
  X_tr, y_tr,
  family = "cox",
  alpha=0.1, lambda=0.1 # not recommended
)
y_te   <- Surv(test$time, test$event)
ci <- Cindex(predict(fit, X_te), y_te)
cat("\n=== Test-set C-index ===\n")
print(ci) 

#Fit Model
fit <- rvflnet(
  X_tr, y_tr,
  family = "cox",
  alpha=0.1, nlambda=50
)
y_te   <- Surv(test$time, test$event)
(cis <- apply(predict(fit, X_te), 2, function(x) glmnet::Cindex(x, y_te)))
#cat("\n=== Test-set C-index ===\n")
plot(log(fit$fit$lambda), cis, type = 'l')
abline(h=0.8, lty=2, col="red")
