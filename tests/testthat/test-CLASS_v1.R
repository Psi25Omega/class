test_that("Random Sanity Check Test", {

  data <- read.csv("power_train.csv")
  target_col <- "Total_Power"

  y <- as.numeric(data[[target_col]])
  X <- as.matrix(data[, !(names(data) %in% target_col)])

  N <- nrow(X)
  p <- ncol(X)

  start <- Sys.time()
  res <- CLASS(X, y, nSample = 500, nTimes = 30, k = 5000)
  end <- Sys.time()

  expect_true(length(res$feature_counts) == p)

  print(res$feature_counts)
  cat("\nTime taken:", end - start, "\n")
  cat("R2:", res$r_squared, "\n", "MSE:", res$mse, "\n")

  data_test <- read.csv("power_test.csv")
  y_test <- as.numeric(data_test[[target_col]])

  X_test_unpadded <- as.matrix(data_test[, !names(data_test) %in% target_col])
  X_test = cbind(1, X_test_unpadded)

  beta_calc = res$beta

  y_pred_test = X_test %*% beta_calc
  residual = y_test - y_pred_test
  mse <- mean(residual^2)
  r_squared = 1 - sum(residual^2)/sum((y_test - mean(y_test))^2)

  cat("\n Test MSE:", mse, "\n")
  cat("\n Test R^2:", r_squared, "\n")
})

# test_that("time test", {
#   iter <- 5
#   total_time <- 0
#   for (i in 1:iter) {
#     N <- 50000
#     p <- 250

#     X = matrix(rnorm(N * p), N, p)
#     y = rnorm(N)

#     start <- Sys.time()
#     res <- CLASS(X, y, nSample = 5000, nTimes = 50, k = 50000)
#     end <- Sys.time()
#     total_time <- total_time + (end - start)
#   }
#   average_time <- total_time / iter
#   print(average_time)
# })

# test_that("Active variables are actually selected", {
#   N <- 50000
#   p <- 10
#   set.seed(42)
#   beta_gen <- c(2, 3, 4, 5, 7, 1, 0, 0, 0, 0)
#   X <- matrix(rnorm(N * p), N, p)
#   epsilon <- rnorm(N, mean = 0, sd = 0.1)
#   y <- X %*% beta_gen + epsilon
#   start <- Sys.time()
#   res <- CLASS(X, y, nSample = 5000, nTimes = 50, k = 8000)
#   end <- Sys.time()
#   print(res$feature_counts)
#   beta <- res$beta
#   expect_true(beta[8] == 0)
#   expect_true(beta[9] == 0)
#   expect_true(beta[10] == 0)
#   expect_true(beta[11] == 0)
#   cat(res$selected_indices)
#   cat("\nTime taken:", end - start, "\n")
# })

# test_that("Subsampling is different each time", {

# })

