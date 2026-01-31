test_that("Random Sanity Check Test", {

  data <- read.csv("power_train.csv")
  target_col <- "Total_Power"

  y <- as.numeric(data[[target_col]])
  X <- as.matrix(data[, !(names(data) %in% target_col)])

  N <- nrow(X)
  p <- ncol(X)

  start <- Sys.time()
  res <- CLASS2(X, y, nSample = 500, nTimes = 30, k = 5000)
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
