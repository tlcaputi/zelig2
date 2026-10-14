test_that("sim requires setx first", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  expect_error(sim(z), "No scenario set")
})

test_that("sim produces expected number of draws", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 500L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_length(z$sim_out$ev, 500)
  expect_length(z$sim_out$pv, 500)
})

test_that("sim with custom num overrides default", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 1000L)
  z <- setx(z, hp = 150)
  z <- sim(z, num = 200L)
  expect_length(z$sim_out$ev, 200)
})

test_that("sim produces first differences with setx1", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 500L)
  z <- setx(z, hp = 100)
  z <- setx1(z, hp = 200)
  z <- sim(z)
  expect_true(!is.null(z$sim_out$fd))
  expect_length(z$sim_out$fd, 500)
  # First difference should be negative (more hp -> lower mpg)
  expect_true(mean(z$sim_out$fd) < 0)
})

test_that("sim produces risk ratios for binary models", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  z <- zelig2(y ~ x1 + x2, model = "logit", data = df, num = 500L)
  z <- setx(z, x1 = 0)
  z <- setx1(z, x1 = 1)
  z <- sim(z)
  expect_true(!is.null(z$sim_out$rr))
  expect_length(z$sim_out$rr, 500)
})

test_that("sim with range scenario produces matrix output", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 200L)
  z <- setx(z, hp = seq(100, 200, by = 25))
  z <- sim(z)
  expect_true(is.matrix(z$sim_out$ev))
  expect_equal(nrow(z$sim_out$ev), 200)
  expect_equal(ncol(z$sim_out$ev), 5)  # 100,125,150,175,200
})

test_that("sim EV is approximately correct for OLS", {
  set.seed(123)
  n <- 1000
  x <- rnorm(n)
  y <- 2 + 3 * x + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x = x)
  z <- zelig2(y ~ x, model = "ls", data = df, num = 5000L)
  z <- setx(z, x = 1)
  z <- sim(z)
  # EV at x=1 should be approximately 2 + 3*1 = 5

  expect_true(abs(mean(z$sim_out$ev) - 5) < 0.2)
})

test_that("parameter draws have the target mean and covariance", {
  coefs <- c(a = 1, b = -2, c = 0.5)
  V <- matrix(c(1, 0.3, 0.1, 0.3, 2, -0.4, 0.1, -0.4, 0.5), 3)
  set.seed(1)
  d <- param_fn_mvn(coefs, V, 200000)
  expect_equal(dim(d), c(200000L, 3L))
  expect_equal(colnames(d), names(coefs))
  expect_equal(unname(colMeans(d)), unname(coefs), tolerance = 0.01)
  expect_equal(unname(cov(d)), V, tolerance = 0.02)
})

test_that("parameter draws are stable to rounding-level changes in V", {
  coefs <- c(a = 1, b = -2, c = 0.5)
  V <- matrix(c(1, 0.3, 0.1, 0.3, 2, -0.4, 0.1, -0.4, 0.5), 3)
  set.seed(9); d1 <- param_fn_mvn(coefs, V, 1000)
  set.seed(9); d2 <- param_fn_mvn(coefs, V * (1 + 1e-13), 1000)
  expect_lt(max(abs(d1 - d2)), 1e-10)
})

test_that("a covariance matrix that is not positive definite falls back with a warning", {
  coefs <- c(a = 0, b = 0)
  V <- matrix(c(1, 1, 1, 1), 2)
  set.seed(3)
  expect_warning(d <- param_fn_mvn(coefs, V, 100), "not positive definite")
  expect_equal(dim(d), c(100L, 2L))
})

test_that("draws = \"eigen\" reproduces MASS::mvrnorm", {
  coefs <- c(a = 1, b = -2, c = 0.5)
  V <- matrix(c(1, 0.3, 0.1, 0.3, 2, -0.4, 0.1, -0.4, 0.5), 3)
  set.seed(4); d1 <- param_fn_mvn(coefs, V, 500, draws = "eigen")
  set.seed(4); d2 <- MASS::mvrnorm(500, coefs, V)
  expect_identical(attr(d1, "draws"), "eigen")
  attr(d1, "draws") <- NULL
  expect_identical(d1, d2)
  set.seed(1)
  d <- param_fn_mvn(coefs, V, 200000, draws = "eigen")
  expect_equal(unname(cov(d)), V, tolerance = 0.02)
})

test_that("sim() passes draws through and records it", {
  set.seed(42)
  df <- data.frame(y = rbinom(300, 1, 0.4), x = rnorm(300))
  z <- setx(zelig2(y ~ x, model = "logit", data = df), x = 0)
  set.seed(5); zc <- sim(z, num = 300L)
  set.seed(5); ze <- sim(z, num = 300L, draws = "eigen")
  expect_identical(zc$sim_out$draws, "cholesky")
  expect_identical(ze$sim_out$draws, "eigen")
  set.seed(5); p <- MASS::mvrnorm(300L, z$coef, z$vcov)
  expect_equal(ze$sim_out$ev, as.numeric(plogis(p %*% c(1, 0))))
  expect_error(sim(z, draws = "svd"), "should be one of")
})

test_that("the draw method actually used is recorded, including the fallback", {
  coefs <- c(a = 0, b = 0)
  expect_identical(attr(param_fn_mvn(coefs, diag(2), 10), "draws"), "cholesky")
  expect_identical(attr(param_fn_mvn(coefs, diag(2), 10, draws = "eigen"), "draws"), "eigen")
  expect_warning(d <- param_fn_mvn(coefs, matrix(1, 2, 2), 10), "not positive definite")
  expect_identical(attr(d, "draws"), "eigen (fallback)")
})

test_that("NA in the coefficients or covariance matrix is an error", {
  expect_error(param_fn_mvn(c(a = 0, b = NA), diag(2), 10), "contain NA")
  expect_error(param_fn_mvn(c(a = 0, b = 0), matrix(c(1, NA, NA, 1), 2), 10), "contain NA")
})
