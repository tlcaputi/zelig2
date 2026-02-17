test_that("tobit model fits and runs sim", {
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  y_star <- 2 + 3 * x + rnorm(n)
  y <- pmax(y_star, 0)
  df <- data.frame(y = y, x = x)
  z <- zelig2(y ~ x, model = "tobit", data = df, num = 100L)
  expect_s3_class(z, "zelig2")
  expect_true(length(z$coef) >= 2)  # intercept + x (+ log(scale))
  z <- setx(z, x = 0)
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})

test_that("tobit matches AER::tobit", {
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  y <- pmax(2 + 3 * x + rnorm(n), 0)
  df <- data.frame(y = y, x = x)
  z <- zelig2(y ~ x, model = "tobit", data = df)
  raw <- AER::tobit(y ~ x, data = df)
  # Compare main coefficients (not log(scale))
  expect_equal(unname(coef(z)["x"]), unname(coef(raw)["x"]),
               tolerance = 1e-6)
})

test_that("quantile model fits and runs sim", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "quantile", data = mtcars, num = 100L)
  expect_s3_class(z, "zelig2")
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})

test_that("quantile matches quantreg::rq", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "quantile", data = mtcars)
  raw <- quantreg::rq(mpg ~ hp + wt, data = mtcars, tau = 0.5)
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-6)
})

test_that("tobit predicted values are censored at 0", {
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  y <- pmax(2 + 3 * x + rnorm(n), 0)
  df <- data.frame(y = y, x = x)
  z <- zelig2(y ~ x, model = "tobit", data = df, num = 500L)
  z <- setx(z, x = -2)  # x far negative -> many censored predictions
  z <- sim(z)
  expect_true(all(z$sim_out$pv >= 0))
})
