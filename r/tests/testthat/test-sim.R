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
