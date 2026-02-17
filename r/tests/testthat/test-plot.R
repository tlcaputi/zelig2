test_that("plot requires sim results", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = 150)
  expect_error(plot(z), "No simulation results")
})

test_that("plot returns ggplot for continuous model", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  p <- plot(z)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot returns ggplot for binary model", {
  set.seed(42)
  n <- 200
  df <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n))
  z <- zelig2(y ~ x, model = "logit", data = df, num = 100L)
  z <- setx(z, x = 0)
  z <- sim(z)
  p <- plot(z)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot works for range scenarios", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = seq(100, 200, by = 25))
  z <- sim(z)
  p <- plot(z)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot works with first differences", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 100)
  z <- setx1(z, hp = 200)
  z <- sim(z)
  p <- plot(z)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot respects ci parameter", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  # Should not error with different CI levels
  p90 <- plot(z, ci = 0.90)
  p99 <- plot(z, ci = 0.99)
  expect_true(inherits(p90, "gg") || inherits(p90, "patchwork"))
  expect_true(inherits(p99, "gg") || inherits(p99, "patchwork"))
})
