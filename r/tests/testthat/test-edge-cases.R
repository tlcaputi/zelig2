test_that("handles NAs in data", {
  df <- mtcars
  df$hp[c(1, 5, 10)] <- NA
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = df)
  expect_s3_class(z, "zelig2")
  # Should fit on complete cases
  raw <- glm(mpg ~ hp + wt, data = df, family = gaussian())
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("intercept-only model works", {
  data(mtcars)
  z <- zelig2(mpg ~ 1, model = "ls", data = mtcars, num = 100L)
  expect_length(z$coef, 1)
  z <- setx(z)
  z <- sim(z)
  # EV should be close to mean(mpg)
  expect_true(abs(mean(z$sim_out$ev) - mean(mtcars$mpg)) < 1)
})

test_that("dot formula works", {
  # y ~ . should expand to all other columns
  df <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  z <- zelig2(y ~ ., model = "ls", data = df)
  expect_equal(length(z$coef), 3)  # intercept + x1 + x2
})

test_that("single predictor model works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})

test_that("coef and vcov S3 methods work", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  c <- coef(z)
  v <- vcov(z)
  expect_length(c, 3)
  expect_equal(dim(v), c(3, 3))
  expect_equal(names(c), rownames(v))
})

test_that("print method works without error", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  expect_output(print(z), "zelig2")
  expect_output(print(z), "Formula")
})

test_that("summary method works without error", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_output(s <- summary(z), "Simulation Summary")
  expect_true(is.list(s))
  expect_true("coef_table" %in% names(s))
})

test_that("collinear predictors handled gracefully", {
  df <- data.frame(y = rnorm(50), x1 = rnorm(50))
  df$x2 <- df$x1 * 2  # perfectly collinear
  # This should either work (with NA coefs) or error informatively
  # glm handles this by setting one coef to NA
  z <- zelig2(y ~ x1 + x2, model = "ls", data = df)
  expect_s3_class(z, "zelig2")
})

test_that("large num works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp, model = "ls", data = mtcars, num = 5000L)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_length(z$sim_out$ev, 5000)
})

test_that("multiple factor variables work", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    color = factor(sample(c("red", "blue"), n, replace = TRUE)),
    size = factor(sample(c("S", "M", "L"), n, replace = TRUE))
  )
  z <- zelig2(y ~ color + size, model = "ls", data = df, num = 100L)
  z <- setx(z, color = "blue", size = "M")
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})
