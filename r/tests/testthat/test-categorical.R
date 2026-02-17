test_that("character variables auto-converted to factors", {
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = sample(c("red", "blue", "green"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  z <- zelig2(y ~ x1 + x2, model = "ls", data = df)
  expect_true(is.factor(z$data$x2))
  expect_equal(length(z$coef), 4)  # intercept + x1 + x2blue + x2green (or similar)
})

test_that("setx with categorical works end to end", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    color = sample(c("red", "blue", "green"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  z <- zelig2(y ~ x1 + color, model = "ls", data = df, num = 100L)
  z <- setx(z, x1 = 0, color = "blue")
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})

test_that("first differences with categorical variables work", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    color = sample(c("red", "blue", "green"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  z <- zelig2(y ~ x1 + color, model = "ls", data = df, num = 100L)
  z <- setx(z, color = "red")
  z <- setx1(z, color = "blue")
  z <- sim(z)
  expect_true(!is.null(z$sim_out$fd))
  expect_length(z$sim_out$fd, 100)
})

test_that("categorical logit model works", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    group = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )
  z <- zelig2(y ~ x1 + group, model = "logit", data = df, num = 100L)
  z <- setx(z, x1 = 0, group = "B")
  z <- sim(z)
  expect_true(all(z$sim_out$ev >= 0 & z$sim_out$ev <= 1))
})

test_that("cat_vars metadata is populated", {
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = factor(c("a", "b"))
  )
  z <- zelig2(y ~ x1 + x2, model = "ls", data = df)
  expect_true("x2" %in% names(z$cat_vars))
  expect_equal(z$cat_vars$x2, c("a", "b"))
})

test_that("default for factor vars is mode", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(100),
    color = factor(c(rep("red", 60), rep("blue", 40)))
  )
  z <- zelig2(y ~ color, model = "ls", data = df, num = 100L)
  z <- setx(z)  # no values specified -> default to mode
  z <- sim(z)
  # Should run without error

  expect_length(z$sim_out$ev, 100)
})
