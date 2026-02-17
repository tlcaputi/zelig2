test_that("zelig2 basic OLS workflow works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  expect_s3_class(z, "zelig2")
  expect_equal(z$model_name, "ls")
  expect_equal(length(z$coef), 3)  # intercept + 2 predictors
  expect_true(is.matrix(z$vcov))
})

test_that("zelig2 rejects bad inputs", {
  data(mtcars)
  expect_error(zelig2("mpg ~ hp", model = "ls", data = mtcars),
               "formula")
  expect_error(zelig2(mpg ~ hp, model = "ls", data = list(a = 1)),
               "data frame")
  expect_error(zelig2(mpg ~ hp, model = c("ls", "logit"), data = mtcars),
               "single character")
  expect_error(zelig2(mpg ~ hp, model = "nonexistent", data = mtcars),
               "not found")
})

test_that("full pipeline works: zelig2 -> setx -> sim -> plot", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_true(!is.null(z$sim_out))
  expect_length(z$sim_out$ev, z$num)
  p <- plot(z)
  expect_s3_class(p, "gg")
})

test_that("pipe workflow works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars) |>
    setx(hp = 150, wt = 3) |>
    sim()
  expect_true(!is.null(z$sim_out))
})

test_that("auto-factorize handles character columns", {
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = sample(c("a", "b", "c"), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
  z <- zelig2(y ~ x1 + x2, model = "ls", data = df)
  expect_true(is.factor(z$data$x2))
})

test_that("list_models returns all 8 models", {
  models <- list_models()
  expect_true(all(c("ls", "logit", "probit", "poisson", "negbin",
                     "gamma", "tobit", "quantile") %in% models))
})
