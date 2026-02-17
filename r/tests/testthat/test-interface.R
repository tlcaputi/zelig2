test_that("to_zelig2 wraps a glm object", {
  data(mtcars)
  raw <- glm(mpg ~ hp + wt, data = mtcars, family = gaussian())
  z <- to_zelig2(raw, model = "ls", data = mtcars)
  expect_s3_class(z, "zelig2")
  expect_equal(unname(coef(z)), unname(coef(raw)))
})

test_that("to_zelig2 with logit works", {
  data(mtcars)
  raw <- glm(vs ~ hp + wt, data = mtcars, family = binomial())
  z <- to_zelig2(raw, model = "logit", data = mtcars)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z, num = 100L)
  expect_true(all(z$sim_out$ev >= 0 & z$sim_out$ev <= 1))
})

test_that("from_zelig2_model extracts fit", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  fit <- from_zelig2_model(z)
  expect_true(inherits(fit, "glm"))
  expect_equal(unname(coef(fit)), unname(coef(z)))
})

test_that("from_zelig2_model rejects non-zelig2", {
  expect_error(from_zelig2_model(list(a = 1)), "zelig2 object")
})

test_that("zelig2_qi_to_df works for single scenario", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  df <- zelig2_qi_to_df(z)
  expect_s3_class(df, "data.frame")
  expect_true("ev" %in% names(df))
  expect_true("pv" %in% names(df))
  expect_equal(nrow(df), 100)
})

test_that("zelig2_qi_to_df works with first differences", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 100)
  z <- setx1(z, hp = 200)
  z <- sim(z)
  df <- zelig2_qi_to_df(z)
  expect_true("fd" %in% names(df))
})

test_that("zelig2_qi_to_df works for range scenario", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 50L)
  z <- setx(z, hp = c(100, 150, 200))
  z <- sim(z)
  df <- zelig2_qi_to_df(z)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 50 * 3)
})

test_that("zelig2_qi_to_df rejects non-simulated object", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  expect_error(zelig2_qi_to_df(z), "simulation results")
})

test_that("to_zelig2 with robust SEs works", {
  data(mtcars)
  raw <- glm(mpg ~ hp + wt, data = mtcars, family = gaussian())
  z <- to_zelig2(raw, model = "ls", data = mtcars, vcov_type = "robust")
  V_rob <- sandwich::vcovHC(raw, type = "HC1")
  expect_equal(vcov(z), V_rob, tolerance = 1e-10)
})
