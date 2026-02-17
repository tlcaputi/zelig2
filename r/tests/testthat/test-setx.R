test_that("setx stores scenario", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = 150, wt = 3)
  expect_true(!is.null(z$scenario))
  expect_false(z$scenario$is_range)
  expect_equal(nrow(z$scenario$x_matrix), 1)
})

test_that("setx defaults to median for unspecified covariates", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = 150)
  # wt should be at its median
  expected_wt <- median(mtcars$wt)
  # The x_matrix should have 3 columns: intercept, hp, wt
  expect_equal(ncol(z$scenario$x_matrix), 3)
})

test_that("setx handles range scenarios", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = seq(50, 300, by = 50))
  expect_true(z$scenario$is_range)
  expect_equal(z$scenario$range_var, "hp")
  expect_equal(nrow(z$scenario$x_matrix), 6)  # 50,100,150,200,250,300
})

test_that("setx rejects multiple range variables", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  expect_error(setx(z, hp = 1:5, wt = 1:3), "Only one variable")
})

test_that("setx1 stores contrast scenario", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = 100)
  z <- setx1(z, hp = 200)
  expect_true(!is.null(z$scenario))
  expect_true(!is.null(z$scenario1))
})

test_that("setx handles factor variables", {
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = factor(sample(c("a", "b", "c"), 100, replace = TRUE))
  )
  z <- zelig2(y ~ x1 + x2, model = "ls", data = df)
  z <- setx(z, x1 = 0, x2 = "b")
  expect_true(!is.null(z$scenario))
})

test_that("setx with fn argument works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, fn = "mean")
  expect_true(!is.null(z$scenario))
})

test_that("setx clears previous sim results", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_true(!is.null(z$sim_out))
  z <- setx(z, hp = 200)
  expect_true(is.null(z$sim_out))
})
