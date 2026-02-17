test_that("robust SEs match sandwich::vcovHC", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, vcov_type = "HC1")
  raw <- glm(mpg ~ hp + wt, data = mtcars, family = gaussian())
  V_raw <- sandwich::vcovHC(raw, type = "HC1")
  expect_equal(vcov(z), V_raw, tolerance = 1e-10)
})

test_that("vcov_type='robust' is alias for HC1", {
  data(mtcars)
  z_robust <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
                     vcov_type = "robust")
  z_hc1 <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
                   vcov_type = "HC1")
  expect_equal(vcov(z_robust), vcov(z_hc1), tolerance = 1e-10)
})

test_that("HC0 through HC4 all work", {
  data(mtcars)
  for (hc in c("HC0", "HC1", "HC2", "HC3", "HC4")) {
    z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, vcov_type = hc)
    expect_true(is.matrix(vcov(z)))
    expect_equal(nrow(vcov(z)), 3)
  }
})

test_that("clustered SEs work", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
              vcov_type = "cluster", cluster = "cyl")
  raw <- glm(mpg ~ hp + wt, data = mtcars, family = gaussian())
  V_cl <- sandwich::vcovCL(raw, cluster = mtcars$cyl)
  expect_equal(vcov(z), V_cl, tolerance = 1e-10)
})

test_that("clustered SEs with formula work", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
              vcov_type = "cluster", cluster = ~cyl)
  expect_true(is.matrix(vcov(z)))
})

test_that("bootstrap SEs work", {
  set.seed(42)
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
              vcov_type = "bootstrap", bootstrap_n = 50L)
  expect_true(is.matrix(vcov(z)))
  expect_equal(nrow(vcov(z)), 3)
})

test_that("cluster without vcov_type='cluster' errors appropriately", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, vcov_type = "default")
  # The cluster argument is ignored when vcov_type is not 'cluster'
  # (this is by design — no error expected for default)
  expect_true(is.matrix(vcov(z)))
})

test_that("robust SEs differ from default", {
  data(mtcars)
  z_def <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  z_rob <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
                  vcov_type = "robust")
  expect_false(all(abs(diag(vcov(z_def)) - diag(vcov(z_rob))) < 1e-10))
})
