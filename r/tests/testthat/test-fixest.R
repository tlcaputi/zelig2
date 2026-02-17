test_that("ls with FE in formula uses fixest", {
  data(mtcars)
  z <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars)
  expect_s3_class(z, "zelig2")
  expect_true(z$is_fixest)
  expect_true(inherits(z$fit, "fixest"))
  expect_equal(z$fe_names, "cyl")
})

test_that("ls with fixef argument uses fixest", {
  data(mtcars)
  z <- zelig2(mpg ~ hp, model = "ls", data = mtcars, fixef = ~cyl)
  expect_true(z$is_fixest)
  expect_true(inherits(z$fit, "fixest"))
})

test_that("fixef as character vector works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp, model = "ls", data = mtcars, fixef = "cyl")
  expect_true(z$is_fixest)
})

test_that("FE coefs match fixest::feols directly", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars)
  raw <- fixest::feols(mpg ~ hp + wt | cyl, data = mtcars)
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("FE sim runs end to end", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars, num = 200L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_length(z$sim_out$ev, 200)
  expect_length(z$sim_out$pv, 200)
})

test_that("FE sim with specified FE level works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars, num = 200L)
  # Specify the FE level
  z <- setx(z, hp = 150, wt = 3, cyl = "6")
  z <- sim(z)
  expect_length(z$sim_out$ev, 200)
})

test_that("FE default is mean across FE levels", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars, num = 200L)
  # No FE specified -> uses mean
  z_mean <- setx(z, hp = 150, wt = 3)
  z_mean <- sim(z_mean)

  # With specific FE -> different predictions
  z_4cyl <- setx(z, hp = 150, wt = 3, cyl = "4")
  z_4cyl <- sim(z_4cyl)

  # EVs should differ since FE contributions differ
  expect_false(abs(mean(z_mean$sim_out$ev) - mean(z_4cyl$sim_out$ev)) < 0.01)
})

test_that("FE first differences work", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars, num = 200L)
  z <- setx(z, hp = 100)
  z <- setx1(z, hp = 200)
  z <- sim(z)
  expect_true(!is.null(z$sim_out$fd))
  expect_length(z$sim_out$fd, 200)
  # More hp -> lower mpg
  expect_true(mean(z$sim_out$fd) < 0)
})

test_that("FE plot works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150)
  z <- sim(z)
  p <- plot(z)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("FE range scenario works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = seq(100, 250, by = 50))
  z <- sim(z)
  expect_true(is.matrix(z$sim_out$ev))
  expect_equal(ncol(z$sim_out$ev), 4)
})

test_that("logit with FE works", {
  data(mtcars)
  mtcars$vs_int <- as.integer(mtcars$vs)
  z <- zelig2(vs_int ~ hp | gear, model = "logit", data = mtcars, num = 100L)
  expect_true(z$is_fixest)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_true(all(z$sim_out$ev >= 0 & z$sim_out$ev <= 1))
})

test_that("poisson with FE works", {
  data(mtcars)
  z <- zelig2(carb ~ hp | cyl, model = "poisson", data = mtcars, num = 100L)
  expect_true(z$is_fixest)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_true(all(z$sim_out$ev > 0))
  expect_true(all(z$sim_out$pv >= 0))
})

test_that("negbin with FE works", {
  data(mtcars)
  z <- zelig2(carb ~ hp | cyl, model = "negbin", data = mtcars, num = 100L)
  expect_true(z$is_fixest)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_true(all(z$sim_out$pv >= 0))
})

test_that("gamma with FE works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp | cyl, model = "gamma", data = mtcars, num = 100L)
  expect_true(z$is_fixest)
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_true(all(z$sim_out$ev > 0))
})

test_that("probit with FE works", {
  data(mtcars)
  mtcars$vs_int <- as.integer(mtcars$vs)
  z <- zelig2(vs_int ~ wt | gear, model = "probit", data = mtcars, num = 100L)
  expect_true(z$is_fixest)
  z <- setx(z, wt = 3)
  z <- sim(z)
  expect_true(all(z$sim_out$ev >= 0 & z$sim_out$ev <= 1))
})

test_that("FE robust vcov works", {
  data(mtcars)
  z_def <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars)
  z_rob <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars,
                  vcov_type = "robust")
  # SEs should differ
  expect_false(all(abs(diag(vcov(z_def)) - diag(vcov(z_rob))) < 1e-10))
})

test_that("FE cluster vcov works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars,
              vcov_type = "cluster", cluster = ~cyl)
  expect_true(is.matrix(vcov(z)))
})

test_that("tobit rejects FE gracefully", {
  set.seed(42)
  n <- 100
  df <- data.frame(y = pmax(rnorm(n), 0), x = rnorm(n), g = sample(1:3, n, TRUE))
  expect_error(
    zelig2(y ~ x | g, model = "tobit", data = df),
    "not supported"
  )
})

test_that("quantile rejects FE gracefully", {
  data(mtcars)
  expect_error(
    zelig2(mpg ~ hp | cyl, model = "quantile", data = mtcars),
    "not supported"
  )
})

test_that("multiple FEs work", {
  data(mtcars)
  z <- zelig2(mpg ~ hp | cyl + gear, model = "ls", data = mtcars, num = 200L)
  expect_true(z$is_fixest)
  expect_equal(sort(z$fe_names), sort(c("cyl", "gear")))
  z <- setx(z, hp = 150)
  z <- sim(z)
  expect_length(z$sim_out$ev, 200)
})

test_that("pipe workflow with FE works", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt | cyl, model = "ls", data = mtcars) |>
    setx(hp = 150, wt = 3) |>
    sim()
  expect_true(!is.null(z$sim_out))
})

test_that("print and summary show FE info", {
  data(mtcars)
  z <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars)
  expect_output(print(z), "Fixed effects")
  expect_output(summary(z), "Fixed effects")
})

test_that("from_zelig2_model returns fixest object", {
  data(mtcars)
  z <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars)
  fit <- from_zelig2_model(z)
  expect_true(inherits(fit, "fixest"))
})
