test_that("ls matches base R glm exactly", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
  raw <- glm(mpg ~ hp + wt, data = mtcars, family = gaussian())
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("logit matches base R glm exactly", {
  data(mtcars)
  z <- zelig2(vs ~ hp + wt, model = "logit", data = mtcars)
  raw <- glm(vs ~ hp + wt, data = mtcars, family = binomial(link = "logit"))
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("probit matches base R glm exactly", {
  data(mtcars)
  z <- zelig2(vs ~ hp + wt, model = "probit", data = mtcars)
  raw <- glm(vs ~ hp + wt, data = mtcars, family = binomial(link = "probit"))
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("poisson matches base R glm exactly", {
  data(mtcars)
  z <- zelig2(carb ~ hp + wt, model = "poisson", data = mtcars)
  raw <- glm(carb ~ hp + wt, data = mtcars, family = poisson())
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("negbin matches MASS::glm.nb exactly", {
  data(mtcars)
  z <- zelig2(carb ~ hp + wt, model = "negbin", data = mtcars)
  raw <- MASS::glm.nb(carb ~ hp + wt, data = mtcars)
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-6)
})

test_that("gamma matches base R glm exactly", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "gamma", data = mtcars)
  raw <- glm(mpg ~ hp + wt, data = mtcars, family = Gamma(link = "inverse"))
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("ls simulation runs without error", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})

test_that("logit simulation produces probabilities in [0,1]", {
  data(mtcars)
  z <- zelig2(vs ~ hp + wt, model = "logit", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_true(all(z$sim_out$ev >= 0 & z$sim_out$ev <= 1))
  expect_true(all(z$sim_out$pv %in% c(0, 1)))
})

test_that("poisson simulation produces non-negative counts", {
  data(mtcars)
  z <- zelig2(carb ~ hp + wt, model = "poisson", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_true(all(z$sim_out$ev > 0))
  expect_true(all(z$sim_out$pv >= 0))
  expect_true(all(z$sim_out$pv == floor(z$sim_out$pv)))  # integers
})

test_that("gamma simulation produces positive values", {
  data(mtcars)
  z <- zelig2(mpg ~ hp + wt, model = "gamma", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_true(all(z$sim_out$ev > 0))
  expect_true(all(z$sim_out$pv > 0))
})

test_that("negbin simulation produces non-negative integers", {
  data(mtcars)
  z <- zelig2(carb ~ hp + wt, model = "negbin", data = mtcars, num = 100L)
  z <- setx(z, hp = 150, wt = 3)
  z <- sim(z)
  expect_true(all(z$sim_out$pv >= 0))
  expect_true(all(z$sim_out$pv == floor(z$sim_out$pv)))
})
