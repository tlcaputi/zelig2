test_that("survey-weighted ls matches survey::svyglm", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                               fpc = ~fpc)
  z <- zelig2(api00 ~ ell + meals, model = "ls", data = apiclus1,
              survey_design = dclus1)
  raw <- survey::svyglm(api00 ~ ell + meals, design = dclus1)
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("survey-weighted logit matches survey::svyglm", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  apiclus1$high_api <- as.numeric(apiclus1$api00 > 600)
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                               fpc = ~fpc)
  z <- zelig2(high_api ~ ell + meals, model = "logit", data = apiclus1,
              survey_design = dclus1)
  raw <- survey::svyglm(high_api ~ ell + meals, design = dclus1,
                         family = binomial(link = "logit"))
  expect_equal(unname(coef(z)), unname(coef(raw)), tolerance = 1e-10)
})

test_that("convenience survey params build design automatically", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  z <- zelig2(api00 ~ ell + meals, model = "ls", data = apiclus1,
              ids = ~dnum, weights = ~pw, fpc = ~fpc)
  expect_true(z$is_survey)
  expect_s3_class(z, "zelig2")
})

test_that("survey vcov differs from non-survey", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                               fpc = ~fpc)
  z_svy <- zelig2(api00 ~ ell + meals, model = "ls", data = apiclus1,
                  survey_design = dclus1)
  z_raw <- zelig2(api00 ~ ell + meals, model = "ls", data = apiclus1)
  # SEs should differ
  expect_false(all(abs(diag(vcov(z_svy)) - diag(vcov(z_raw))) < 1e-6))
})

test_that("survey sim runs end to end", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                               fpc = ~fpc)
  z <- zelig2(api00 ~ ell + meals, model = "ls", data = apiclus1,
              survey_design = dclus1, num = 100L)
  z <- setx(z, ell = 20, meals = 50)
  z <- sim(z)
  expect_length(z$sim_out$ev, 100)
})

test_that("setx fn='mean' uses weighted means for numeric covariates on weighted fits", {
  skip_if_not_installed("survey")
  set.seed(123)
  n <- 800
  d <- data.frame(
    y       = rbinom(n, 1, 0.3),
    x_num   = rep(c(0, 1), each = n / 2),
    x_fac   = factor(sample(c("A", "B"), n, replace = TRUE)),
    w       = c(rep(1, n / 2), rep(9, n / 2)),
    psu     = rep(seq_len(40), length.out = n),
    stratum = rep(seq_len(4), length.out = n)
  )
  des <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w,
                            data = d, nest = TRUE)
  z <- zelig2(y ~ x_num + x_fac, model = "logit", data = d,
              survey_design = des)
  z <- setx(z, fn = "mean", factor_default = "mean")

  unweighted_x <- mean(d$x_num)
  weighted_x   <- weighted.mean(d$x_num, d$w)

  # Locate the x_num column in the X-row by name
  full_mm <- stats::model.matrix(z$fit)
  x_idx   <- which(colnames(full_mm) == "x_num")
  x_val   <- z$scenario$x_matrix[1, x_idx]

  # The patched setx should use the weighted mean (~0.9), not 0.5
  expect_equal(x_val, weighted_x, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(x_val, unweighted_x, tolerance = 1e-3)))
})

test_that("setx fn='mean' falls back to unweighted means on unweighted fits", {
  set.seed(456)
  d <- data.frame(
    y     = rbinom(200, 1, 0.4),
    x_num = rnorm(200, mean = 5)
  )
  z <- zelig2(y ~ x_num, model = "logit", data = d)
  z <- setx(z, fn = "mean")
  full_mm <- stats::model.matrix(z$fit)
  x_idx   <- which(colnames(full_mm) == "x_num")
  expect_equal(z$scenario$x_matrix[1, x_idx], mean(d$x_num), tolerance = 1e-10)
})

test_that("numeric weight vector works alongside ids/strata (svydesign path)", {
  # REGRESSION TEST for a bug where resolve_weights() attached the
  # `.zelig2_weights` column to its own local copy of `data` (R is
  # copy-on-modify), so svydesign() later failed with
  #   "object '.zelig2_weights' not found".
  #
  # It survived because every other test here passes weights either as a
  # FORMULA (~pw) or via a pre-built survey_design. Only the combination
  # "numeric vector + ids/strata" hits the broken path.
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 600
  d <- data.frame(
    x       = rbinom(n, 1, 0.5),
    w       = runif(n, 0.5, 2),
    psu     = rep(seq_len(2), times = n / 2),
    stratum = rep(seq_len(30), each = n / 30)
  )
  d$y <- rbinom(n, 1, plogis(-0.2 + 0.8 * d$x))

  expect_no_error(
    zelig2(y ~ x, model = "logit", data = d,
           weights = d$w, ids = "psu", strata = "stratum",
           nest = TRUE, num = 25L)
  )

  # And it must agree with the equivalent pre-built design.
  d$.w <- d$w
  des <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~.w,
                           data = d, nest = TRUE)
  z_vec <- zelig2(y ~ x, model = "logit", data = d, weights = d$w,
                  ids = "psu", strata = "stratum", nest = TRUE, num = 25L)
  z_des <- zelig2(y ~ x, model = "logit", data = d,
                  survey_design = des, num = 25L)
  expect_equal(unname(coef(z_vec)), unname(coef(z_des)), tolerance = 1e-10)
})

test_that("supplying both survey_design and components is an error, not a silent drop", {
  # A pre-built design carries its own weights and clustering. Passing `weights`
  # as well used to be discarded SILENTLY, so a caller could believe their weights
  # were applied when the design's were used instead.
  #
  # This ERRORS rather than warns on purpose: there is no coherent reason to pass
  # both, the cost of guessing wrong in a statistical package is a wrong published
  # number, and a warning mid-script is easily missed.
  skip_if_not_installed("survey")
  set.seed(7)
  n <- 400
  d <- data.frame(
    x       = rbinom(n, 1, 0.5),
    psu     = rep(seq_len(2), times = n / 2),
    stratum = rep(seq_len(20), each = n / 20)
  )
  d$w_real  <- runif(n, 0.5, 2)
  d$w_wrong <- rep(1, n)
  d$y <- rbinom(n, 1, plogis(-0.3 + 0.8 * d$x))

  des <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w_real,
                           data = d, nest = TRUE)

  expect_error(
    zelig2(y ~ x, model = "logit", data = d,
           survey_design = des, weights = d$w_wrong, num = 10L),
    "not both"
  )
  expect_error(
    zelig2(y ~ x, model = "logit", data = d,
           survey_design = des, strata = "stratum", num = 10L),
    "not both"
  )

  # A design on its own still works.
  expect_no_error(
    zelig2(y ~ x, model = "logit", data = d, survey_design = des, num = 10L)
  )
})
