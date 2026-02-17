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
