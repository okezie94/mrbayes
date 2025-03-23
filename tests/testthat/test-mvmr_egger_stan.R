## MVMR-Egger stan

context("Tests for MVMR-Egger function using stan")

test_that("Dataset is formatted", {
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )
  expect_s3_class(dat, "mvmr_format")
})

test_that("MVMR-Egger using default prior method", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  eggerfit <- mvmr_egger_stan(dat)
  summfit <- rstan::summary(eggerfit)
  expect_equal(class(eggerfit)[1], "stanfit")
  expect_equal(summfit$summary["estimate[1]", "mean"], 0.528, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "sd"], 0.071, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "2.5%"], 0.393, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "50%"], 0.529, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "97.5%"], 0.66, tol = 1e-1)
  expect_equal(summfit$summary["estimate[2]", "mean"], -0.105, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "sd"], 0.0637, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "2.5%"], -0.233, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "50%"], -0.104, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "97.5%"], 0.0164, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "mean"], 0.327, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "sd"], 0.08, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "2.5%"], 0.176, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "50%"], 0.328, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "97.5%"], 0.474, tol = 1e-2)
})

test_that("MVMR-Egger using weak prior method", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  eggerfit1 <- mvmr_egger_stan(dat, prior = 2)
  summfit <- rstan::summary(eggerfit1)
  expect_equal(class(eggerfit1)[1], "stanfit")
  expect_equal(summfit$summary["estimate[1]", "mean"], 0.528, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "sd"], 0.071, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "2.5%"], 0.393, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "50%"], 0.529, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "97.5%"], 0.671, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "mean"], -0.105, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "sd"], 0.0637, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "2.5%"], -0.233, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "50%"], -0.104, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "97.5%"], 0.0164, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "mean"], 0.327, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "sd"], 0.08, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "2.5%"], 0.176, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "50%"], 0.328, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "97.5%"], 0.474, tol = 1e-2)
})

test_that("MVMR-Egger using pseudo prior method", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  eggerfit1 <- mvmr_egger_stan(dat, prior = 3)
  summfit <- rstan::summary(eggerfit1)
  expect_equal(class(eggerfit1)[1], "stanfit")
  expect_equal(summfit$summary["estimate[1]", "mean"], 0.528, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "sd"], 0.071, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "2.5%"], 0.393, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "50%"], 0.529, tol = 1e-2)
  expect_equal(summfit$summary["estimate[1]", "97.5%"], 0.66, tol = 1e-1)
  expect_equal(summfit$summary["estimate[2]", "mean"], -0.105, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "sd"], 0.0637, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "2.5%"], -0.233, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "50%"], -0.104, tol = 1e-2)
  expect_equal(summfit$summary["estimate[2]", "97.5%"], 0.0164, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "mean"], 0.327, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "sd"], 0.08, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "2.5%"], 0.176, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "50%"], 0.328, tol = 1e-2)
  expect_equal(summfit$summary["estimate[3]", "97.5%"], 0.474, tol = 1e-2)
})
