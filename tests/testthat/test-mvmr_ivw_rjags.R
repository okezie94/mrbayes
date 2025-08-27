context("Tests for IVW function using JAGS")

skip_if(
  Sys.info()[["sysname"]] == "Emscripten",
  message = "Skip tests on Emscripten aka WebR."
)

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


test_that("IVW using default prior method", {
  skip_on_cran()
  skip_if_not_installed("rjags")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  ivwfit <- mvmr_ivw_rjags(dat, seed = c(123, 456, 789))
  expect_equal(class(ivwfit), "mvivwjags")
  expect_equal(unname(ivwfit$CausalEffect[1]), 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit$CausalEffect[2]), -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit$CausalEffect[3]), 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit$StandardError[1]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit$StandardError[2]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit$StandardError[3]), 0.05, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[1], 0.34, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[2], -0.19, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[3], 0.18, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[4], 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[5], -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[6], 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[7], 0.5, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[8], -0.03, tol = 1e-2)
  expect_equal(unname(ivwfit$CredibleInterval)[9], 0.38, tol = 1e-2)
  expect_equal(class(ivwfit$samples), "mcmc.list")
  expect_equal(ivwfit$priormethod, "default")
})

test_that("IVW using weak prior method", {
  skip_on_cran()
  skip_if_not_installed("rjags")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  ivwfit1 <- mvmr_ivw_rjags(dat, prior = "weak", seed = c(123, 456, 789))
  expect_equal(class(ivwfit1), "mvivwjags")
  expect_equal(unname(ivwfit1$CausalEffect[1]), 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit1$CausalEffect[2]), -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit1$CausalEffect[3]), 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit1$StandardError[1]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit1$StandardError[2]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit1$StandardError[3]), 0.05, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[1], 0.34, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[2], -0.19, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[3], 0.18, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[4], 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[5], -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[6], 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[7], 0.5, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[8], -0.03, tol = 1e-2)
  expect_equal(unname(ivwfit1$CredibleInterval)[9], 0.38, tol = 1e-2)
  expect_equal(class(ivwfit1$samples), "mcmc.list")
  expect_equal(ivwfit1$priormethod, "weak")
})

test_that("IVW using pseudo prior method", {
  skip_on_cran()
  skip_if_not_installed("rjags")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  ivwfit2 <- mvmr_ivw_rjags(dat, prior = "pseudo", seed = c(123, 456, 789))
  expect_equal(class(ivwfit2), "mvivwjags")
  expect_equal(unname(ivwfit2$CausalEffect[1]), 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit2$CausalEffect[2]), -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit2$CausalEffect[3]), 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit2$StandardError[1]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit2$StandardError[2]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit2$StandardError[3]), 0.05, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[1], 0.34, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[2], -0.20, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[3], 0.18, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[4], 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[5], -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[6], 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[7], 0.5, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[8], -0.03, tol = 1e-2)
  expect_equal(unname(ivwfit2$CredibleInterval)[9], 0.38, tol = 1e-2)
  expect_equal(class(ivwfit2$samples), "mcmc.list")
  expect_equal(ivwfit2$priormethod, "pseudo")
})

test_that("IVW using beta prior method", {
  skip_on_cran()
  skip_if_not_installed("rjags")
  dat <- mvmr_format(
    rsid = dodata$rsid,
    xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
    ybeta = dodata$chdbeta,
    xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
    yse = dodata$chdse
  )

  ivwfit3 <- mvmr_ivw_rjags(
    dat,
    betaprior = "dnorm(0, 1E-6)",
    seed = c(123, 456, 789)
  )
  expect_equal(unname(ivwfit3$CausalEffect[1]), 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit3$CausalEffect[2]), -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit3$CausalEffect[3]), 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit3$StandardError[1]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit3$StandardError[2]), 0.04, tol = 1e-2)
  expect_equal(unname(ivwfit3$StandardError[3]), 0.05, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[1], 0.34, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[2], -0.20, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[3], 0.18, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[4], 0.42, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[5], -0.12, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[6], 0.28, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[7], 0.5, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[8], -0.03, tol = 1e-2)
  expect_equal(unname(ivwfit3$CredibleInterval)[9], 0.38, tol = 1e-2)
  expect_equal(class(ivwfit3$samples), "mcmc.list")
  expect_equal(ivwfit3$priormethod, "default")
})
