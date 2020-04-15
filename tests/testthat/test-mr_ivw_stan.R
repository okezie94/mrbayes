# Tests for the mrbayes package

context("Tests for mr_ivw_stan() function")

# Analysis
## IVW stan

test_that("IVW using default prior method",
          {
            ivwfit <- mr_ivw_stan(do_data, seed = 123)
            expect_equal(class(ivwfit), "ivwjags")
            expect_equal(unname(ivwfit$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(ivwfit$StandardError), 0.04, tol = 1e-2)
            expect_equal(unname(ivwfit$CredibleInterval)[1], 0.44, tol = 1e-2)
            expect_equal(unname(ivwfit$CredibleInterval)[2], 0.50, tol = 1e-2)
            expect_equal(unname(ivwfit$CredibleInterval)[3], 0.58, tol = 1e-2)
            expect_equal(class(ivwfit$samples), "mcmc.list")
            expect_equal(ivwfit$priormethod, "default")
          })
#

test_that("IVW using weak prior method",
          {
            ivwfit1 <- mr_ivw_stan(mrdat, prior = "weak", seed = 123)
            expect_equal(class(ivwfit1), "ivwjags")
            expect_equal(unname(ivwfit1$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(ivwfit1$StandardError), 0.04, tol = 1e-2)
            expect_equal(unname(ivwfit1$CredibleInterval[1]), 0.44, tol = 1e-2)
            expect_equal(unname(ivwfit1$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(ivwfit1$CredibleInterval[3]), 0.58, tol = 1e-2)
            expect_equal(class(ivwfit1$samples), "mcmc.list")
            expect_equal(ivwfit1$priormethod, "weak")
          })

test_that("IVW using pseudo prior method",
          {
            ivwfit2 <- mr_ivw_stan(mrdat, prior = "pseudo", seed = 123)
            expect_equal(class(ivwfit2), "ivwjags")
            expect_equal(unname(ivwfit2$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(ivwfit2$StandardError), 0.03, tol = 1e-2)
            expect_equal(unname(ivwfit2$CredibleInterval[1]), 0.43, tol = 1e-2)
            expect_equal(unname(ivwfit2$CredibleInterval[2]), 0.50, tol = 1e-2)
            expect_equal(unname(ivwfit2$CredibleInterval[3]), 0.57, tol = 1e-2)
            expect_equal(class(ivwfit2$samples), "mcmc.list")
            expect_equal(ivwfit2$priormethod, "pseudo")
          })


test_that("IVW using beta prior method",
          {
            ivwfit3 <- mr_ivw_stan(mrdat, betaprior = "dnorm(0, 1E-6)", seed = 123)
            expect_equal(class(ivwfit3), "ivwjags")
            expect_equal(unname(ivwfit3$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(ivwfit3$StandardError), 0.04, tol = 1e-2)
            expect_equal(unname(ivwfit3$CredibleInterval[1]), 0.44, tol = 1e-2)
            expect_equal(unname(ivwfit3$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(ivwfit3$CredibleInterval[3]), 0.58, tol = 1e-2)
            expect_equal(class(ivwfit3$samples), "mcmc.list")
          })
