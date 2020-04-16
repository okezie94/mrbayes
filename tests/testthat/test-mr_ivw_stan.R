# Tests for the mrbayes package

context("Tests for mr_ivw_stan() function")

# Analysis
## IVW stan

test_that("IVW using default prior method",
          {
            ivwfit <- mr_ivw_stan(do_data, seed = 123)
            summfit <- rstan::summary(ivwfit)
            expect_equal(class(ivwfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit$summary["estimate","sd"], 0.05, tol = 1e-2) # 0.04 ??
            expect_equal(summfit$summary["estimate","2.5%"], 0.38, tol = 1e-2) # .44 ??
            expect_equal(summfit$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit$summary["estimate","97.5%"], 0.61, tol = 1e-2) # .58
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
