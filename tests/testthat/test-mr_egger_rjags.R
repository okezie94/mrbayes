## MR-Egger Rjags

context("Tests for MR-Egger function using JAGS")

test_that("MR-Egger using default prior method",
          {
            eggerfit <- mr_egger_rjags(do_data,
                                       seed = c(123, 456, 789))
            expect_equal(class(eggerfit), "eggerjags")
            expect_equal(unname(eggerfit$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(eggerfit$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[1]), 0.39, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit$samples), "mcmc.list")
            expect_equal(eggerfit$priormethod, "default")
          })

test_that("MR-Egger using weak prior method",
          {
            eggerfit1 <-
              mr_egger_rjags(do_data,
                             prior = "weak",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit1), "eggerjags")
            expect_equal(unname(eggerfit1$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(eggerfit1$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[1]), 0.39 , tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit1$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit1$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit1$samples), "mcmc.list")
            expect_equal(eggerfit1$priormethod, "weak")
          })

test_that("MR-Egger using pseudo prior method",
          {
            eggerfit2 <-
              mr_egger_rjags(do_data,
                             prior = "pseudo",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit2), "eggerjags")
            expect_equal(unname(eggerfit2$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit2$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[1]), 0.38, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit2$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit2$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit2$samples), "mcmc.list")
            expect_equal(eggerfit2$priormethod, "pseudo")
          })

test_that("MR-Egger using joint prior method",
          {
            skip_on_cran()
            eggerfit3 <-
              mr_egger_rjags(do_data,
                             prior = "joint",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit3), "eggerjags")
            expect_equal(unname(eggerfit3$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit3$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit3$CredibleInterval[1]), 0.39, tol = 1e-2)
            expect_equal(unname(eggerfit3$CredibleInterval[2]), 0.51, tol = 1e-2)
            expect_equal(unname(eggerfit3$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit3$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit3$sigma), 1.59, tol = 1e-2)
            expect_equal(class(eggerfit3$samples), "mcmc.list")
            expect_equal(eggerfit3$priormethod, "joint")
          })

test_that("MR-Egger using betaprior and sigmaprior method",
          {
            eggerfit4 <-
              mr_egger_rjags(do_data,
                             betaprior = "dnorm(0, 1E-6)",
                             sigmaprior = "dunif(.0001, 10)",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit4), "eggerjags")
            expect_equal(unname(eggerfit4$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(eggerfit4$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit4$CredibleInterval[1]), 0.39 , tol = 1e-2)
            expect_equal(unname(eggerfit4$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit4$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit4$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit4$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit4$samples), "mcmc.list")
          })
