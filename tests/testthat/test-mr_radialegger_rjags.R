# Radial-Egger Rjags

context("Tests for Radial MR-Egger function using JAGS")

test_that("Radial-Egger using default prior method",
          {
            radialeggerfit <- mr_radialegger_rjags(do_data,
                                                   seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit), "radialeggerjags")
            expect_equal(unname(radialeggerfit$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(radialeggerfit$StandardError), 0.058, tol = 1e-2)
            expect_equal(unname(radialeggerfit$CredibleInterval[1]), 0.393, tol = 1e-2)
            expect_equal(unname(radialeggerfit$CredibleInterval[2]), 0.51, tol = 1e-2)
            expect_equal(unname(radialeggerfit$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(radialeggerfit$AvgPleio), -0.0633, tol = 1e-2 )
            expect_equal(unname(radialeggerfit$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit$samples), "mcmc.list")
            expect_equal(radialeggerfit$priormethod, "default")
          })

test_that("Radial-Egger using weak prior method",
          {
            radialeggerfit1 <- mr_radialegger_rjags(do_data,
                                                    prior = "weak",
                                                    seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit1), "radialeggerjags")
            expect_equal(unname(radialeggerfit1$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$StandardError), 0.058, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$CredibleInterval[1]),0.393 , tol = 1e-2)
            expect_equal(unname(radialeggerfit1$CredibleInterval[2]), 0.51, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$AvgPleio), -0.0633, tol = 1e-2 )
            expect_equal(unname(radialeggerfit1$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit1$samples), "mcmc.list")
            expect_equal(radialeggerfit1$priormethod, "weak")
          })

test_that("Radial-Egger using pseudo prior method",
          {
            radialeggerfit2 <- mr_radialegger_rjags(do_data,
                                                    prior = "pseudo",
                                                    seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit2), "radialeggerjags")
            expect_equal(unname(radialeggerfit2$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$StandardError), 0.058, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$CredibleInterval[1]), 0.393, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$CredibleInterval[2]), 0.505, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$CredibleInterval[3]), 0.619, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$AvgPleio), -0.0633, tol = 1e-2 )
            expect_equal(unname(radialeggerfit2$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit2$samples), "mcmc.list")
            expect_equal(radialeggerfit2$priormethod, "pseudo")
          })

test_that("Radial-Egger using joint prior method",
          {
            skip_on_cran()
            radialeggerfit3 <- mr_radialegger_rjags(do_data,
                                                    prior = "joint",
                                                    seed = c(123, 456, 789),
                                                    rho = 0.5)
            expect_equal(class(radialeggerfit3), "radialeggerjags")
            expect_equal(unname(radialeggerfit3$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$StandardError), 0.058, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$CredibleInterval[1]), 0.392, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$CredibleInterval[2]), 0.505, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$CredibleInterval[3]), 0.613, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$AvgPleio), -0.0654, tol = 1e-2 )
            expect_equal(unname(radialeggerfit3$sigma), 0.406, tol = 1e-2)
            expect_equal(class(radialeggerfit3$samples), "mcmc.list")
            expect_equal(radialeggerfit3$priormethod, "joint")
          })

test_that("Radial-Egger using betaprior and sigmaprior method",
          {
            radialeggerfit4 <- mr_radialegger_rjags(do_data,
                                                    betaprior = "dnorm(0, 1E-6)",
                                                    sigmaprior = "dunif(.0001, 10)",
                                                    seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit4), "radialeggerjags")
            expect_equal(unname(radialeggerfit4$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$StandardError), 0.058, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$CredibleInterval[1]),0.393 , tol = 1e-2)
            expect_equal(unname(radialeggerfit4$CredibleInterval[2]), 0.505, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$CredibleInterval[3]), 0.619, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$AvgPleio), -0.0633, tol = 1e-2 )
            expect_equal(unname(radialeggerfit4$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit4$samples), "mcmc.list")
          })
