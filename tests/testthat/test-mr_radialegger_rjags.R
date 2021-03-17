# Radial-Egger Rjags

context("Tests for Radial MR-Egger function using JAGS")

test_that("Dataset is formatted",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            expect_s3_class(dat, "mr_format")
          })

test_that("Radial-Egger using default prior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            radialeggerfit <- mr_radialegger_rjags(dat,
                                                   seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit), "radialeggerjags")
            expect_equal(unname(radialeggerfit$CausalEffect), 0.591, tol = 1e-2)
            expect_equal(unname(radialeggerfit$StandardError), 0.078, tol = 1e-2)
            expect_equal(unname(radialeggerfit$CredibleInterval[1]), 0.438, tol = 1e-2)
            expect_equal(unname(radialeggerfit$CredibleInterval[2]), 0.591, tol = 1e-2)
            expect_equal(unname(radialeggerfit$CredibleInterval[3]), 0.746, tol = 1e-2)
            expect_equal(unname(radialeggerfit$AvgPleio), -0.252, tol = 1e-2 )
            expect_equal(unname(radialeggerfit$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit$samples), "mcmc.list")
            expect_equal(radialeggerfit$priormethod, "default")
          })

test_that("Radial-Egger using weak prior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            radialeggerfit1 <- mr_radialegger_rjags(dat,
                                                    prior = "weak",
                                                    seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit1), "radialeggerjags")
            expect_equal(unname(radialeggerfit1$CausalEffect), 0.591, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$StandardError), 0.0788, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$CredibleInterval[1]),0.438 , tol = 1e-2)
            expect_equal(unname(radialeggerfit1$CredibleInterval[2]), 0.591, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$CredibleInterval[3]), 0.746, tol = 1e-2)
            expect_equal(unname(radialeggerfit1$AvgPleio), -0.252, tol = 1e-2 )
            expect_equal(unname(radialeggerfit1$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit1$samples), "mcmc.list")
            expect_equal(radialeggerfit1$priormethod, "weak")
          })

test_that("Radial-Egger using pseudo prior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            radialeggerfit2 <- mr_radialegger_rjags(dat,
                                                    prior = "pseudo",
                                                    seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit2), "radialeggerjags")
            expect_equal(unname(radialeggerfit2$CausalEffect), 0.586, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$StandardError), 0.0795, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$CredibleInterval[1]), 0.426, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$CredibleInterval[2]), 0.586, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$CredibleInterval[3]), 0.739, tol = 1e-2)
            expect_equal(unname(radialeggerfit2$AvgPleio), -0.245, tol = 1e-2 )
            expect_equal(unname(radialeggerfit2$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit2$samples), "mcmc.list")
            expect_equal(radialeggerfit2$priormethod, "pseudo")
          })

test_that("Radial-Egger using joint prior method",
          {
            skip_on_cran()
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            radialeggerfit3 <- mr_radialegger_rjags(dat,
                                                    prior = "joint",
                                                    seed = c(123, 456, 789),
                                                    rho = 0.5)
            expect_equal(class(radialeggerfit3), "radialeggerjags")
            expect_equal(unname(radialeggerfit3$CausalEffect), 0.588, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$StandardError), 0.0797, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$CredibleInterval[1]), 0.434, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$CredibleInterval[2]), 0.588, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$CredibleInterval[3]), 0.745, tol = 1e-2)
            expect_equal(unname(radialeggerfit3$AvgPleio), -0.241, tol = 1e-2 )
            expect_equal(unname(radialeggerfit3$sigma), 0.406, tol = 1e-2)
            expect_equal(class(radialeggerfit3$samples), "mcmc.list")
            expect_equal(radialeggerfit3$priormethod, "joint")
          })

test_that("Radial-Egger using betaprior and sigmaprior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            radialeggerfit4 <- mr_radialegger_rjags(dat,
                                                    betaprior = "dnorm(0, 1E-6)",
                                                    sigmaprior = "dunif(.0001, 10)",
                                                    seed = c(123, 456, 789))
            expect_equal(class(radialeggerfit4), "radialeggerjags")
            expect_equal(unname(radialeggerfit4$CausalEffect), 0.591, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$StandardError), 0.078, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$CredibleInterval[1]),0.438 , tol = 1e-2)
            expect_equal(unname(radialeggerfit4$CredibleInterval[2]), 0.591, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$CredibleInterval[3]), 0.746, tol = 1e-2)
            expect_equal(unname(radialeggerfit4$AvgPleio), -0.252, tol = 1e-2 )
            expect_equal(unname(radialeggerfit4$sigma), 0.404, tol = 1e-2)
            expect_equal(class(radialeggerfit4$samples), "mcmc.list")
          })
