## MR-Egger Rjags

context("Tests for MR-Egger function using JAGS")

test_that("Dataset is formatted",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            expect_s3_class(dat, "mr_format")
          })

test_that("MR-Egger using default prior method",
          {
            skip_on_cran()
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            eggerfit <- mr_egger_rjags(dat,
                                       seed = c(123, 456, 789))
            expect_equal(class(eggerfit), "eggerjags")
            expect_equal(unname(eggerfit$CausalEffect), 0.57, tol = 1e-2)
            expect_equal(unname(eggerfit$StandardError), 0.077, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[1]), 0.42, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[2]), 0.57, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[3]), 0.72, tol = 1e-2)
            expect_equal(unname(eggerfit$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit$samples), "mcmc.list")
            expect_equal(eggerfit$priormethod, "default")
          })

test_that("MR-Egger using weak prior method",
          {
            skip_on_cran()
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            eggerfit1 <-
              mr_egger_rjags(dat,
                             prior = "weak",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit1), "eggerjags")
            expect_equal(unname(eggerfit1$CausalEffect), 0.569, tol = 1e-2)
            expect_equal(unname(eggerfit1$StandardError), 0.077, tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[1]), 0.42 , tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[2]), 0.569, tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[3]), 0.721, tol = 1e-2)
            expect_equal(unname(eggerfit1$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit1$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit1$samples), "mcmc.list")
            expect_equal(eggerfit1$priormethod, "weak")
          })

test_that("MR-Egger using pseudo prior method",
          {
            skip_on_cran()
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            eggerfit2 <-
              mr_egger_rjags(dat,
                             prior = "pseudo",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit2), "eggerjags")
            expect_equal(unname(eggerfit2$CausalEffect), 0.567, tol = 1e-2)
            expect_equal(unname(eggerfit2$StandardError), 0.078, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[1]), 0.42, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[2]), 0.566, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[3]), 0.719, tol = 1e-2)
            expect_equal(unname(eggerfit2$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit2$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit2$samples), "mcmc.list")
            expect_equal(eggerfit2$priormethod, "pseudo")
          })

test_that("MR-Egger using joint prior method",
          {
            skip_on_cran()
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            eggerfit3 <-
              mr_egger_rjags(dat,
                             prior = "joint",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit3), "eggerjags")
            expect_equal(unname(eggerfit3$CausalEffect), 0.568, tol = 1e-2)
            expect_equal(unname(eggerfit3$StandardError), 0.076, tol = 1e-2)
            expect_equal(unname(eggerfit3$CredibleInterval[1]), 0.42, tol = 1e-2)
            expect_equal(unname(eggerfit3$CredibleInterval[2]), 0.567, tol = 1e-2)
            expect_equal(unname(eggerfit3$CredibleInterval[3]), 0.72, tol = 1e-2)
            expect_equal(unname(eggerfit3$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit3$sigma), 1.59, tol = 1e-2)
            expect_equal(class(eggerfit3$samples), "mcmc.list")
            expect_equal(eggerfit3$priormethod, "joint")
          })

test_that("MR-Egger using betaprior and sigmaprior method",
          {
            skip_on_cran()
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            eggerfit4 <-
              mr_egger_rjags(dat,
                             betaprior = "dnorm(0, 1E-6)",
                             sigmaprior = "dunif(.0001, 10)",
                             seed = c(123, 456, 789))
            expect_equal(class(eggerfit4), "eggerjags")
            expect_equal(unname(eggerfit4$CausalEffect), 0.569, tol = 1e-2)
            expect_equal(unname(eggerfit4$StandardError), 0.0775, tol = 1e-2)
            expect_equal(unname(eggerfit4$CredibleInterval[1]), 0.42 , tol = 1e-2)
            expect_equal(unname(eggerfit4$CredibleInterval[2]), 0.569, tol = 1e-2)
            expect_equal(unname(eggerfit4$CredibleInterval[3]), 0.721, tol = 1e-2)
            expect_equal(unname(eggerfit4$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit4$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit4$samples), "mcmc.list")
          })
