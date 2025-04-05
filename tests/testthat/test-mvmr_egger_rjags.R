## MR-Egger Rjags

context("Tests for MR-Egger function using JAGS")

skip_if(Sys.info()[["sysname"]] == "Emscripten", message = "Skip tests on Emscripten aka WebR.")

test_that("Dataset is formatted",
          {
            dat <- mvmr_format(rsid = dodata$rsid,
                               xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
                               ybeta = dodata$chdbeta,
                               xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
                               yse = dodata$chdse)
            expect_s3_class(dat, "mvmr_format")
          })

test_that("MR-Egger using default prior method",
          {
            skip_on_cran()
            skip_if_not_installed("rjags")
            dat <- mvmr_format(rsid = dodata$rsid,
                               xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
                               ybeta = dodata$chdbeta,
                               xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
                               yse = dodata$chdse)

            eggerfit <- mvmr_egger_rjags(dat,
                                         seed = c(123, 456, 789))
            expect_equal(class(eggerfit), "mveggerjags")
            expect_equal(unname(eggerfit$CausalEffect[1]), 0.531, tol = 1e-2)
            expect_equal(unname(eggerfit$CausalEffect[2]), -0.104, tol = 1e-2)
            expect_equal(unname(eggerfit$CausalEffect[3]), 0.326, tol = 1e-2)
            expect_equal(unname(eggerfit$StandardError[1]), 0.0711, tol = 1e-2)
            expect_equal(unname(eggerfit$StandardError[2]), 0.0615, tol = 1e-2)
            expect_equal(unname(eggerfit$StandardError[3]), 0.08, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[1]), 0.391, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[2]), -0.234, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[3]), 0.176, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[4]), 0.53, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[5]), -0.104, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[6]), 0.326, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[7]), 0.671, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[8]), 0.0167, tol = 1e-2)
            expect_equal(unname(eggerfit$CredibleInterval[9]), 0.474, tol = 1e-2)
            expect_equal(unname(eggerfit$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit$sigma), 1.46, tol = 1e-2)
            expect_equal(class(eggerfit$samples), "mcmc.list")
            expect_equal(eggerfit$priormethod, "default")
          })

test_that("MR-Egger using weak prior method",
            {
              skip_on_cran()
              skip_if_not_installed("rjags")
              dat <- mvmr_format(rsid = dodata$rsid,
                                 xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
                                 ybeta = dodata$chdbeta,
                                 xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
                                 yse = dodata$chdse)

              eggerfit1 <- mvmr_egger_rjags(dat,
                                           seed = c(123, 456, 789),
                                           prior = "weak")
              expect_equal(class(eggerfit1), "mveggerjags")
              expect_equal(unname(eggerfit1$CausalEffect[1]), 0.531, tol = 1e-2)
              expect_equal(unname(eggerfit1$CausalEffect[2]), -0.104, tol = 1e-2)
              expect_equal(unname(eggerfit1$CausalEffect[3]), 0.326, tol = 1e-2)
              expect_equal(unname(eggerfit1$StandardError[1]), 0.0711, tol = 1e-2)
              expect_equal(unname(eggerfit1$StandardError[2]), 0.062, tol = 1e-2)
              expect_equal(unname(eggerfit1$StandardError[3]), 0.08, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[1]), 0.391, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[2]), -0.234, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[3]), 0.176, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[4]), 0.53, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[5]), -0.104, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[6]), 0.326, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[7]), 0.671, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[8]), 0.017, tol = 1e-2)
              expect_equal(unname(eggerfit1$CredibleInterval[9]), 0.47, tol = 1e-2)
              expect_equal(unname(eggerfit1$AvgPleio), -0.002, tol = 1e-2)
              expect_equal(unname(eggerfit1$sigma), 1.46, tol = 1e-2)
              expect_equal(class(eggerfit1$samples), "mcmc.list")
              expect_equal(eggerfit1$priormethod, "weak")
            })


# test_that("MR-Egger using joint prior method",
#           {
#             skip_on_cran()
#             skip_if_not_installed("rjags")
#             dat <- mr_format(rsid = dodata$rsid,
#                              xbeta = dodata$ldlcbeta,
#                              ybeta = dodata$chdbeta,
#                              xse = dodata$ldlcse,
#                              yse = dodata$chdse)
#             eggerfit3 <-
#               mr_egger_rjags(dat,
#                              prior = "joint",
#                              seed = c(123, 456, 789))
#             expect_equal(class(eggerfit3), "eggerjags")
#             expect_equal(unname(eggerfit3$CausalEffect), 0.568, tol = 1e-2)
#             expect_equal(unname(eggerfit3$StandardError), 0.076, tol = 1e-2)
#             expect_equal(unname(eggerfit3$CredibleInterval[1]), 0.42, tol = 1e-2)
#             expect_equal(unname(eggerfit3$CredibleInterval[2]), 0.567, tol = 1e-2)
#             expect_equal(unname(eggerfit3$CredibleInterval[3]), 0.72, tol = 1e-2)
#             expect_equal(unname(eggerfit3$AvgPleio), -0.002, tol = 1e-2)
#             expect_equal(unname(eggerfit3$sigma), 1.59, tol = 1e-2)
#             expect_equal(class(eggerfit3$samples), "mcmc.list")
#             expect_equal(eggerfit3$priormethod, "joint")
#           })

test_that("MR-Egger using betaprior and sigmaprior method",
          {
            skip_on_cran()
            skip_if_not_installed("rjags")
              dat <- mvmr_format(rsid = dodata$rsid,
                                 xbeta = cbind(dodata$ldlcbeta, dodata$hdlcbeta, dodata$tgbeta),
                                 ybeta = dodata$chdbeta,
                                 xse = cbind(dodata$ldlcse, dodata$hdlcse, dodata$tgse),
                                 yse = dodata$chdse)

              eggerfit3 <- mvmr_egger_rjags(dat,
                                           seed = c(123, 456, 789),
                                           betaprior = "dnorm(0, 1E-3)",
                                           sigmaprior = "dunif(0.001,1)")
              expect_equal(class(eggerfit3), "mveggerjags")
              expect_equal(unname(eggerfit3$CausalEffect[1]), 0.531, tol = 1e-2)
              expect_equal(unname(eggerfit3$CausalEffect[2]), -0.104, tol = 1e-2)
              expect_equal(unname(eggerfit3$CausalEffect[3]), 0.326, tol = 1e-2)
              expect_equal(unname(eggerfit3$StandardError[1]), 0.048, tol = 1e-2)
              expect_equal(unname(eggerfit3$StandardError[2]), 0.042, tol = 1e-2)
              expect_equal(unname(eggerfit3$StandardError[3]), 0.048, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[1]), 0.437, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[2]), -0.177, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[3]), 0.225, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[4]), 0.531, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[5]), -0.094, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[6]), 0.326, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[7]), 0.625, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[8]), -0.0224, tol = 1e-2)
              expect_equal(unname(eggerfit3$CredibleInterval[9]), 0.427, tol = 1e-2)
              expect_equal(unname(eggerfit3$AvgPleio), -0.002, tol = 1e-2)
              expect_equal(unname(eggerfit3$sigma), 0.997, tol = 1e-2)
              expect_equal(class(eggerfit3$samples), "mcmc.list")
              expect_equal(eggerfit3$priormethod, "default")
            })
