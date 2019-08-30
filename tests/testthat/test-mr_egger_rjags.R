## MR-Egger Rjags

test_that("Check the class of the data object",
          {
            mrdat <- with(do_data, mr_format(rsid, ldlcbeta, chdbeta, ldlcse, chdse))
            expect_equal(class(mrdat), c("data.frame","mr_format"))
          })


mrdat <-
  with(do_data, mr_format(rsid, ldlcbeta, chdbeta, ldlcse, chdse))

test_that("MR-Egger using default prior method",
          {
            skip_on_cran()
            eggerfit <- mr_egger_rjags(mrdat, seed = 123)

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
            skip_on_cran()
            eggerfit1 <-
              mr_egger_rjags(mrdat, prior = "weak", seed = 123)

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
            skip_on_cran()
            eggerfit2 <-
              mr_egger_rjags(mrdat, prior = "pseudo", seed = 123)

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
            eggerfit2 <-
              mr_egger_rjags(mrdat, prior = "joint", seed = 123)

            expect_equal(class(eggerfit2), "eggerjags")
            expect_equal(unname(eggerfit2$CausalEffect), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit2$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[1]), 0.39, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[2]), 0.51, tol = 1e-2)
            expect_equal(unname(eggerfit2$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit2$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit2$sigma), 1.59, tol = 1e-2)
            expect_equal(class(eggerfit2$samples), "mcmc.list")
            expect_equal(eggerfit2$priormethod, "joint")
          })


test_that("MR-Egger using betaprior and sigmaprior method",
          {
            skip_on_cran()
            eggerfit1 <-
              mr_egger_rjags(mrdat,
                             betaprior = "dnorm(0, 1E-6)",
                             sigmaprior = "dunif(.0001, 10)",
                             seed = 123)
            expect_equal(class(eggerfit1), "eggerjags")
            expect_equal(unname(eggerfit1$CausalEffect), 0.51, tol = 1e-2)
            expect_equal(unname(eggerfit1$StandardError), 0.06, tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[1]), 0.39 , tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[2]), 0.5, tol = 1e-2)
            expect_equal(unname(eggerfit1$CredibleInterval[3]), 0.62, tol = 1e-2)
            expect_equal(unname(eggerfit1$AvgPleio), -0.002, tol = 1e-2)
            expect_equal(unname(eggerfit1$sigma), 1.58, tol = 1e-2)
            expect_equal(class(eggerfit1$samples), "mcmc.list")
          })
