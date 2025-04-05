# Tests for the mrbayes package

context("Tests for mr_radialegger_stan() function")

skip_if(Sys.info()[["sysname"]] == "Emscripten", message = "Skip tests on Emscripten aka WebR.")

# Analysis
## MR-RadialEgger stan

test_that("Dataset is formatted",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)
            expect_s3_class(dat, "mr_format")
          })

test_that("MR-Radialegger using default prior method",
          {
            skip_on_cran()
            skip_if_not_installed("rstan")
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mrradialeggerfit <- mr_radialegger_stan(dat)
            summfit <- rstan::summary(mrradialeggerfit)
            expect_equal(class(mrradialeggerfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate", "mean"], 0.591, tol = 1e-2)
            expect_equal(summfit$summary["estimate", "sd"], 0.08, tol = 1e-2)
            expect_equal(summfit$summary["estimate", "2.5%"], 0.43, tol = 1e-2)
            expect_equal(summfit$summary["estimate", "50%"], 0.59, tol = 1e-2)
            expect_equal(summfit$summary["estimate", "97.5%"], 0.75, tol = 1e-2)
          })

test_that("MR-Radialegger using weak prior method",
          {
            skip_on_cran()
            skip_if_not_installed("rstan")
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mrradialeggerfit1 <- mr_radialegger_stan(dat, prior = 2)
            summfit1 <- rstan::summary(mrradialeggerfit1)
            expect_equal(class(mrradialeggerfit1)[1], "stanfit")
            expect_equal(summfit1$summary["estimate", "mean"], 0.59, tol = 1e-2)
            expect_equal(summfit1$summary["estimate", "sd"], 0.08, tol = 1e-2)
            expect_equal(summfit1$summary["estimate", "2.5%"], 0.43, tol = 1e-2)
            expect_equal(summfit1$summary["estimate", "50%"], 0.58, tol = 1e-2)
            expect_equal(summfit1$summary["estimate", "97.5%"], 0.75, tol = 1e-2)
          })

test_that("MR-Radialegger using pseudo prior method",
          {
            skip_on_cran()
            skip_if_not_installed("rstan")
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mrradialeggerfit2 <- mr_radialegger_stan(dat, prior = 3)
            summfit2 <- rstan::summary(mrradialeggerfit2)
            expect_equal(class(mrradialeggerfit2)[1], "stanfit")
            expect_equal(summfit2$summary["estimate", "mean"], 0.586, tol = 1e-2)
            expect_equal(summfit2$summary["estimate", "sd"], 0.08, tol = 1e-2)
            expect_equal(summfit2$summary["estimate", "2.5%"], 0.43, tol = 1e-2)
            expect_equal(summfit2$summary["estimate", "50%"], 0.58, tol = 1e-2)
            expect_equal(summfit2$summary["estimate", "97.5%"], 0.75, tol = 1e-2)
          })

test_that("MR-Radialegger using joint prior method",
          {
            skip_on_cran()
            skip_if_not_installed("rstan")
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mrradialeggerfit3 <- mr_radialegger_stan(dat, prior = 4)
            summfit3 <- rstan::summary(mrradialeggerfit3)
            expect_equal(class(mrradialeggerfit3)[1], "stanfit")
            expect_equal(summfit3$summary["estimate", "mean"], 0.591, tol = 1e-2)
            expect_equal(summfit3$summary["estimate", "sd"], 0.08, tol = 1e-2)
            expect_equal(summfit3$summary["estimate", "2.5%"], 0.43, tol = 1e-2)
            expect_equal(summfit3$summary["estimate", "50%"], 0.59, tol = 1e-2)
            expect_equal(summfit3$summary["estimate", "97.5%"], 0.75, tol = 1e-2)
          })
