# Tests for the mrbayes package

context("Tests for mr_radialegger_stan() function")

# Analysis
## MR-RadialEgger stan

test_that("MR-Radialegger using default prior method",
          {
            mrradialeggerfit <- mr_radialegger_stan(do_data)
            summfit <- rstan::summary(mrradialeggerfit)
            expect_equal(class(mrradialeggerfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })
#

test_that("MR-Radialegger using weak prior method",
          {
            mrradialeggerfit1 <- mr_radialegger_stan(do_data, prior = 2)
            summfit1 <- rstan::summary(mrradialeggerfit1)
            expect_equal(class(mrradialeggerfit1)[1], "stanfit")
            expect_equal(summfit1$summary["estimate","mean"], 0.51, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","50%"], 0.51, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })

test_that("MR-Radialegger using pseudo prior method",
          {
            mrradialeggerfit2 <- mr_radialegger_stan(do_data, prior = 3)
            summfit2 <- rstan::summary(mrradialeggerfit2)
            expect_equal(class(mrradialeggerfit2)[1], "stanfit")
            expect_equal(summfit2$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","50%"], 0.5, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })

test_that("MR-Radialegger using joint prior method",
          {
            mrradialeggerfit3 <- mr_radialegger_stan(do_data, prior = 4)
            summfit3 <- rstan::summary(mrradialeggerfit3)
            expect_equal(class(mrradialeggerfit3)[1], "stanfit")
            expect_equal(summfit3$summary["estimate","mean"], 0.51, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","50%"], 0.51, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })
