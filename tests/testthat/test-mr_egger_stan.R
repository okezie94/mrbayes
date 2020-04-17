# Tests for the mrbayes package

context("Tests for mr_egger_stan() function")

# Analysis
## MR-Egger stan

test_that("MR-Egger using default prior method",
          {
            mreggerfit <- mr_egger_stan(do_data)
            summfit <- rstan::summary(mreggerfit)
            expect_equal(class(mreggerfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })

test_that("MR-Egger using weak prior method",
          {
            mreggerfit1 <- mr_egger_stan(do_data, prior = 2)
            summfit1 <- rstan::summary(mreggerfit1)
            expect_equal(class(mreggerfit1)[1], "stanfit")
            expect_equal(summfit1$summary["estimate","mean"], 0.51, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","50%"], 0.51, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })

test_that("MR-Egger using pseudo prior method",
          {
            mreggerfit2 <- mr_egger_stan(do_data, prior = 3)
            summfit2 <- rstan::summary(mreggerfit2)
            expect_equal(class(mreggerfit2)[1], "stanfit")
            expect_equal(summfit2$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","50%"], 0.5, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })

test_that("MR-Egger using joint prior method",
          {
            mreggerfit3 <- mr_egger_stan(do_data, prior = 4)
            summfit3 <- rstan::summary(mreggerfit3)
            expect_equal(class(mreggerfit3)[1], "stanfit")
            expect_equal(summfit3$summary["estimate","mean"], 0.51, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","sd"], 0.06, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","2.5%"], 0.39, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","50%"], 0.51, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","97.5%"], 0.62, tol = 1e-2)
          })
