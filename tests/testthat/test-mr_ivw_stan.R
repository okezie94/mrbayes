# Tests for the mrbayes package

context("Tests for mr_ivw_stan() function")

# Analysis
## IVW stan

test_that("IVW using default prior method",
          {
            ivwfit <- mr_ivw_stan(do_data)
            summfit <- rstan::summary(ivwfit)
            expect_equal(class(ivwfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit$summary["estimate","sd"], 0.04, tol = 1e-2)
            expect_equal(summfit$summary["estimate","2.5%"], 0.43, tol = 1e-2)
            expect_equal(summfit$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit$summary["estimate","97.5%"], 0.57, tol = 1e-2)
          })

test_that("IVW using weak prior method",
          {
            ivwfit1 <- mr_ivw_stan(do_data, prior = 2)
            summfit1 <- rstan::summary(ivwfit1)
            expect_equal(class(ivwfit1)[1], "stanfit")
            expect_equal(summfit1$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","sd"], 0.037, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","2.5%"], 0.43, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","50%"], 0.5, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","97.5%"], 0.576, tol = 1e-2)
          })

test_that("IVW using pseudo prior method",
          {
            ivwfit2 <- mr_ivw_stan(do_data, prior = 3)
            summfit2 <- rstan::summary(ivwfit2)
            expect_equal(class(ivwfit2)[1], "stanfit")
            expect_equal(summfit2$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","sd"], 0.0379, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","2.5%"], 0.429, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","97.5%"], 0.576, tol = 1e-2)
          })
