# Tests for the mrbayes package

context("Tests for mr_ivw_stan() function")

# Analysis
## IVW stan

test_that("IVW using default prior method",
          {
            ivwfit <- mr_ivw_stan(do_data, seed = 123)
            summfit <- rstan::summary(ivwfit)
            expect_equal(class(ivwfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit$summary["estimate","sd"], 0.05, tol = 1e-2) # 0.04 ??
            expect_equal(summfit$summary["estimate","2.5%"], 0.38, tol = 1e-2) # .44 ??
            expect_equal(summfit$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit$summary["estimate","97.5%"], 0.61, tol = 1e-2) # .58 ??
          })
#

test_that("IVW using weak prior method",
          {
            ivwfit1 <- mr_ivw_stan(do_data, prior = 2, seed = 123)
            summfit1 <- rstan::summary(ivwfit1)
            expect_equal(class(ivwfit1)[1], "stanfit")
            expect_equal(summfit1$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","sd"], 0.058, tol = 1e-2) # .04
            expect_equal(summfit1$summary["estimate","2.5%"], 0.38, tol = 1e-2) # .44
            expect_equal(summfit1$summary["estimate","50%"], 0.5, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","97.5%"], 0.61, tol = 1e-2) # .58
          })

test_that("IVW using pseudo prior method",
          {
            ivwfit2 <- mr_ivw_stan(do_data, prior = 3, seed = 123)
            summfit2 <- rstan::summary(ivwfit2)
            expect_equal(class(ivwfit2)[1], "stanfit")
            expect_equal(summfit2$summary["estimate","mean"], 0.5, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","sd"], 0.057, tol = 1e-2) # .03
            expect_equal(summfit2$summary["estimate","2.5%"], 0.387, tol = 1e-2) # .43
            expect_equal(summfit2$summary["estimate","50%"], 0.50, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","97.5%"], 0.605, tol = 1e-2) # .57
          })
