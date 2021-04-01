# Tests for the mrbayes package

context("Tests for mr_egger_stan() function")

# Analysis
## MR-Egger stan

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
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mreggerfit <- mr_egger_stan(dat)
            summfit <- rstan::summary(mreggerfit)
            expect_equal(class(mreggerfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate","mean"], 0.568, tol = 1e-2)
            expect_equal(summfit$summary["estimate","sd"], 0.0752, tol = 1e-2)
            expect_equal(summfit$summary["estimate","2.5%"], 0.42, tol = 1e-2)
            expect_equal(summfit$summary["estimate","50%"], 0.568, tol = 1e-2)
            expect_equal(summfit$summary["estimate","97.5%"], 0.72, tol = 1e-2)
          })

test_that("MR-Egger using weak prior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mreggerfit1 <- mr_egger_stan(dat, prior = 2)
            summfit1 <- rstan::summary(mreggerfit1)
            expect_equal(class(mreggerfit1)[1], "stanfit")
            expect_equal(summfit1$summary["estimate","mean"], 0.57, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","sd"], 0.076, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","2.5%"], 0.42, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","50%"], 0.571, tol = 1e-2)
            expect_equal(summfit1$summary["estimate","97.5%"], 0.72, tol = 1e-2)
          })

test_that("MR-Egger using pseudo prior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mreggerfit2 <- mr_egger_stan(dat, prior = 3)
            summfit2 <- rstan::summary(mreggerfit2)
            expect_equal(class(mreggerfit2)[1], "stanfit")
            expect_equal(summfit2$summary["estimate","mean"], 0.566, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","sd"], 0.0755, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","2.5%"], 0.417, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","50%"], 0.566, tol = 1e-2)
            expect_equal(summfit2$summary["estimate","97.5%"], 0.713, tol = 1e-2)
          })

test_that("MR-Egger using joint prior method",
          {
            dat <- mr_format(rsid = dodata$rsid,
                             xbeta = dodata$ldlcbeta,
                             ybeta = dodata$chdbeta,
                             xse = dodata$ldlcse,
                             yse = dodata$chdse)

            mreggerfit3 <- mr_egger_stan(dat, prior = 4)
            summfit3 <- rstan::summary(mreggerfit3)
            expect_equal(class(mreggerfit3)[1], "stanfit")
            expect_equal(summfit3$summary["estimate","mean"], 0.568, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","sd"], 0.078, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","2.5%"], 0.415, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","50%"], 0.568, tol = 1e-2)
            expect_equal(summfit3$summary["estimate","97.5%"], 0.722, tol = 1e-2)
          })
