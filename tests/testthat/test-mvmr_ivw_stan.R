context("Tests for MVIVW function using STAN")

test_that("Dataset is formatted",
          {
            dat <- mvmr_format(rsid = dodata$rsid,
                               xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
                               ybeta = dodata$chdbeta,
                               xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
                               yse = dodata$chdse)
            expect_s3_class(dat, "mvmr_format")
          })


test_that("IVW using default prior method",
          {
            skip_on_cran()
            dat <- mvmr_format(rsid = dodata$rsid,
                               xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
                               ybeta = dodata$chdbeta,
                               xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
                               yse = dodata$chdse)

            ivwfit <- mvmr_ivw_stan(dat, prior = 1)
            summfit <- rstan::summary(ivwfit)
            expect_equal(class(ivwfit)[1], "stanfit")
            expect_equal(summfit$summary["estimate[1]","mean"], 0.424, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","sd"], 0.038, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","2.5%"], 0.349, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","50%"], 0.424, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","97.5%"], 0.498, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","mean"], -0.117, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","sd"], 0.04, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","2.5%"], -0.198, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","50%"], -0.117, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","97.5%"], -0.0365, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","mean"], 0.281, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","sd"], 0.051, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","2.5%"], 0.176, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","50%"], 0.282, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","97.5%"], 0.381, tol = 1e-2)
          })

test_that("IVW using weak prior method",
          {
            skip_on_cran()
            dat <- mvmr_format(rsid = dodata$rsid,
                               xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
                               ybeta = dodata$chdbeta,
                               xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
                               yse = dodata$chdse)

            ivwfit1 <- mvmr_ivw_stan(dat, prior = 1)
            summfit <- rstan::summary(ivwfit1)
            expect_equal(class(ivwfit1)[1], "stanfit")
            expect_equal(summfit$summary["estimate[1]","mean"], 0.424, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","sd"], 0.038, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","2.5%"], 0.349, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","50%"], 0.424, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","97.5%"], 0.498, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","mean"], -0.117, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","sd"], 0.04, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","2.5%"], -0.198, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","50%"], -0.117, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","97.5%"], -0.0365, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","mean"], 0.281, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","sd"], 0.051, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","2.5%"], 0.176, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","50%"], 0.282, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","97.5%"], 0.381, tol = 1e-2)
          })

test_that("IVW using pseudo prior method",
          {
            skip_on_cran()
            dat <- mvmr_format(rsid = dodata$rsid,
                               xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
                               ybeta = dodata$chdbeta,
                               xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
                               yse = dodata$chdse)


            ivwfit2 <- mvmr_ivw_stan(dat, prior = 2)
            summfit <- rstan::summary(ivwfit2)
            expect_equal(class(ivwfit2)[1], "stanfit")
            expect_equal(summfit$summary["estimate[1]","mean"], 0.424, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","sd"], 0.038, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","2.5%"], 0.349, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","50%"], 0.424, tol = 1e-2)
            expect_equal(summfit$summary["estimate[1]","97.5%"], 0.498, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","mean"], -0.117, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","sd"], 0.04, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","2.5%"], -0.198, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","50%"], -0.117, tol = 1e-2)
            expect_equal(summfit$summary["estimate[2]","97.5%"], -0.0365, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","mean"], 0.281, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","sd"], 0.051, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","2.5%"], 0.176, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","50%"], 0.282, tol = 1e-2)
            expect_equal(summfit$summary["estimate[3]","97.5%"], 0.381, tol = 1e-2)
          })
