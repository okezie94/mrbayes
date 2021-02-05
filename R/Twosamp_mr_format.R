Twosamp_mr_format <- function(dat)
{
  out <- plyr::dlply(dat, c("exposure", "outcome"), function(x)
  {
    x <- plyr::mutate(x)
    message("Converting:")
    message(" - exposure: ", x$exposure[1])
    message(" - outcome: ", x$outcome[1])
    d <- subset(x, mr_keep=TRUE)
    d <- mr_format(d$SNP,d$beta.exposure, d$beta.outcome, d$se.exposure, d$se.outcome)
    return(d)
  })
  out <- out[[1]]
  return(out)
}
