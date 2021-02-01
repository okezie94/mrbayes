Twosamp_mr_format <- function(dat)
{
  out <- plyr::dlply(dat, c("exposure", "outcome"), function(x)
  {
    x <- plyr::mutate(x)
    message("Converting:")
    message(" - exposure: ", x$exposure[1])
    message(" - outcome: ", x$outcome[1])
    d <- subset(x, mr_keep=TRUE)
    d <- mrbayes::mr_format(d$beta.exposure, d$beta.outcome, d$se.exposure, d$se.outcome, RSID=d$SNP)
    return(d)
  })
  return(out)
}
