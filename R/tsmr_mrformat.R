#' @importFrom plyr mutate dlply
tsmr_mrformat <- function(dat){
  dat <- plyr::dlply(dat,c("exposure","outcome"),
                     function(x)
                       {
                       x <- plyr::mutate(x)
                       message("Converting:")
                       message("-exposure: ",
                               x$exposure[1])
                       message("-outcome: ",
                               x$outcome[1])
                       out <- subset(x, mr_keep = TRUE)
                       out <- mr_format(out$SNP,out$beta.exposure,out$beta.outcome,out$se.exposure,out$se.outcome)
                       out <- out[[1]]
                       return(out)
                     })
  return(dat)
}
