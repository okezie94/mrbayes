mrinput_mr_format <- function(dat) {
  if (!("MRInput" %in% class(dat))) {
    stop('The class of the input data object must be "MRInput"')
  }

  out <- mr_format(xbeta = dat@betaX,
                       ybeta = dat@betaY,
                       xse = dat@betaXse,
                       yse = dat@betaYse,
                       rsid = dat@snps)
  out
}

