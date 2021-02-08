options(mc.cores = parallel::detectCores())
if (requireNamespace("parallel", quietly = TRUE)) {
  cores <- parallel::detectCores()
  usecores <- ceiling(0.75*cores)
  options(Ncpus = usecores)
}
