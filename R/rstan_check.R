# Check for rstan

rstan_check <- function() {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package \"rstan\" needed for this function to work. Please install it.",
         call. = FALSE)
  }}
