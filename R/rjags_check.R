# Check for rjags

rjags_check <- function() {
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("Package \"rjags\" needed for this function to work. Please install it.",
         call. = FALSE)
}}
