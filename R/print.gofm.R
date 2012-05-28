print.gofm <- function(x, digits = getOption("digits"), ...)
{
  cat("\nRho-squared =", format(x$RHO2, digits = digits, ...), "\n")
  cat("Adjusted rho-squared =", format(x$AdjRHO2, digits = digits, ...), "\n")
  cat("Number of coefficients =", x$K, "\n")
  cat("Log likelihood at start =", format(x$LL0, digits = digits, ...), "\n")
  cat("Log likelihood at convergence =", format(x$LLb, digits = digits, ...), "\n\n")

  invisible(x)
}

