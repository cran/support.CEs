print.mwtp <- function(x, digits = max(3, getOption("digits") - 3), scientific = FALSE, ...)
{
  cat("\n")
  print(format(x$mwtp.table, digits = digits, scientific = scientific, ...), quote = FALSE)
  cat("\n")

  invisible(x)
}

