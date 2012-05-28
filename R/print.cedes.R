print.cedes <- function(x, ...)
{
  nalt <- x$design.information$nalternatives
  cat("\nChoice sets:\n")
  for(i in 1:nalt) {
    cat("alternative", i, "in each choice set\n")
    print(x$alternatives[[i]], ...)
    cat("\n")
  }

  cat("Candidate design:\n")
  print(x$candidate, ...)
  cat("\n")

  cat("Design information:\n")
  cat("number of blocks =", x$design.information$nblocks, "\n")
  cat("number of questions per block =", x$design.information$nquestions, "\n")
  cat("number of alternatives per choice set =", x$design.information$nalternatives, "\n")
  cat("number of attributes per alternative =", x$design.information$nattributes, "\n\n")

  invisible(x)
}

