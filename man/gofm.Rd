\name{gofm}
\alias{gofm}
\alias{print.gofm}
\title{
Calculating goodness-of-fit measures
}
\description{
This function provides rho-squared and rho-squared adjusted by the number of estimated coefficients.
}
\usage{
gofm(output)

\method{print}{gofm}(x, digits = getOption("digits"), ...)
}
\arguments{
  \item{output}{
An object containing the output from the function \code{clogit} or \code{glm}.
}
  \item{x}{
An object of class "gofm."
}
  \item{digits}{
A number of significant digits.
}
  \item{...}{
Arguments passed to the function \code{format}.
}
}
\details{
This function provides rho-squared (also called McFadden's R-squared or pseudo R-squared), rho-squared adjusted by the number of estimated coefficients, the number of estimated coefficients, and log likelihood values at the start and at convergence (see, e.g., Ben-Akiva and Lerman 1985; Cameron and Trivedi 2005).

In version 0.3-0 and later versions, this function is also available for calculating goodness-of-fit measures for binary choice models estimated by using the function \code{glm} in the package \pkg{stats}.
}
\value{
This function returns an object of S3 class "gofm" that is a list with the following components.
  \item{RHO2}{The rho-squared value. Defined as \code{RHO2 = 1 - (LLb / LL0)}. }
  \item{AdjRHO2}{The rho-squared value adjusted by the number of estimated coefficients. Defined as \code{AdjRHO2 = 1 - ((LLb - K) / LL0)}.}
  \item{AIC}{The Akaike Information Criterion (AIC).}
  \item{BIC}{The Bayesian Information Criterion.}
  \item{K}{The number of estimated coefficients.}
  \item{LL0}{The log likelihood value at the start.}
  \item{LLb}{The log likelihood value at convergence.}
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link[survival]{clogit}}, \code{\link[stats]{glm}}, \code{\link{make.dataset}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{survival}
