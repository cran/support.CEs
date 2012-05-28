\name{gofm}
\alias{gofm}
\alias{print.gofm}
\title{
Calculating goodness-of-fit measures for the conditional logit model
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
A name of an object containing the output from the function \code{clogit}.
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
This function provides rho-squared (also called McFadden's R-squared or pseudo R-squared), rho-squared adjusted by the number of estimated coefficients, the number of estimated coefficients, and log likelihood values at the start and at convergence.
}
\value{
This function returns an object of S3 class "gofm" that is a list with the following components.
  \item{RHO2}{The rho-squared value. Defined as \code{RHO2 = 1 - (LLb / LL0)}. }
  \item{AdjRHO2}{The rho-squared value adjusted by the number of estimated coefficients. Defined as \code{AdjRHO2 = 1 - ((LLb - K) / LL0)}.}
  \item{K}{The number of estimated coefficients.}
  \item{LL0}{The log likelihood value at the start.}
  \item{LLb}{The log likelihood value at convergence.}
}
\references{
Ben-Akiva, M. and Lerman, S. R. (1985) \emph{Discrete Choice Analysis: Theory and Application to Travel Demand}. The MIT Press.
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link[survival]{clogit}}, \code{\link{make.dataset}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{survival}
