\name{mwtp}
\alias{mwtp}
\alias{print.mwtp}
\title{
Calculating the marginal willingness to pay
}
\description{
This function calculates the marginal willingness to pay for the attributes and/or levels of the estimated model.
}
\usage{
mwtp(output, monetary.variables, nonmonetary.variables = NULL,
     nreplications = 10000,
     percentile.points = NULL,
     confidence.level = 0.95,
     method = "kr", 
     seed = NULL)

\method{print}{mwtp}(x, digits = max(3, getOption("digits") - 3),
      scientific = FALSE, ...)
}
\arguments{
  \item{output}{
An object containing the output from the function \code{clogit} in the package \pkg{survival} or from the function \code{glm} in the package \pkg{stats}.
}
  \item{monetary.variables}{
A vector containing the names of the monetary variables in the \code{output} used to calculate the MWTPs.
}
  \item{nonmonetary.variables}{
A vector containing the names of the non-monetary variables in the \code{output} used to calculate the MWTPs. Its default is \code{NULL}.
}
  \item{nreplications}{
An integer value denoting the number of replications in the simulation method. The default value is set as \code{10000}.
}
  \item{percentile.points}{
It is only kept for giving an error message regarding unused argument. It will be removed. The argument \code{confidence.level} is used instead.
}
  \item{confidence.level}{
A value showing the confidence level (coefficient) of an empirical distribution of each MWTP. Its default vector is set as \code{0.95}, which indicates the lower and upper bounds of the 95 percent confidence interval.
}
  \item{method}{
A character variable describing the method used for calculating confidence intervals of MWTPs. It is set as "kr" if the Krinsky and Robb's method is used; it is set as "delta" if the delta method is used.
}
  \item{seed}{
Seed for a random number generator.
}
  \item{x}{
An object of S3 class "mwtp."
}
  \item{digits}{
A number of significant digits. See the function \code{format}.
}
  \item{scientific}{
Whether MWTPs and their confidence intervals are encoded in scientific format. See the function \code{format}.
}
  \item{...}{
Arguments passed to the function \code{format}.
}
}
\details{
The definition of the marginal willingness to pay (MWTP) for a non-monetary variable provided by this function is \eqn{-b_{nm}/b_{m}}; where, \eqn{b_{nm}} is the estimated coefficient of the non-monetary variable, and \eqn{b_{m}} is the estimated coefficient of a monetary variable. Further, confidence intervals for the MWTPs are calculated according to the simulation method proposed by Krinsky and Robb (1987) or the delta method (see, e.g., Hole 2007).

In the Krinsky and Robb's method, \emph{N} replications of a vector of the coefficients in the model are randomly sampled from a multivariate normal distribution with a vector of means and a variance-covariance matrix of the estimated coefficients. An empirical distribution for each of the MWTPs can be generated from \emph{N} sets of the replicated coefficients, and a confidence interval for each of the MWTPs is identified on the basis of each empirical distribution.

In the delta method, a variance of MWTP of the non-monetary variable is calculated using estimated coefficients and variance-covariance matrix regarding the non-monetary and monetary variables, and then a confidence interval for the MWTP is calculated.

When the argument \code{nonmonetary.variables} is not set by the user, variables in the argument \code{output}---except for those assigned by the argument \code{monetary.variables}---are treated as non-monetary variables, and the MWTPs for these variables are calculated. In the model that assumes alternative-specific attribute variables (that is, a labeled type choice experiment design), variables in the argument \code{output} are classified into monetary and non-monetary variables according to the alternatives. Therefore, the argument \code{monetary.variables} is set as a vector, whereas the argument \code{nonmonetary.variables} is set as a list of vectors.

In version 0.3-0 and later versions, this function is also available for binary choice models estimated using the function \code{glm}.
}
\value{
This function returns an object of S3 class "mwtp" that is a list with the following components.
  \item{mwtp.table}{A matrix containing the MWTPs for the non-monetary attribute variables and confidence intervals for each of the MWTPs.}
  \item{method}{A character variable containing the method used for calculating confidence intervals of MWTPs.}
  \item{mwtps}{A matrix containing empirical distributions of the MWTPs. It is included when the Krinsky and Robb's method is used.} 
  \item{repb}{A matrix containing N sets of replicated coefficients. It is included when the Krinsky and Robb's method is used.} 
The object \code{mwtps} can be used for a function \code{mded} in the package \pkg{mded} that calculates differences between two independent/dependent empirical distributions of the MWTPs.
}
\references{
Louviere, J. J., Hensher, D. A. and Swait, J. D. (2000) \emph{Stated Choice Methods: Analysis and Application}. Cambridge University Press.

Krinsky, I. and Robb. A. L. (1986) On Approximating the Statistical Properties of Elasticities. \emph{The Review of Economics and Statistics}, \bold{68}, 715--719.

Hole, A. R. (2007) A Comparison of Approaches to Estimating COnfidence Intervals for Willingness to Pay Measures. \emph{Health Economics}, \bold{16}, 827--840.

Aizaki, H. (2012) Basic Functions for Supporting an Implementation of Choice Experiments in R. \emph{Journal of Statistical Software, Code Snippets}, \bold{50}(2), 1--24. \url{http://www.jstatsoft.org/v50/c02/}
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.dataset}}, \code{\link[survival]{clogit}}, \code{\link[stats]{glm}}, \code{\link[mded]{mded}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{survival}
