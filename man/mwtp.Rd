\name{mwtp}
\alias{mwtp}
\title{
Calculating the marginal willingness to pay
}
\description{
This function calculates the marginal willingness to pay for the attributes and/or levels of the estimated model.
}
\usage{
mwtp(output, monetary.variables, nonmonetary.variables = NULL,
     nreplications = 10000,
     percentile.points = c(0.5, 2.5, 5, 95, 97.5, 99.5), 
     decimal.places = 3, seed = NULL)
}
\arguments{
  \item{output}{
A name of an object containing the output from the function \code{clogit} in the package \pkg{survival}.
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
A vector of values showing the percentile points of an empirical distribution of each MWTP. Its default vector is set as \code{c(0.5, 2.5, 5, 95, 97.5, 99.5)}, which indicates the lower and upper bounds of the 99, 95, and 90 percent confidence intervals.
}
  \item{decimal.places}{
An integer value indicating the number of decimal places. Its default value is set as \code{3}.
}
  \item{seed}{
Seed for a random number generator.
}
}
\details{
The definition of the marginal willingness to pay (MWTP) for a non-monetary variable provided by this function is \emph{--bnm/bm}; where, \emph{bnm} is the estimated coefficient of the non-monetary variable, and \emph{bm} is the estimated coefficient of a monetary variable. Further, confidence intervals for the MWTPs are calculated according to the simulation method proposed by Krinsky and Robb (1987): \emph{N} replications of a vector of the coefficients in the conditional logit model are randomly sampled from a multivariate normal distribution with a vector of means and a variance-covariance matrix of the estimated coefficients. An empirical distribution for each of the MWTPs can be generated from \emph{N} sets of the replicated coefficients, and a confidence interval for each of the MWTPs is identified on the basis of each empirical distribution.

When the argument \code{nonmonetary.variables} is not set by the user, variables in the argument \code{output}---except for those assigned by the argument \code{monetary.variables}---are treated as non-monetary variables, and the MWTPs for these variables are calculated. In the model that assumes alternative-specific attribute variables (that is, a labeled type choice experiment design), variables in the argument \code{output} are classified into monetary and non-monetary variables according to the alternatives. Therefore, the argument \code{monetary.variables} is set as a vector, whereas the argument \code{nonmonetary.variables} is set as a list of vectors.
}
\value{
The MWTPs for the non-monetary attribute variables and confidence intervals for each of the MWTPs. 
}
\references{
Louviere, J. J., Hensher, D. A. and Swait, J. D. (2000) \emph{Stated Choice Methods: Analysis and Application}. Cambridge University Press.

Krinsky, I. and Robb. A. L. (1986) On Approximating the Statistical Properties of Elasticities. \emph{The Review of Economics and Statistics}, \bold{68}, 715--719.
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.dataset}}, \code{\link[survival]{clogit}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{survival}
