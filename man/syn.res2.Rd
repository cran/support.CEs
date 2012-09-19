\name{syn.res2}
\alias{syn.res2}
\docType{data}
\title{
Synthetic respondent data set (2)
}
\description{
A data set artificially created for the "Examples" in this package. 
}
\usage{data(syn.res2)}
\format{
  A data frame with 100 respondents on the following 11 variables.
  \describe{
    \item{\code{ID}}{Identification number of respondents}
    \item{\code{BLOCK}}{Serial number of blocks to which each respondent had been assigned}
    \item{\code{q1}}{Response to choice experiment question 1}
    \item{\code{q2}}{Response to choice experiment question 2}
    \item{\code{q3}}{Response to choice experiment question 3}
    \item{\code{q4}}{Response to choice experiment question 4}
    \item{\code{q5}}{Response to choice experiment question 5}
    \item{\code{q6}}{Response to choice experiment question 6}
    \item{\code{q7}}{Response to choice experiment question 7}
    \item{\code{q8}}{Response to choice experiment question 8}
    \item{\code{q9}}{Response to choice experiment question 9}
  }
}
\details{
The "Examples" for each function in this package is based on two hypothetical cases in which choice experiments are applied to measure consumers' valuation of an agricultural product. 

The data set \code{syn.res2} is based on a case in which a labeled choice experiment design created by the \emph{L^MA} method is used in a questionnaire survey. A total of 100 respondents were assumed to have been requested to select their most preferred from among three agricultural products and the option "none of these."

\tabular{c}{ \cr}

Q1. Please select your most preferred alternative from the following:

\tabular{llll}{
             \tab \tab \tab \cr
             \tab Region A \tab Region B \tab Region C \cr
Eco-friendly \tab Most \tab Conv. \tab More \cr
Price \tab $1.1 \tab $1 \tab $1.1 \cr
}

\itemize{
\item{I select the Region A product.}
\item{I select the Region B product.}
\item{I select the Region C product.}
\item{I select none of these.}
}

\tabular{c}{ \cr}

Although the agricultural products have three attributes such as the region of origin, the eco-friendly label and the price (see "Details" for the data set \code{syn.res1}), the region of origin attribute was treated as an alternative specific attribute: the first, second, and third alternatives in a choice set always read as "Region A," "Region B," and "Region C," respectively. 

A total of 18 choice experiment questions were created by the function \code{Lma.design} and divided into two blocks (see "Examples" for the function \code{Lma.design}): this means that two types of questionnaire were created, each of which was randomly assigned to the respondents. Therefore, the sample size of the analysis based on their responses was 900 (= 9 CE questions per respondent * 100 respondents).
}
\author{
Hideo Aizaki
}
\references{
Aizaki, H. (2012) Basic Functions for Supporting an Implementation of Choice Experiments in R. \emph{Journal of Statistical Software, Code Snippets}, \bold{50}(2), 1--24. \url{http://www.jstatsoft.org/v50/c02/}
}
\seealso{
 \code{\link{make.dataset}}, \code{\link{Lma.design}}, \code{\link{syn.res1}}
}
\examples{
data(syn.res2)
syn.res2
}
\keyword{datasets}
