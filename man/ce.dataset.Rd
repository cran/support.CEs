\name{ce.dataset}
\alias{ce.dataset}
\title{
Making a data set from a choice experiment design and respondent data set
}
\description{
This function combines \code{make.design.matrix} and \code{make.dataset}. It returns a data set for analysis that is similar to one from \code{make.dataset}.
}
\usage{
ce.dataset(data,
           response,
           design,
           categorical.attributes = NULL, 
           continuous.attributes = NULL,
           common = NULL,
           optout = TRUE,
           unlabeled = TRUE,
           binary = FALSE,
           detail = FALSE)
}
\arguments{
  \item{data}{
A data frame containing respondents' answers to choice experiment questions. It corresponds to an argument  \code{respondent.dataset} in \code{make.dataset}.
}
  \item{response}{
A vector of variables showing the alternative of which was selected in each choice experiment question. It corresponds to an argument \code{choice.indicators} in \code{make.dataset}.
}
  \item{design}{
A data frame containing a choice experiment design created by the function \code{Lma.design} or \code{rotation.design}. It corresponds to an argument \code{choice.experiment.design} in \code{make.design.matrix}.
}
  \item{categorical.attributes}{
A vector containing the names of attributes treated as categorical independent variables in the analysis.
}
  \item{continuous.attributes}{
A vector containing the names of attributes treated as continuous independent variables in the analysis.
}
  \item{common}{
A vector containing a fixed combination of attribute-levels corresponding to a common base option in each question. If there is no common base option, the argument is set as \code{NULL} (default).
} 
  \item{optout}{
A logical variable describing whether or not the opt-out alternative is included in the design matrix created by this function. If \code{TRUE} (default), the opt-out alternative is included; otherwise it is not.
}
  \item{unlabeled}{
A logical variable describing the types of a choice experiment design assigned by the argument \code{choice.experiment.design}. If the type is unlabeled, the argument is set as \code{TRUE} (default). If the type is labeled, it is set as \code{FALSE}.
}
  \item{binary}{
When the function is applied to the conditional logit model, the argument is set as \code{FALSE} (default). When the function is applied to the binary choice model, it is set as \code{TRUE}.
}
  \item{detail}{
A logical variable describing whether or not some variables contained in the argument \code{data} and variables created in this function are stored in a data set produced by this function.
}

}
\details{
This function provides a data set for analysis by combining a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} and respondent data set. The resultant data set is similar to one from \code{make.dataset}.

For details on arguments, see help for \code{make.dataset} and \code{make.design.matrix}.

For details on the output, see help for \code{make.dataset}.
}
\value{
This function provides a data set (data frame) for discrete choice analysis of responses to choice experiment questions. The resultant data set is similar to one from the function \code{make.dataset}. For details, see help for \code{make.dataset}.
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link{make.dataset}}, \code{\link{make.design.matrix}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{datasets}
\keyword{survival}
