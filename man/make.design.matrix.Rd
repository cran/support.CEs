\name{make.design.matrix}
\alias{make.design.matrix}
\title{
Converting a choice experiment design into a design matrix
}
\description{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design}, into a design matrix suitable for conditional logit model analysis with the function \code{clogit} in the package \pkg{survival}.
}
\usage{
make.design.matrix(choice.experiment.design,
                   optout = TRUE,
                   categorical.attributes = NULL, 
                   continuous.attributes = NULL,
                   unlabeled = TRUE)
}
\arguments{
  \item{choice.experiment.design}{
A data frame containing a choice experiment design created by the function \code{Lma.design} or \code{rotation.design}.
}
  \item{optout}{
A logical variable describing whether or not the opt-out alternative is included in the design matrix created by this function. If \code{TRUE} (default), the opt-out alternative is included; otherwise it is not.
}
  \item{categorical.attributes}{
A vector containing the names of attributes treated as categorical independent variables in the conditional logit model analysis.
}
  \item{continuous.attributes}{
A vector containing the names of attributes treated as continuous independent variables in the conditional logit model analysis.
}
  \item{unlabeled}{
A logical variable describing the types of a choice experiment design assigned by the argument \code{choice.experiment.design}. If the type is unlabeled, the argument is set as \code{TRUE} (default). If the type is labeled, it is set as \code{FALSE}.
}
}
\details{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into a design matrix that is suitable for conditional logit model analysis with the function \code{clogit} in the package \pkg{survival}.

A choice experiment design created by the function \code{Lma.design} or \code{rotation.design} is assigned to the argument \code{choice.experiment.design}.

Attributes included in the choice experiment design assigned to the argument \code{choice.experiment.design} are classified into categorical and continuous attributes that are assigned to the arguments \code{categorical.attributes} and \code{continuous.attributes}, respectively. For example, an alternative may have three attributes such as X, Y, and Z. In the conditional logit model analysis, when attributes X and Y are treated as categorical variables and attribute Z is treated as continuous variable, the argument \code{categorical.attributes} is set as \code{c("X", "Y")}, and the argument \code{continuous.attributes} is set as \code{c("Z")}. 

The categorical variables are created in dummy-variable format. In other words, the minimum value in a categorical attribute is normalized; as a result, each dummy variable is assigned a value of 1 when the categorical attribute takes on a value other than the minimum value. The dummy variables are referred to by their levels. For example, in a categorical attribute X with the three levels of "x1," "x2," and "x3," the dummy variables are created as follows: (1) When the choice experiment design is unlabeled, two dummy variables are created: a dummy variable x2 that assumes a value of 1 when attribute X takes "x2," and 0 otherwise; and a dummy variable x3 that assumes a value of 1 when attribute X takes "x3," and 0 otherwise. (2) When the choice experiment design is labeled and the design contains two alternatives ("alternative 1" and "alternative 2"), excluding an opt-out alternative, four dummy variables are created: a dummy variable x21 that assumes a value of 1 when attribute X in alternative 1 takes "x2," and 0 otherwise; a dummy variable x22 that assumes a value of 1 when attribute X in alternative 2 takes "x2," and 0 otherwise; a dummy variable x31 that assumes a value of 1 when attribute X in alternative 1 takes "x3," and 0 otherwise; and a dummy variable x32 that assumes a value of 1 when attribute X in alternative 2 takes "x3," and 0 otherwise.

Two points should be noted with regard to continuous and categorical variables in the function \code{make.design.matrix}. First, the level of the argument \code{continuous.attributes} must take on numerical values: that is, the level must not contain a unit of the attribute, such as "USD," "kg," or "km." For example, when the argument \code{continuous.attributes} is set as \code{c("Z")} and it shows the price attribute of a product alternative, the variable Z must not contain the levels \code{USD10}, \code{USD20}, and \code{USD30} but must have the levels of \code{10}, \code{20}, and \code{30}, respectively. Second, categorical variables created by the function are not in factor format. R usually treats categorical variables as factors. However, values of attribute variables in each row corresponding to an opt-out option must be set as zero (\code{0}) because the systematic component of the utility for the opt-out option is normalized to zero. Therefore, the function \code{make.design.matrix} converts categorical attributes into dummy variables (the same treatment is applied to the function \code{make.dataset}).

The argument \code{unlabeled} is set as \code{TRUE} when the choice experiment design assigned to the argument \code{choice.experiment.design} is unlabeled; it is \code{FALSE} otherwise (when the choice experiment design is labeled).

The argument \code{optout} is set as \code{TRUE} when an opt-out alternative such as the option "none of these" is included in the choice experiment questions. It is set as \code{FALSE} when the opt-out alternative is not included.

}
\value{
This function provides a design matrix that can be directly assigned to an argument \code{design.matrix} in the function \code{make.dataset}. The design matrix contains categorical and/or continuous variables created by this function as well as the following four kinds of variables.
  \item{BLOCK}{An integer variable describing the serial number of blocks.}
  \item{QES}{An integer variable describing the serial number of questions according to the value of the variable \code{BLOCK}.}
  \item{ALT}{An integer variable describing the serial number of alternatives according to the value of the variable \code{QES}.}
  \item{ASC}{Alternative specific constant(s). When the choice experiment design is labeled, the serial number of alternatives (\code{ALT}) is automatically appended to the tail of \code{ASC} (such as \code{ASC1}, \code{ASC2}, and \code{ASC3})}
}
\references{
Aizaki, H. and Nishimura, K. (2008) Design and Analysis of Choice Experiments Using R: A Brief Introduction. \emph{Agricultural Information Research}, \bold{17}(2), 86--94. \url{http://dx.doi.org/10.3173/air.17.86}
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link{Lma.design}}, \code{\link{rotation.design}}, \code{\link{make.dataset}}, \code{\link{syn.res1}}, \code{\link{syn.res2}}, \code{\link[survival]{clogit}}
}
\examples{
# A case of the function rotaion.design
# See "Details" for the data set syn.res1.
des1 <- rotation.design(attribute.names = list(
 Region = c("Reg_A", "Reg_B", "Reg_C"), 
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 2, nblocks = 1, row.renames = FALSE, randomize = TRUE, 
 seed = 987)
make.design.matrix(choice.experiment.design = des1, 
 optout = TRUE, 
 categorical.attributes = c("Region", "Eco"), 
 continuous.attributes = c("Price"), unlabeled = TRUE)

# A case of the function Lma.design
# See "Details" for the data set syn.res2.
des2 <- Lma.design(attribute.names = list(
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 3, nblocks = 2, row.renames = FALSE, seed = 987)
make.design.matrix(choice.experiment.design = des2, 
 optout = TRUE, 
 categorical.attributes = c("Eco"), 
 continuous.attributes = c("Price"), 
 unlabeled = FALSE)
}
\keyword{design}
\keyword{survival}
