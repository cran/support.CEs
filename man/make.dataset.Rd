\name{make.dataset}
\alias{make.dataset}
\title{
Making a data set suitable for the conditional logit model
}
\description{
This function makes a data set used for the function \code{clogit} in the package \pkg{survival}.
}
\usage{
make.dataset(respondent.dataset, design.matrix, 
             choice.indicators, detail = FALSE)
}
\arguments{
  \item{respondent.dataset}{
A name of a data frame containing respondents' answers to choice experiment questions.
}
  \item{design.matrix}{
A name of a data frame containing a design matrix created by the function \code{make.design.matrix} in this package.
}
  \item{choice.indicators}{
A vector of the names of variables showing the alternative of which was selected in each choice experiment question.
}
  \item{detail}{
A logical variable describing whether or not some variables contained in the argument \code{respondent.dataset} and variables created in this function are stored in a data set produced by this function.
}
}
\details{
Conditional logit model analyses of responses to choice experiment questions in R can be conducted using the function \code{clogit} in the package \pkg{survival}. When the function is used to analyze the responses to the choice experiment questions, a data set in a special format is needed; each alternative should comprise one row of the data set (see the example in Figure 4 in Aizaki and Nishimura 2008). The function \code{make.dataset} is able to create such a data set by combining a data set containing information about responses to the choice experiment questions and a data set containing a design matrix related to these questions.

The respondent data set has to be created by the user and is assigned to the argument \code{respondent.dataset}. The data set, in which each row shows one respondent, has to contain a variable \code{ID}, corresponding to the respondent's identification number; a variable \code{BLOCK}, corresponding to the serial number of blocks to which each respondent had been assigned; and response variables, corresponding to the answers to each of the choice experiment questions. If necessary, covariates showing the respondent's individual characteristics such as age and gender are also included in the \code{respondent.dataset}. Although the names of the response variables and covariates are discretionary, the names of the respondent's identification number variable and block variable must be \code{ID} and \code{BLOCK}, respectively. 

The names of the response variables are assigned to the argument \code{choice.indicators}. For example, when the names of the response variables in the respondent data set are q1, q2, q3, and q4, the argument is set as \code{c("q1", "q2", "q3", "q4")}. The response variables show the serial number of the alternative selected by the respondent for each choice experiment question. The method of assigning the serial number of the alternatives must be the same as that of assigning the \code{ALT} in the output from the \code{make.design.matrix}. In other words, each alternative must be assigned an integer value that starts from 1. 

In the \code{respondent.dataset}, all variables are automatically treated as covariates, except for the variable \code{ID}, the variable \code{BLOCK}, and the response variables assigned by the argument \code{choice.indicators}. 

The structure of the design matrix data set (\code{design.matrix}), in which each row shows one alternative, is the same as that of the output from the function \code{make.design.matrix} (see "Details" for the \code{make.design.matrix}). The design matrix data set must include the variables \code{BLOCK}, \code{QES}, and \code{ALT}, whose definitions are the same as those in the \code{make.design.matrix}. The names of other variables (such as categorical and/or continuous independent variables) in the design matrix data set are discretionary.
}
\value{
The data set that is suitable for the function \code{clogit} is returned. In addition to some variables contained in the respondent and design matrix data sets, this data set also contains the following two variables that are used in the function \code{clogit}:
  \item{STR}{A variable assigned to the argument \code{strata} in the function \code{clogit} in order to identify each combination of respondent and question.}
  \item{RES}{A variable corresponding to a logical variable that takes on \code{TRUE} when the alternative is selected and \code{FALSE} when it is not. }
}
\references{
Aizaki, H. and Nishimura, K. (2008) Design and Analysis of Choice Experiments Using R: A Brief Introduction. \emph{Agricultural Information Research}, \bold{17}(2), 86--94. \url{http://www.jstage.jst.go.jp/article/air/17/2/17_86/_article}
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.design.matrix}}, \code{\link{syn.res1}}, \code{\link{syn.res2}}, \code{\link[survival]{clogit}}
}
\examples{
# A case of the function rotaion.design
# See "Details" for the data set syn.res1.
des1 <- rotation.design(attribute.names = list(
 Region = c("Reg_A", "Reg_B", "Reg_C"), 
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 2, nblocks = 1, row.renames = FALSE, 
 randomize = TRUE, seed = 987)
desmat1 <- make.design.matrix(choice.experiment.design = des1, 
 nblocks = 1, nquestions = 9, nalternatives = 2, optout = TRUE, 
 categorical.attributes = c("Region", "Eco"), 
 continuous.attributes = c("Price"), unlabeled = TRUE)
data(syn.res1)
dataset1 <- make.dataset(respondent.dataset = syn.res1, 
 choice.indicators = 
  c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"), 
 design.matrix = desmat1)
clogout1 <- clogit(RES ~ ASC + Reg_B + Reg_C + More + Most + 
 More:F + Most:F + Price + strata(STR), data = dataset1)
clogout1
gofm(clogout1)
mwtp(output = clogout1, monetary.variables = c("Price"), 
 nonmonetary.variables = 
  c("Reg_B", "Reg_C", "More", "Most", "More:F", "Most:F"), 
 percentile.points = c(5, 95), seed = 987)

# A case of the function Lma.design
# See "Details" for the data set syn.res2.
des2 <- Lma.design(attribute.names = list(
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 3, nblocks = 2, row.renames = FALSE, seed = 987)
desmat2 <- make.design.matrix(choice.experiment.design = des2, 
 nblocks = 2, nquestions = 9, nalternatives = 3, optout = TRUE, 
 categorical.attributes = c("Eco"), 
 continuous.attributes = c("Price"), 
 unlabeled = FALSE)
data(syn.res2)
dataset2 <- make.dataset(respondent.dataset = syn.res2, 
 choice.indicators = 
  c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"), 
 design.matrix = desmat2)
clogout2 <- clogit(RES ~ ASC1 + More1 + Most1 + Price1 + 
 ASC2 + More2 + Most2 + Price2 + ASC3 + More3 + Most3 + Price3 + 
 strata(STR), data = dataset2)
clogout2
gofm(clogout2)
mwtp(output = clogout2, 
 monetary.variables = c("Price1", "Price2", "Price3"), 
 nonmonetary.variables = list(
 c("More1", "Most1"), c("More2", "Most2"), c("More3", "Most3")), 
 percentile.points = c(5, 95), seed = 987)
}
\keyword{datasets}
\keyword{survival}
