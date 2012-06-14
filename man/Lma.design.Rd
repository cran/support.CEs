\name{Lma.design}
\alias{Lma.design}
\alias{print.cedes}
\title{
Creating a choice experiment design using the L^MA method
}
\description{
This function creates a choice experiment design according to the \emph{L^MA} method.
}
\usage{
Lma.design(candidate.array = NULL, attribute.names, 
           nalternatives, nblocks, row.renames = TRUE, 
           seed = NULL)

\method{print}{cedes}(x, ...)
}
\arguments{
  \item{candidate.array}{
A data frame containing an array created by the user. Normally, when this function is used, this argument does not need to be set by the user.
}
  \item{attribute.names}{
A list of the names of attributes and levels.
}
  \item{nalternatives}{
An integer value describing the number of alternatives per choice set, excluding an opt-out alternative such as a "none of these."
}
  \item{nblocks}{
An integer value describing the number of blocks into which a choice experiment design is divided.
}
  \item{row.renames}{
A logical variable describing whether or not the row names of a choice experiment design created by this function are changed. When its value is \code{TRUE} (default), integer values are assigned to the row names starting from 1. When its value is \code{FALSE}, the row names are the same as those of an array created by the function \code{oa.design} (included in the \pkg{DoE.base} package) via the function \code{Lma.design}, or those of an array assigned to the argument \code{candidate.array} by the user.
}
  \item{seed}{
Seed for a random number generator.
}
  \item{x}{
An object of S3 class "ceds."
}
  \item{...}{
Arguments passed to the function \code{print}.
}
}
\details{
The \emph{L^MA} method directly creates a choice experiment design from an orthogonal main-effect array (Johnson et al. 2007). In this method, an orthogonal main-effect array with \code{M} times \code{A} columns of \code{L} level factors is used to create each choice set that contains \code{M} alternatives of \code{A} attributes with \code{L} levels. Each row of the array corresponds to the alternatives of a choice set.

This method creates a labeled type choice experiment design that can contain both generic attributes and alternative-specific attributes: the generic attribute refers to that which is included in all the alternatives; the alternative-specific attribute is that which is included in only one alternative. The reader is referred to chapters 3 and 5 of Louviere et al. (2000) for details about the types of attribute---generic or alternative-specific---and the types of choice experiment design---labeled or unlabeled.

When this function is used, the combination of attributes and attribute levels, the number of alternatives per choice set excluding an opt-out alternative, and the number of blocks are respectively assigned to the arguments.

The combination of attributes and attribute levels are assigned to the argument \code{attribute.names} in list format. For example, let's assume that the alternative has three attributes, each of which has three levels: an attribute X with the three levels of x1, x2, and x3; an attribute Y with the three levels of y1, y2, and y3; and an attribute Z with the three levels of 10, 20, and 30. In this case, the argument \code{attribute.names} is set to be \code{list(X = c("x1", "x2", "x3"), Y = c("y1", "y2", "y3"), Z = c("10", "20", "30"))}.

The number of alternatives per choice set is defined by the argument \code{nalternatives}: the number of alternatives does not include an opt-out alternative, such as the option "none of these."

When a large choice experiment design is created (that is, there are numerous choice experiment questions), the respondent may carry a heavy psychological burden in terms of answering the questions: in these cases, the choice experiment design is frequently divided into two or more blocks (subsets) of choice sets (questions), and each respondent is asked to answer one block of questions. The argument \code{nblocks} assigns the number of blocks. For example, when the argument \code{nblocks} is set to be \code{3} and the choice experiment design contains 27 individual choice sets (that is, there are 27 choice experiment questions), the choice experiment design is divided into 3 blocks, each of which has 9 individual choice sets (9 choice experiment questions). "Blocking" is performed on the basis of a factor with \code{nblocks} levels.

Under default settings, this function uses an orthogonal main-effect array that is automatically produced by the function \code{oa.design} in the package \pkg{DoE.base} based on the argument \code{attribute.names} to create a choice experiment design. However, when there is no array corresponding to the argument \code{attribute.names}, the function \code{oa.design} returns a full factorial based on the argument \code{attribute.names} (See help for the function \code{oa.design} in the packge \pkg{DoE.base}). On the other hand, when this function does not create a choice experiment design matching the user's requirements, the user might achieve it by assigning an arbitrary (user-defined) array to the argument \code{candidate.array}: this function uses the array to create a choice experiment design. When the user-defined array is used, the last column of the array must contain a column for dividing the design based on the argument \code{nblocks}. The arguments \code{attribute.names} and \code{nblocks} must also be assigned according to the array.
}
\value{
This function returns an object of S3 class "cedes" that is a list with the following components.
  \item{alternatives }{A list of objects, \code{alt.j}: the \emph{j}th alternative in each choice set created by this function. Each of \code{alt.j} includes a variable \code{BLOCK} describing the serial number of blocks, a variable \code{QES} describing the serial number of choice experiment questions for each value of the variable \code{BLOCK}, a variable \code{ALT} describing the serial number of alternatives for each value of the \code{QES} variable, and attribute variables corresponding to the argument \code{attribute.names}.}
  \item{candidate }{A candidate array used for creating a choice experiment design, which is generated using the function \code{oa.design} in the package \pkg{DoE.base} or which the user sets for the argument \code{candidate.array}. When \code{nblocks} >= 2, the last column in this value (\code{candidate}) shows a factor that is used for blocking.}
  \item{design.information }{Information related to the CE design created by this function, which is used as arguments in post-processing functions, such as the functions \code{questionnaire} and \code{make.design.matrix}. This list includes objects such as the number of blocks into which the CE design is divided (\code{nblocks}), the number of questions per block (\code{nquestions}), the number of alternatives per choice set excluding an opt-out alternative (\code{nalternatives}), and the number of attributes per alternative (\code{nalternatives}).}
Messages are frequently shown immediately after executing this function when it works properly. These messages are taken from the function \code{oa.design} and may be valuable to a user who wishes to define the original array and assign it the argument \code{candidate.array}.
}
\references{
Johnson, F. R., Kanninen, B., Bingham, M. and \"{O}zdemir, S. (2007) Experimental Design for Stated Choice Studies. In B. J. Kanninen (ed), \emph{Valuing Environmental Amenities Using Stated Choice Studies: A Common Sense Approach to Theory and Practice}. pp.159--202. Springer.

Louviere, J. J., Hensher, D. A. and Swait, J. D. (2000) \emph{Stated Choice Methods: Analysis and Application}. Cambridge University Press.
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link{rotation.design}}, \code{\link{syn.res2}}, \code{\link[DoE.base]{oa.design}}
}
\examples{
# See "Details" for the data set syn.res2.
des2 <- Lma.design(attribute.names = list(
 Eco = c("Conv.", "More", "Most"),
 Price = c("1", "1.1", "1.2")),
 nalternatives = 3, nblocks = 2, 
 row.renames = FALSE, seed = 987)
des2
}
\keyword{design}
