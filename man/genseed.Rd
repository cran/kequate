\name{genseed}
\alias{genseed}
\title{
Standard Error of the Difference Between Two Equating Functions
}
\description{
Given two equating functions with associated standard error vectors, the standard error of the difference between two equating functions (SEED) is calculated.
}
\usage{
genseed(in1, in2, linear = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in1}{
S4 object of class keout containing information about the first equating function, as created by the function kequate.
}
  \item{in2}{
S4 object of class keout containing information about the second equating function, as created by the function kequate.
}
  \item{linear}{
A logical vector indicating if the two linear equatings are to be compared instead of the two equipercentile equatings. Default is FALSE.
}
}
\details{
The standard error of the difference between two equating functions (SEED) can be used to compare equating functions and decide which is most suitable to use. For example, if the difference between a linear and an equipercentile equating function is small and within the boundaries of the estimated SEED the simpler linear equating function may be preferable. Any two equipercentile or linear equating functions can be compared using genseed, given that the log-linear model is identical between the two (and that the designs are comparable). As an example, it is impossible to compare an EG equating to an SG equating in this manner as the log-linear models are not the same. It is however useful to compare a chain equating to a post-stratification equating in the NEAT design, or to compare post-stratification equatings with different weights. 
}
\value{
A data frame containing the difference between the equated values of two equating functions and the SEED of the two equatings for equating X to Y (eqYxD and SEEDYx).
}
\references{
von Davier, A.A., Holland, P.W., Thayer, D.T. (2004). \emph{The Kernel Method of Test Equating.} Springer-Verlag New York.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}
\seealso{
\code{\link{kequate}}
}
\examples{
P<-c(5, 20, 35, 25, 15)
Q<-c(10, 30, 30, 20, 10)
x<-0:4
glmx<-glm(P~I(x)+I(x^2), family="poisson", x=TRUE)
glmy<-glm(Q~I(x)+I(x^2), family="poisson", x=TRUE)
keEG<-kequate("EG", 0:4, glmx, glmy)
keEGnew<-kequate("EG", 0:4, glmx, glmy, hx=0.33, hy=0.33)
SEEDEG<-genseed(keEG, keEGnew)
plot(SEEDEG)
}
