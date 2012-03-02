\name{getPre}
\alias{getPre}
\alias{getPre-class}
\alias{getPre,keout-method}
\title{Retrieve the Percent Relative Error}
\description{
Function to retrieve the percent relative error (PRE) used in an equating from an object of class keout created by kequate.
}
\usage{
getPre(object)
}
\arguments{
  \item{object}{
An object of class keout as created by the function kequate.
}
}
\value{
A data frame containing the PRE of the equating.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

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
getPre(keEG)
}