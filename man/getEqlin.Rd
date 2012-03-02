\name{getEqlin}
\alias{getEqlin}
\alias{getEqlin-class}
\alias{getEqlin,keout-method}
\title{Retrieve Equated Values of the Linear Equating}
\description{
Function to retrieve the equated values of the linear equating from an object of class keout created by kequate.
}
\usage{
getEqlin(object)
}
\arguments{
  \item{object}{
An object of class keout as created by the function kequate.
}
}
\value{
A numeric vector with the equated valuesof the linear equating.
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
getEqlin(keEG)
}