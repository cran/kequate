\name{getEquating}
\alias{getEquating}
\alias{getEquating-class}
\alias{getEquating,keout-method}
\title{Retrieve Information From an Equating}
\description{
Function to retrieve information from an object of class keout created by kequate.
}
\usage{
getEquating(object)
}
\arguments{
  \item{object}{
An object of class keout as created by the function kequate.
}
}
\value{
A data frame with the equated values and standard errors of equating.
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
keEG<-kequate("EG", 0:4, 0:4, glmx, glmy)
getEquating(keEG)
}
