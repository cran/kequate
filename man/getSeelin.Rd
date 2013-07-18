\name{getSeelin}
\alias{getSeelin}
\alias{getSeelin-class}
\alias{getSeelin,keout-method}
\title{Retrieve Standard Errors From a Linear Equating}
\description{
Function to retrieve the standard errors of a linear equating from an object of class keout created by kequate.
}
\usage{
getSeelin(object)
}
\arguments{
  \item{object}{
An object of class keout as created by the function kequate.
}
}
\value{
A vector containing the standard errors of the linear equating.
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
getSeelin(keEG)
}