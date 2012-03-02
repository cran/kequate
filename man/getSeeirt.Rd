\name{getSeeirt}
\alias{getSeeirt}
\alias{getSeeirt-class}
\alias{getSeeirt,keout-method}
\title{Retrieve Equated Values From an IRT-Equating}
\description{
Function to retrieve the standard errors of the IRT-equating from an object of class keout created by kequate.
}
\usage{
getSeeirt(object)
}
\arguments{
  \item{object}{
An object of class keout as created by the function kequate.
}
}
\value{
A vector containing the standard errors of the IRT-equating.
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
irtmatX<-matrix(0, 4, 3)
irtmatX[,1]<-c(0.1, 0.3, 0.2, 0.5)
irtmatX[,2]<-c(0.2, 0.5, 0.4, 0.7)
irtmatX[,3]<-c(0.3, 0.7, 0.6, 0.9)
irtmatY<-matrix(0, 4, 3)
irtmatY[,1]<-c(0.15, 0.2, 0.1, 0.4)
irtmatY[,2]<-c(0.2, 0.4, 0.3, 0.6)
irtmatY[,3]<-c(0.25, 0.6, 0.5, 0.8)
irtX<-c(12, 32, 54, 22, 10)
irtY<-c(24, 36, 48, 12, 10)
xirt=0:4
glmXirt<-glm(irtX~xirt+I(xirt^2), family="poisson", x=TRUE)
glmYirt<-glm(irtY~xirt+I(xirt^2), family="poisson", x=TRUE)
keEGirt<-kequate("EG", xirt, glmXirt, glmYirt, irtx=irtmatX, irty=irtmatY)
getSeeirt(keEGirt)
}