\name{getSeed}
\alias{getSeed}
\alias{getSeed-class}
\alias{getSeed,keout-method}
\title{Retrieve the Standard Errors of the Difference Between Two Equatings}
\description{
Function to retrieve the standard error of the difference between an equipercentile equating and a linear equating from an object of class keout created by kequate.
}
\usage{
getSeed(object)
}
\arguments{
  \item{object}{
An object of class keout as created by the function kequate.
}
}
\value{
An object of class genseed containing the difference between an equipercentile and a linear equating and the standard errors of the difference between an equipercentile equating and a linear equating.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{kequate}}, \code{\link{genseed}}
}
\examples{
P<-c(5, 20, 35, 25, 15)
Q<-c(10, 30, 30, 20, 10)
x<-0:4
glmx<-glm(P~I(x)+I(x^2), family="poisson", x=TRUE)
glmy<-glm(Q~I(x)+I(x^2), family="poisson", x=TRUE)
keEG<-kequate("EG", 0:4, 0:4, glmx, glmy)
keEGseed<-getSeed(keEG)
plot(keEGseed)
}