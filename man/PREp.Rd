\name{PREp}
\alias{PREp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percent Relative Error
}
\description{
Calculates the percent relative error (PRE) between an equated distribution and the reference distribution for the first ten moments.
}
\usage{
PREp(eq, obs, r, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eq}{
A numeric vector containing the equated values from X to Y or Y to X.
}
  \item{obs}{
The score vector of test Y or X.
}
  \item{r}{
A vector of probabilities corresponding to the equated values.
}
  \item{s}{
A vector of probabilities corresponding to the score values of test Y or X.
}
}
\details{
If we equate test X to test Y, then we have equated values eYx with estimated probabilities r and estimated probabilities s for the score values on Y. To compare the moments between these two distributions, we can calculate the percent relative error (PRE) between them. If we denote the p:th moment of Y and eYx by \eqn{\mu(Y)}{\mu(Y)} and \eqn{\mu(eYx)}{\mu(eYx)} respectively, the PRE for moment p is defined as

\deqn{PRE(p) = \frac{100 \mu(eYx)-\mu(Y)}{\mu(Y)}.}{%
      PRE(p) = 100*(\mu(eYx)-\mu(Y))/\mu(Y).}
    
}
\value{
A numeric  vector containing the percentage relative error for the first ten moments.
}
\references{
von Davier, A.A., Holland, P.W., Thayer, D.T. (2004). \emph{The Kernel Method of Test Equating.} Springer-Verlag New York.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{glm}}
\code{\link{kequate}}
}
\examples{
P<-c(5, 20, 35, 25, 15)
Q<-c(10, 30, 30, 20, 10)
x<-0:4
glmx<-glm(P~I(x)+I(x^2), family="poisson", x=TRUE)
glmy<-glm(Q~I(x)+I(x^2), family="poisson", x=TRUE)
keEG<-kequate("EG", 0:4, glmx, glmy)
PREp(getEq(keEG), 0:4, glmx$fitted.values/100, glmy$fitted.values/100)
}