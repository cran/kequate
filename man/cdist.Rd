\name{cdist}
\alias{cdist}
\title{Conditional Mean, Variance, Skewness and Kurtosis}
\description{
Calculates conditional means, variances, skewnesses and kurtoses for observed and estimated bivariate probability distributions of test scores.
}
\usage{
cdist(est, obs, xscores, ascores)
}
\arguments{
  \item{est}{
Matrix of estimated bivariate score probabilities.
}
  \item{obs}{
Matrix of observed bivariate score probabilities.
}
  \item{xscores}{
Optional argument to specify the score vector for test X.
}
  \item{ascores}{
Optional argument to specify the score vector for test A.
}
}
\value{
An object of class 'cdist' containing the following slots
\item{est1}{Matrix of conditional means, variances, skewnesses and kurtoses of X given A for the estimated score distribution.}
\item{est2}{Matrix of conditional means, variances, skewnesses and kurtoses of A given X for the estimated score distribution.}
\item{obs1}{Matrix of conditional means, variances, skewnesses and kurtoses of X given A for the observed score distribution.}
\item{obs2}{Matrix of conditional means, variances, skewnesses and kurtoses of A given X for the observed score distribution.}
}
\references{
Holland, P.W.,  Thayer, D. (1998). Univariate and Bivariate Loglinear Models for Discrete Test Score Distributions \emph{ETS Technical Report No} \bold{98-1}.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

\seealso{
\code{\link{kequate}}
\code{\link{PREp}}
}
\examples{
freqdata<-data.frame(X=c(1,2,2,1,2,2,2,2,3,1,2,1,4,2,1,1,3,3,3,3), 
A=(c(0,2,1,1,0,3,1,2,2,0,2,0,3,1,1,2,2,2,1,2)))
Pdata<-kefreq(freqdata$X, 0:5, freqdata$A, 0:3)
Pglm<-glm(frequency~X+I(X^2)+A+I(A^2)+X:A, data=Pdata, family="poisson", x=TRUE)
Pobs<-matrix(Pdata$freq, nrow=6)/sum(Pglm$y)
Pest<-matrix(Pglm$fitted.values, nrow=6)/sum(Pglm$y)
cdP<-cdist(Pest, Pobs, 0:5, 0:3)
plot(cdP)
}