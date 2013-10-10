\name{simeq}
\docType{data}
\alias{simeq}
\title{Simulated Test Data}
\description{Contains bivariate test score data (bivar1 and bivar2), IRT data (irtNEATx and irtNEATy), data from an equivalent groups (EG) design (FXEGglm and FYEGglm) and data from a non-equivalent groups with covariates (NEC) design (testdata1 and testdata2), for usage in equating.}
\usage{data(simeq)}
\format{A list containing the data frames bivar1 and bivar2 with 1000 observations each, two 5x20 matrices irtNEATx and irtNEATy, two glm objects FXEGglm and FYEGglm and two data frames testdata1 and testdata2.}
\source{The EG data is taken from the log-linear models specified in von Davier (2004). The remaining data was generated in R.}
\references{
Branberg, K., Wiberg, M. (2011). Observed Score Linear Equating with Covariates.  \emph{Journal of Educational Measurement}, \bold{44(4)}, 419--440. URL \url{http://dx.doi.org/10.1111/j.1745-3984.2011.00153.x}

von Davier, A.A., Holland, P.W., Thayer, D.T. (2004). \emph{The Kernel Method of Test Equating.} Springer-Verlag New York.
}
\keyword{datasets}