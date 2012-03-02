\name{cdist-class}
\Rdversion{1.1}
\docType{class}
\alias{cdist-class}

\title{Class "cdist"}
\description{
Stores conditional means, variances, skewnesses and kurtoses of observed and estimated bivariate distributions of test scores.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cdist", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
	\item{est1}{Matrix of conditional means, variances, skewnesses and kurtoses of X given A for the estimated score distribution.}
	\item{est2}{Matrix of conditional means, variances, skewnesses and kurtoses of A given X for the estimated score distribution.}
	\item{obs1}{Matrix of conditional means, variances, skewnesses and kurtoses of X given A for the observed score distribution.}
	\item{obs2}{Matrix of conditional means, variances, skewnesses and kurtoses of A given X for the observed score distribution.}
}
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "keout")}: ... }
}
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}


\seealso{
\code{\link{cdist}}
}
\examples{
showClass("cdist")
}
\keyword{classes}
