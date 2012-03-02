\name{genseed-class}
\Rdversion{1.1}
\docType{class}
\alias{genseed-class}

\title{Class "genseed"}
\description{
Stores the difference between two equatings which use identical log-linear models, along with the standard error of the difference of the two equatings.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("genseed", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{out}:}{Object of class \code{"data.frame"} containing the difference between the equated values of two equating functions and the SEED of the two equatings for equating X to Y (eqYxD and SEEDYx).
 }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "genseed")}: ... }
	 }
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
showClass("genseed")
}
\keyword{classes}
