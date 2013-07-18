\name{keout-class}
\Rdversion{1.1}
\docType{class}
\alias{keout-class}

\title{Class "keout"}
\description{
Information relating to an equating between two tests X and Y.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("keout", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Cr}:}{Object of class \code{"matrix"}. The C-matrix from the log-linear model of test X on population P. (EG design only) }
    \item{\code{Cs}:}{Object of class \code{"matrix"}. The C-matrix from the log-linear model of test Y on population Q. (EG design only)}
    \item{\code{Cp}:}{Object of class \code{"matrix"}. The C-matrix from the log-linear model of tests X and Y or X and A on population P. (CB design: C-matrix from the log-linear model for the first group) (CB/SG/NEAT CE/NEAT PSE/NEC designs only) }
    \item{\code{Cq}:}{Object of class \code{"matrix"}. The C-matrix from the log-linear model of tests X and Y or X and A on population Q. (CB design: C-matrix from the log-linear model for the second group) (CB/NEAT CE/NEAT PSE/NEC designs only) }
    \item{\code{coveqYx}:}{Object of class \code{"matrix"}. Matrix containing the covariance matrix of the equating function. (IRT-OSE only)}
    \item{\code{SEEvect}:}{Object of class \code{"SEEvect"}. Matrices containing the standard error vectors for the equatings. If linear=TRUE, then only the standard error vectors for the linear case are included.}
    \item{\code{Pest}:}{Object of class \code{"matrix"}. The estimated probability matrix over population P.}
    \item{\code{Pobs}:}{Object of class \code{"matrix"}. The observed probability matrix over population P.}
    \item{\code{Qest}:}{Object of class \code{"matrix"}. The estimated probability matrix over population Q.}
    \item{\code{Qobs}:}{Object of class \code{"matrix"}. The observed probability matrix over population Q.}
    \item{\code{scores}:}{Object of class \code{"list"}. A list of data frames containing the score value vectors for the tests X and Y (all designs except NEAT CE) or the score value vectors for the tests X, Y and A (only for NEAT CE). Also included are the estimated score probabilities and the continuized cumulative distribution functions for the respective tests.}
    \item{\code{linear}:}{Object of class \code{"logical"}. A logical vector. TRUE if linear=TRUE was specified, otherwise FALSE. }
    \item{\code{PRE}:}{Object of class \code{"data.frame"}. A data frame containing the percent relative error (PRE) in the ten first moments between the equated scores and the reference distribution. (For chain equating, the PRE is calculated for the linking from X to A and the linking from A to Y.)}
    \item{\code{h}:}{Object of class \code{"data.frame"}. A data frame containing the continuization parameters used in the equating.}
    \item{\code{kernel}:}{Object of class \code{"character"}. A character vector denoting the kernel used. }
    \item{\code{type}:}{Object of class \code{"character"}. A character vector describing the design used. }
    \item{\code{equating}:}{Object of class \code{"data.frame"}. A data frame containing the equated values from X to Y and the associated standard errors (for either an equipercentile or a linear equating), as well as the SEED between the equipercentile and linear equating functions and the equated values and the associated standard errors  in the linear case (if an equipercentile equating is conducted).}
    \item{\code{irt}:}{Object of class \code{"list"}. A list containing the objects from the package ltm containing the IRT models specified, and the asymptotic covariance matrix of the item parameters under the regular IRT parametrization. (IRT-OSE only) }
    \item{\code{see}:}{Object of class \code{"character"}. A character vector denoting which type of standard errors of equating that are used: analytical or bootstrap. }
    \item{\code{replications}:}{Object of class \code{"numeric"}. The number of bootstrap replications if using the bootstrap standard error calculations. }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "keout")}: ... }
    \item{summary}{\code{signature(object = "keout")}: ... }
	 }
}

\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{kequate}}
}
\examples{
showClass("keout")
}
\keyword{classes}
