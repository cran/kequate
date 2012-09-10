\name{kefreq}
\alias{kefreq}
\title{
Test Score Frequency Tabulation
}
\description{
Tabulates the frequencies of score values/combinations of score values from data at the individual 
level. Can handle univariate or bivariate scores.
}
\usage{
kefreq(in1, xscores, in2, ascores)
}
\arguments{
  \item{in1}{
A numeric vector with the total test scores on test X for each individual.
}
  \item{xscores}{
A numeric vector containing the possible score values on test X.
}
  \item{in2}{
A numeric vector with the total test scores on test Y or test A for each individual.
}
  \item{ascores}{
A numeric vector containing the possible score values on test Y or A.
}
}
\details{
Test data often consists of data at the individual level, i.e. there is a data frame, matrix or vector containing the total test score for each test taker along with other possible information about each  test taker such as the total anchor test score. In order to use such data in equating, the data needs to be converted into frequencies for each score value or combination of score values. The function kefreq handles both univariate and bivariate data, resulting in a data frame that is ordered correctly for usage with the kequate function to equate two tests. In the case of univariate data, only the first two arguments should be specified. In the case of equating using an SG design, only the first score value vector should be provided in case the score values are the same for the two tests. All arguments should be used for a NEAT design.
}
\value{
A data frame containing the score values and the frequencies associated with each score value/score value combination, sorted in the manner appropriate for usage with functions glm and kequate.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

\seealso{
\code{\link{table}}, \code{\link{kequate}}
}
\examples{
freqdata<-data.frame(X=c(1,2,2,1,2,2,2,2,3,1,2,1,4,2,1,1,3,3,3,3), 
A=(c(0,2,1,1,0,3,1,2,2,0,2,0,3,1,1,2,2,2,1,2)))
Pdata<-kefreq(freqdata$X, 0:5, freqdata$A, 0:3)
}