\name{FTres}
\alias{FTres}
\title{
Freeman-Tukey Residuals
}
\description{
Calculates the Freeman-Tukey residuals for log-linear models of frequency data. If the frequencies 
are assumed to be Poisson distributed, then the Freeman-Tukey residuals are approximately normal 
distributed.
}
\usage{
FTres(obs, fit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{
A numeric vector containing the observed frequencies.
}
  \item{fit}{
A numeric vector containing the estimated frequencies.
}
}
\details{
For an observed frequency \eqn{n_{i}}{ni} and the estimated frequency \eqn{m_{i}}{mi}, the Freeman-Tukey residual \eqn{FT_{i}}{FTi} is defined as\cr
\eqn{FT_{i} = \sqrt{n_{i}}+\sqrt{n_{i}+1}-\sqrt{4m_{i}+1}.}{FTi = \sqrt(ni)+\sqrt(ni+1)-\sqrt(4mi+1).}
}
\value{
A numeric vector containing the Freeman-Tukey residuals.
}
\references{
Andersson, B., Branberg, K., Wiberg, M. (2013). Performing the Kernel Method of Test Equating with the Package \bold{kequate}. \emph{Journal of Statistical Software}, \bold{55(6)}, 1--25. URL \url{http://www.jstatsoft.org/v55/i06/}

Holland, P.W,  Thayer, D. (1998). Univariate and Bivariate Loglinear Models for Discrete Test Score Distributions \emph{ETS Technical Report No} \bold{98-1}.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

\seealso{
\code{\link{glm}}
}
\examples{
#Example data:
P<-c(5, 20, 35, 25, 15)
x<-0:4
glmx<-glm(P~I(x)+I(x^2), family="poisson", x=TRUE)
res<-FTres(glmx$y, glmx$fitted.values)
}
