\name{irtose}
\alias{irtose}
\title{
Item Response Theory Observed-Score Kernel Equating
}
\description{
A function to conduct an equating between two parallel tests using item response theory (IRT) observed-score kernel equating. Designs available are equivalent groups (EG) and non-equivalent groups with anchor test using chain equating (NEAT CE).
}
\usage{
irtose(design="CE", P, Q, x, y, a=0, qpoints, model="2pl", see="analytical", 
replications=50, kernel="gaussian", h=list(hx=0, hy=0, hxP=0, haP=0, hyQ=0, haQ=0), 
hlin=list(hxlin=0, hylin=0, hxPlin=0, haPlin=0, hyQlin=0, haQlin=0), KPEN=0, 
wpen=0.5, linear=FALSE, slog=1, bunif=1, altopt=FALSE)
}

\arguments{
  \item{design}{
A character vector indicating which design to use. Possible designs are EG and CE.
}

  \item{P}{
Object of class "matrix" or an object created by the R package ltm containing either the responses for each question in  group P or the estimated IRT model in group P.
}

  \item{Q}{
Object of class "matrix" or an object created by the R package ltm containing either the responses for each question in  group Q or the estimated IRT model in group Q.
}

  \item{x}{
Vector of possible score values of the main test X to be equated.
}

  \item{y}{
Vector of possible score values of the main test Y to be equated.
}

  \item{a}{
Vector of possible score values of the anchor test A.
}

  \item{qpoints}{
A numeric vector containing the quadrature points used in the equating. If not specified, the quadrature points from the IRT models will be used.
}

  \item{model}{
A character vector indicating which IRT model to use. Available models are 2PL and 3PL. Default is "2PL".
}

  \item{see}{
A character vector indicating which standard errors of equating to use. Options are "analytical" and "bootstrap", with default "analytical".
}

  \item{replications}{
The number of bootstrap replications if using the bootstrap standard error calculations. Default is 50.
}

  \item{kernel}{
A character vector indicating which kernel to use, either "gaussian", "logistic", "stdgaussian" or "uniform". Default is "gaussian".
}

  \item{h}{
Optional argument to specify the continuization parameters manually as a list with suitable bandwidth parameters. In an EG design or a NEAT PSE design: hx and hy, in a NEAT CE design: hxP, haP, hyQ and haQ. (If linear=TRUE, then these arguments have no effect.) 
}

  \item{hlin}{
Optional argument to specify the linear continuization parameters manually as a list with suitable bandwidth parameters. In an EG design or a NEAT PSE design: hxlin and hylin, in a NEAT CE design: hxPlin, haPlin, hyQlin and haQlin.
}

  \item{slog}{
The parameter used in defining the logistic kernel. Default is 1.
}

  \item{bunif}{
The parameter used in defining the uniform kernel. Default is 0.5.
}

  \item{KPEN}{
The constant used in deciding the optimal continuization parameter. Default is 0. 
}

  \item{wpen}{
 An argument denoting at which point the derivatives in the second part of the penalty function should be evaluated. Default is 1/4.
}

  \item{linear}{
Logical denoting if only a linear equating is to be performed. Default is FALSE. 
}

  \item{altopt}{
Logical which sets the bandwidth parameter equal to a variant of Silverman's rule of thumb. Default is FALSE. 
}

}
\value{
An S4 object of class 'keout' which includes the following slots (accessed by using the get functions):
\item{coveqYx}{The asymptotic covariance matrix of the equating function.}
\item{Pest}{The estimated probability matrix over population P.}
\item{Pobs}{The observed probability matrix over population P.}
\item{Qest}{The estimated probability matrix over population Q.}
\item{Qobs}{The observed probability matrix over population Q.}
\item{scores}{A list containing the score vectors for the tests to be equated and, in a NEAT CE design, the score vector of the anchor test. Also included are the estimated score probabilities and the continuized cumulative distribution functions for the respective tests.}
\item{linear}{A logical vector. TRUE if linear=TRUE was specified, otherwise FALSE.}
\item{PRE}{A data frame containing the percent relative error in the ten first moments between the equated scores and the reference distribution. (For chain equating, the PRE is calculated for the linking from X to A and the linking from A to Y.)}
\item{h}{A data frame containing the continuization parameters used in the equating.}
\item{kernel}{A character vector denoting the kernel used.}
\item{type}{A character vector describing the design used.}
\item{equating}{A data frame containing the equated values from X to Y and the associated standard errors (for either an equipercentile or a linear equating), as well as the SEED between the equipercentile and linear equating functions and the equated values and the associated standard errors in the linear case (if an equipercentile equating is conducted).}

\item{see}{A character vector denoting which standard errors of equating are used.}
\item{replications}{A numeric vector denoting the number of replications used when calculating the bootstrap standard errors of equating.}
\item{irt}{A list containing the objects from the package ltm and the adjusted asymptotic covariance matrices of the item parameters.}
}
\references{
von Davier, A.A., Holland, P.W., Thayer, D.T. (2004). \emph{The Kernel Method of Test Equating.} Springer-Verlag New York.
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
#Generate data, non-equivalent groups with chain equating design
akX <- runif(15, 0.5, 2)
bkX <- rnorm(15)
akY <- runif(15, 0.5, 2)
bkY <- rnorm(15)
akA <- runif(15, 0.5, 2)
bkA <- rnorm(15)

dataP <- matrix(0, nrow=1000, ncol=30)
dataQ <- matrix(0, nrow=1000, ncol=30)

for(i in 1:1000){
	ability <- rnorm(1)
	dataP[i,1:15] <- (1/(1+exp(-akX*(ability-bkX)) )) > runif(15)
	dataP[i,16:30] <- (1/(1+exp(-akA*(ability-bkA)) )) > runif(15)
}

for(i in 1:1000){
	ability <- rnorm(1, mean=0.5)
	dataQ[i,1:15] <- (1/(1+exp(-akY*(ability -bkY)) )) > runif(15)
	dataQ[i,16:30] <- (1/(1+exp(-akA*(ability -bkA)) )) > runif(15)
}
#Estimate the 2-PL IRT model with package ltm
sim2plP <- ltm(dataP ~ z1, IRT.param=TRUE)
sim2plQ <- ltm(dataQ ~ z1, IRT.param=TRUE)

#Equate the two tests
sim2plan <- irtose("CE", sim2plP, sim2plQ, 0:15, 0:15, 0:15)
}