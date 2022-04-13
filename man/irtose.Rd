\name{irtose}
\alias{irtose}
\title{
Item Response Theory Observed-Score Kernel Equating
}
\description{
A function to conduct an equating between two parallel tests using item response theory (IRT) observed-score kernel equating. Designs available are equivalent groups (EG) and non-equivalent groups with anchor test using chain equating (NEAT CE).
}
\usage{
irtose(design="CE", P, Q, x, y, a=0, qpoints=seq(-6, 6, by=0.1), model="2pl", catsX=0, 
catsY=0, catsA=0, see="analytical", replications=199, kernel="gaussian", 
h=list(hx=0, hy=0, hxP=0, haP=0, hyQ=0, haQ=0), 
hlin=list(hxlin=0, hylin=0, hxPlin=0, haPlin=0, hyQlin=0, haQlin=0), KPEN=0, wpen=0.5, 
linear=FALSE, slog=1, bunif=1, altopt=FALSE, wS=0.5, eqcoef="mean-mean", robust=FALSE,
distribution = list("normal", par = data.frame(mu = 0, sigma = 1)), DS = FALSE, 
CV = FALSE)
}

\arguments{
  \item{design}{
A character vector indicating which design to use. Possible designs are EG, CE and PSE.
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
Vector of possible score values of the anchor test A. (Required for all NEAT designs.)
}

  \item{qpoints}{
A numeric vector containing the quadrature points used in the equating. If not specified, the quadrature points from the IRT models will be used.
}

  \item{model}{
A character vector indicating which IRT model to use. Available models for dichotomoous data are 2pl and 3pl while available models for polytomous data are GPCM and GRM. Default is "2pl".
}

  \item{catsX}{
The number of category answers for each item on test X. (GPCM and GRM only.)
}

  \item{catsY}{
The number of category answers for each item on test Y. (GPCM and GRM only.)
}

  \item{catsA}{
The number of category answers for each item on test A. (GPCM and GRM only.)
}

  \item{see}{
A character vector indicating which standard errors of equating to use. Options are "analytical" and "bootstrap", with default "analytical". If using the double smoothing bandwidth selection method (DS = TRUE), the bootstrap is not supported.
}

  \item{replications}{
The number of bootstrap replications if using the bootstrap standard error calculations. Default is 199.
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

  \item{wS}{
The weighting of group P in a PSE equating. Default is 0.5.
}

  \item{eqcoef}{
Character vector indicating which equating coefficients to use. Options are "mean-mean", "mean-gmean", "mean-sigma", "Stocking-Lord" and "Haebara". Default is "mean-mean". (Only for PSE. With polytomous IRT models, only "mean-mean" is available.)
}

  \item{robust}{
Logical indicating whether the robust covariance matrix should be calculated for the IRT model. Warning: is slow for large models and large sample sizes. Default is FALSE. (Only for 2pl and GPCM models.)
}

  \item{distribution}{
A list containing information regarding the latent distribution. Default is the standard normal distribution. (Currently not in use.)
}

  \item{DS}{
Logical which enables bandwidth selection with the double smoothing method.
}

\item{CV}{
Logical which enables bandwidth selection with the cross-validation method.
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
Andersson, B. (2016). Asymptotic Standard Errors of Observed-Score Equating With Polytomous IRT
Models. \emph{Journal of Educational Measurement}, \bold{53}, 459–477.

Andersson, B. (2018). Asymptotic Variance of Linking Coefficient Estimators for Polytomous IRT Models. \emph{Applied Psychological Measurement}, \bold{42}, 192–205.

Andersson, B. and Wiberg, M. (2017). Item Response Theory Observed-Score Kernel Equating. \emph{Psychometrika}, \bold{83}, 48–67. 

von Davier, A.A., Holland, P.W., and Thayer, D.T. (2004). \emph{The Kernel Method of Test Equating.} Springer-Verlag New York.

Ogasawara, H. (2003). Asymptotic standard errors of IRT observed-score equating methods. \emph{Psychometrika}, \bold{68}, 193-211.
}
\author{
\email{bjoern.h.andersson@gmail.com}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

\seealso{
\code{\link{kequate}}
}
\examples{
\dontrun{
#Generate data, non-equivalent groups with chain equating design
akX <- runif(15, 0.5, 2)
bkX <- rnorm(15)
akY <- runif(15, 0.5, 2)
bkY <- rnorm(15)
akA <- runif(15, 0.5, 2)
bkA <- rnorm(15)

dataP <- matrix(0, nrow = 1000, ncol = 30)
dataQ <- matrix(0, nrow = 1000, ncol = 30)

for(i in 1:1000){
	ability <- rnorm(1)
	dataP[i,1:15] <- (1 / (1 + exp(-akX * (ability - bkX)))) > runif(15)
	dataP[i,16:30] <- (1 / (1 + exp(-akA * (ability - bkA)))) > runif(15)
}

for(i in 1:1000){
	ability <- rnorm(1, mean = 0.5)
	dataQ[i,1:15] <- (1 / (1 + exp(-akY * (ability - bkY)))) > runif(15)
	dataQ[i,16:30] <- (1 / (1 + exp(-akA * (ability - bkA)))) > runif(15)
}
#Estimate the 2-PL IRT model with package ltm
sim2plP <- ltm(dataP ~ z1, IRT.param = TRUE)
sim2plQ <- ltm(dataQ ~ z1, IRT.param = TRUE)

#Equate the two tests
sim2plan <- irtose("CE", sim2plP, sim2plQ, 0:15, 0:15, 0:15)
}
}