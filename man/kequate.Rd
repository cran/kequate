\name{kequate}
\alias{kequate}
\alias{kequateCB}
\alias{kequateEG}
\alias{kequateNEAT_CE}
\alias{kequateNEAT_PSE}
\alias{kequateSG}

\title{
Test Equating Using the Kernel Method
}
\description{
A function to conduct an equating between two parallel tests using kernel equating. Designs available are equivalent groups (EG), single group (SG), counterbalanced (CB), non-equivalent groups with anchor test using either chain equating (NEAT CE) or post-stratification equating (NEAT PSE) and non-equivalent groups using covariates (NEC).
}
\usage{
kequate(design, ...)
}

\arguments{
  \item{design}{
A character vector indicating which design to use. Possible designs are: EG, SG, CB, NEAT_CE, NEAT_PSE or NEC.
}
  \item{\dots}{
Further arguments which partly depend on the design chosen. (See section Details for further information.)
}
}
\details{
Besides the above argument, additional arguments must be provided for the different equating designs.

\subsection{EG design}{\cr
\sQuote{x}    A vector of possible score values on the test X to be equated, ordered from the lowest to the highest score. 

\sQuote{y}    A vector of possible score values on the test Y to be equated, ordered from the lowest to the highest score.  

\sQuote{r},\sQuote{s}    Numeric vectors containing the estimated or observed score probabilities for tests X and Y respectively.  Alternatively objects of class \sQuote{glm}.  

\sQuote{DMP}, \sQuote{DMQ}    The design matrices from the log-linear models for the estimated score probabilities for X and Y.  Not needed if arguments r and s are of class \sQuote{glm}.  

\sQuote{N}, \sQuote{M}    The sample sizes of the groups taking tests X and Y, respectively.  Not needed if arguments r and s are of class \sQuote{glm}.  

\sQuote{hx}, \sQuote{hy}    Optional arguments to specify the continuization parameters manually.  (If linear=TRUE, then these arguments have no effect.)  

\sQuote{hxlin}, \sQuote{hylin}    Optional arguments to specify the continuization parameters manually in the linear case.

\sQuote{KPEN}    The constant used in deciding the optimal continuization parameter.  Default is 0.

\sQuote{wpen}    An argument denoting at which point the derivatives in the second part of the penalty function should be evaluated. Default is 1/4.

\sQuote{linear}    Logical denoting if only a linear equating is to be performed.  Default is FALSE.

\sQuote{irtx}, \sQuote{irty} Optional arguments to provide matrices of probabilities to answer correctly to the questions on the parallel tests X and Y, as estimated in an Item Response Theory (IRT) model.

\sQuote{smoothed}    A logical argument denoting if the data provided are pre-smoothed or not. Default is TRUE.

\sQuote{kernel}    A character vector indicating which kernel to use, either "gaussian", "logistic", "stdgaussian" or "uniform". Default is "gaussian".

\sQuote{slog}    The parameter used in defining the logistic kernel. Default is 1.

\sQuote{bunif}    The parameter used in defining the uniform kernel. Default is 0.5.

\sQuote{altopt}    Logical which sets the bandwidth parameter equal to a variant of Silverman's rule of thumb. Default is FALSE.

\sQuote{DS}    Logical which enables bandwidth selection with the double smoothing method.
}

\subsection{SG design}{\cr
\sQuote{x}    A vector of possible score values on the test X to be equated, ordered from the lowest to the highest score. 

\sQuote{y}    A vector of possible score values on the test Y to be equated, ordered from the lowest to the highest score.  

\sQuote{P}    The estimated or observed probability matrix for scores on tests X and Y, where the columns denote scores on test Y and the rows denote scores on test X. Alternatively a vector of score probabilities or an object of class \sQuote{glm}, where the entries are ordered first by the Y-scores and then by the X-scores.

\sQuote{DM}    The design matrix used in the log-linear model. Not needed if the argument P is of class \sQuote{glm}.

\sQuote{N}    The sample size. Not needed if the argument P is of class \sQuote{glm}.

\sQuote{hx}, \sQuote{hy}    Optional arguments to specify the continuization parameter manually. (If linear=TRUE, then these arguments have no effect.)

\sQuote{hxlin}, \sQuote{hylin}    Optional arguments to specify the continuization parameters manually in the linear case.

\sQuote{KPEN}    The constant used in deciding the optimal continuization parameter. Default is 0. 

\sQuote{wpen}    An argument denoting at which point the derivatives in the second part of the penalty function should be evaluated. Default is 1/4.

\sQuote{linear}    Logical denoting if only a linear equating is to be performed. Default is FALSE.

\sQuote{irtx}, \sQuote{irty}    Optional arguments to provide matrices of probabilities to answer correctly to the questions on the parallel tests X and Y, as estimated in an Item Response Theory (IRT) model.

\sQuote{smoothed}    A logical argument denoting if the data provided are pre-smoothed or not. Default is TRUE.

\sQuote{kernel}    A character vector indicating which kernel to use, either "gaussian", "logistic", "stdgaussian" or "uniform". Default is "gaussian".

\sQuote{slog}    The parameter used in defining the logistic kernel. Default is 1.

\sQuote{bunif}    The parameter used in defining the uniform kernel. Default is 0.5.

\sQuote{DS}    Logical which enables bandwidth selection with the double smoothing method.

\sQuote{CV}    Logical which enables bandwidth selection with the cross-validation method.
}

\subsection{CB design}{\cr
\sQuote{x}    A vector of possible score values on the test X to be equated, ordered from the lowest to the highest score. 

\sQuote{y}    A vector of possible score values on the test Y to be equated, ordered from the lowest to the highest score.  

\sQuote{P12}, \sQuote{P21}    The estimated or observed probability matrices for scores on first taking test X and then taking test Y, and first taking test Y and then taking test X respectively, where the rows denote scores on tests X and the columns denote scores on test Y. Alternatively numeric vectors or objects of class \sQuote{glm}, where the entries are ordered first by the Y-scores and then by the X-scores.

\sQuote{DM12}, \sQuote{DM21}    The design matrices from the log-linear models for the estimated score probabilities for the two test groups. Not needed if arguments P12 and P21 are of class \sQuote{glm}.

\sQuote{N}, \sQuote{M}    The sample sizes for the tests X and A and the tests Y and A, respectively. Not needed if arguments P12 and P21 are of class \sQuote{glm}.

\sQuote{hx}, \sQuote{hy}    Optional arguments to specify the continuization parameters manually. (If linear=TRUE, then these arguments have no effect)

\sQuote{hxlin}, \sQuote{hylin}    Optional arguments to specify the continuization parameters manually in the linear case. (Applies both when linear=FALSE and when linear=TRUE.)

\sQuote{wcb}    The weighting of the two groups. Default is 0.5.

\sQuote{KPEN}    Optional argument to specify the constant used in deciding the optimal continuization parameter. Default is 0. 

\sQuote{wpen}    An argument denoting at which point the derivatives in the second part of the penalty function should be evaluated. Default is 1/4.

\sQuote{linear}    Optional logical argument denoting if only a linear equating is to be performed. Default is FALSE.

\sQuote{irtx}, \sQuote{irty} Optional arguments to provide matrices of probabilities to answer correctly to the questions on the parallel tests X and Y, as estimated in an Item Response Theory (IRT) model.

\sQuote{smoothed}    A logical argument denoting if the data provided are pre-smoothed or not. Default is TRUE.

\sQuote{kernel}    A character vector indicating which kernel to use, either "gaussian", "logistic", "stdgaussian" or "uniform". Default is "gaussian".

\sQuote{slog}    The parameter used in defining the logistic kernel. Default is 1.

\sQuote{bunif}    The parameter used in defining the uniform kernel. Default is 0.5.

\sQuote{altopt}    Logical which sets the bandwidth parameter equal to a variant of Silverman's rule of thumb. Default is FALSE.

\sQuote{DS}    Logical which enables bandwidth selection with the double smoothing method.

\sQuote{CV}    Logical which enables bandwidth selection with the cross-validation method.
}

\subsection{NEAT PSE or NEC design}{\cr
\sQuote{x}    A vector of possible score values on the test X to be equated, ordered from the lowest to the highest score. 

\sQuote{y}    A vector of possible score values on the test Y to be equated, ordered from the lowest to the highest score.  

\sQuote{P}, \sQuote{Q}    The estimated or observed probability matrices for scores on tests X and A and tests Y and A respectively, where the rows denote scores on tests X or Y and the columns denote scores on test A. Alternatively numeric vectors or objects of class \sQuote{glm}, where the entries are ordered first by the X-scores/Y-scores and then by the A-scores.

\sQuote{DMP}, \sQuote{DMQ}    The design matrices from the log-linear models for the estimated score probabilities for X and A and Y and A. Not needed if arguments P and Q are of class \sQuote{glm}.

\sQuote{N}, \sQuote{M}    The sample sizes for the tests X and A and the tests Y and A, respectively. Not needed if arguments P and Q are of class \sQuote{glm}.

\sQuote{w}    The weighting of the synthetic population. Default is 0.5.

\sQuote{hx}, \sQuote{hy}    Optional arguments to specify the continuization parameters manually. (If linear=TRUE, then these arguments have no effect)

\sQuote{hxlin}, \sQuote{hylin}    Optional arguments to specify the continuization parameters manually in the linear case. (Applies both when linear=FALSE and when linear=TRUE.)

\sQuote{KPEN}    Optional argument to specify the constant used in deciding the optimal continuization parameter. Default is 0. 

\sQuote{wpen}    An argument denoting at which point the derivatives in the second part of the penalty function should be evaluated. Default is 1/4.

\sQuote{linear}    Optional logical argument denoting if only a linear equating is to be performed. Default is FALSE.

\sQuote{irtx}, \sQuote{irty} Optional arguments to provide matrices of probabilities to answer correctly to the questions on the parallel tests X and Y, as estimated in an Item Response Theory (IRT) model.

\sQuote{smoothed}    A logical argument denoting if the data provided are pre-smoothed or not. Default is TRUE.

\sQuote{kernel}    A character vector indicating which kernel to use, either "gaussian", "logistic", "stdgaussian" or "uniform". Default is "gaussian".

\sQuote{slog}    The parameter used in defining the logistic kernel. Default is 1.

\sQuote{bunif}    The parameter used in defining the uniform kernel. Default is 0.5.

\sQuote{altopt}    Logical which sets the bandwidth parameter equal to a variant of Silverman's rule of thumb. Default is FALSE.

\sQuote{DS}    Logical which enables bandwidth selection with the double smoothing method.

\sQuote{CV}    Logical which enables bandwidth selection with the cross-validation method.
}

\subsection{NEAT CE design}{\cr
\sQuote{x}    A vector of possible score values on the test X to be equated, ordered from the lowest to the highest score. 

\sQuote{y}    A vector of possible score values on the test Y to be equated, ordered from the lowest to the highest score.  

\sQuote{a}    A vector containing the possible score values on the anchor test, ordered from the lowest score to the highest.

\sQuote{P}, \sQuote{Q}    The estimated or observed probability matrices for scores on tests X and A and tests Y and A respectively, where the rows denote scores on test X or Y and the columns denote scores on test A. Alternatively numeric vectors or objects of class \sQuote{glm}, where the entries are ordered first by the X-scores/Y-scores and then by the A-scores.

\sQuote{DMP}, \sQuote{DMQ}    The design matrices from the log-linear models for the estimated score probabilities for X and A and Y and A, respectively. Not needed if arguments P and Q are of class \sQuote{glm}.

\sQuote{N}, \sQuote{M}    The sample sizes for the tests X and A and the tests Y and A, respectively. Not needed if arguments P and Q are of class \sQuote{glm}.

\sQuote{hxP}, \sQuote{hyQ}, \sQuote{haP}, \sQuote{haQ}    Optional arguments to specify the continuization parameters manually. (If linear=TRUE, then these arguments have no effect.)

\sQuote{hxPlin}, \sQuote{hyQlin}, \sQuote{haPlin}, \sQuote{haQlin}    Optional arguments to specify the continuization parameters manually in the linear case. (Applies both when linear=FALSE and when linear=TRUE.)

\sQuote{KPEN}    Optional argument to specify the constant used in deciding the optimal continuization parameter. Default is 0.

\sQuote{wpen}    An argument denoting at which point the derivatives in the second part of the penalty function should be evaluated. Default is 1/4.

\sQuote{linear}    Optional logical argument denoting if only a linear equating is to be performed. Default is FALSE.

\sQuote{irtx}, \sQuote{irty} Optional arguments to provide matrices of probabilities to answer correctly to the questions on the parallel tests X and Y, as estimated in an Item Response Theory (IRT) model.

\sQuote{smoothed}    A logical argument denoting if the data provided are pre-smoothed or not. Default is TRUE.

\sQuote{kernel}    A character vector indicating which kernel to use, either "gaussian", "logistic", "stdgaussian" or "uniform". Default is "gaussian".

\sQuote{slog}    The parameter used in defining the logistic kernel. Default is 1.

\sQuote{bunif}    The parameter used in defining the uniform kernel. Default is 0.5.

\sQuote{altopt}    Logical which sets the bandwidth parameter equal to a variant of Silverman's rule of thumb. Default is FALSE.

\sQuote{DS}    Logical which enables bandwidth selection with the double smoothing method.

\sQuote{CV}    Logical which enables bandwidth selection with the cross-validation method.
}
}
\value{
Kequate returns an S4 object of class 'keout' which includes the following slots (accessed by using the get functions):
\item{Cr}{The C-matrix from the log-linear model of test X on population P. (EG design only)}
\item{Cs}{The C-matrix from the log-linear model of test Y on population Q. (EG design only)}
\item{Cp}{The C-matrix from the log-linear model of tests X and Y or X and A on population P. (SG/NEAT CE/NEAT PSE/NEC designs only)}
\item{Cq}{The C-matrix from the log-linear model of tests X and Y or X and A on population Q. (NEAT CE/NEAT PSE/NEC designs only)}
\item{SEEvect}{An object of class SEEvect consisting of matrices containing the standard error vectors for the equatings. If linear=TRUE, then only the standard error vectors for the linear case are included.}
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
}
\references{
Andersson, B., Branberg, K., and Wiberg, M. (2013). Performing the Kernel Method of Test Equating with the Package \bold{kequate}. \emph{Journal of Statistical Software}, \bold{55(6)}, 1--25. <doi:10.18637/jss.v055.i06>

von Davier, A.A., Holland, P.W., Thayer, D.T. (2004). \emph{The Kernel Method of Test Equating.} Springer-Verlag New York.
}
\author{
\email{bjorn.andersson@statistik.uu.se}\cr
\email{kenny.branberg@stat.umu.se}\cr
\email{marie.wiberg@stat.umu.se}
}

\seealso{
\code{\link{glm}},\code{\link{kefreq}}
}
\examples{
#EG toy example with different kernels
P<-c(5, 20, 35, 25, 15)
Q<-c(10, 30, 30, 20, 10)
x<-0:4
glmx<-glm(P~I(x)+I(x^2), family="poisson", x=TRUE)
glmy<-glm(Q~I(x)+I(x^2), family="poisson", x=TRUE)
keEG<-kequate("EG", 0:4, 0:4, glmx, glmy)
keEGlog<-kequate("EG", 0:4, 0:4, glmx, glmy, kernel="logistic", slog=sqrt(3)/pi)
keEGuni<-kequate("EG", 0:4, 0:4, glmx, glmy, kernel="uniform", bunif=sqrt(3))
plot(keEG)

\dontrun{
#NEAT example using simulated data
data(simeq)
freq1 <- kefreq(simeq$bivar1$X, 0:20, simeq$bivar1$A, 0:10)
freq2 <- kefreq(simeq$bivar2$Y, 0:20, simeq$bivar2$A, 0:10)
glm1<-glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(A)+I(A^2)+I(A^3)+I(A^4)+
I(A):I(X)+I(A):I(X^2)+I(A^2):I(X)+I(A^2):I(X^2), family="poisson", data=freq1, x=TRUE)
glm2<-glm(frequency~I(X)+I(X^2)+I(A)+I(A^2)+I(A^3)+I(A^4)+I(A):I(X)+I(A):I(X^2)+
I(A^2):I(X)+I(A^2):I(X^2), family="poisson", data=freq2, x=TRUE)
keNEATPSE <- kequate("NEAT_PSE", 0:20, 0:20, glm1, glm2)
keNEATCE <- kequate("NEAT_CE", 0:20, 0:20, 0:10, glm1, glm2)
summary(keNEATPSE)
summary(keNEATCE)

#IRT observed-score equating
keNEATCEirt <- kequate("NEAT_CE", 0:20, 0:20, 0:10, glm1, glm2, irtx=simeq$irtNEATx, 
irty=simeq$irtNEATy)
getEquating(keNEATCEirt)
}
}