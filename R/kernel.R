setClass("cdist", representation(est1="matrix", est2="matrix", obs1="matrix", obs2="matrix"))
setClass("SEEvect", representation(SEEYx="matrix", SEEXy="matrix", SEEYxLIN="matrix", SEEXyLIN="matrix"))
setClass("SEEDout", representation(SEED="data.frame", SEEvect="SEEvect", hlin="data.frame"))
setClass("kedist", representation(cdfx="numeric", cdfy="numeric", r="numeric", s="numeric"))
setClass("keout", representation(Cr="matrix", Cs="matrix", Cp="matrix", Cq="matrix", coveqYx="matrix", pdereqYx="matrix", SEEvect="SEEvect", Pest="matrix", Pobs="matrix", Qest="matrix", Qobs="matrix", scores="list", linear="logical", PRE="data.frame", h="data.frame", kernel="character", type="character", equating="data.frame", irt="list", see="character", replications="numeric"))
setClass("genseed", representation(out="data.frame"))

setMethod("plot", signature(x="keout"), function(x){
      if(length(x@equating)<2)
        plot(x@scores$X$x, x@equating$eqYx, pch=16, ylim=c(min(x@equating$eqYx, x@equating$eqXy), max(x@equating$eqYx, x@equating$eqXy)), ylab="Equated values", xlab="Score values")
      else{
        par(mfrow=c(2,1))
        plot(x@scores$X$x, x@equating$eqYx, pch=16, ylim=c(min(x@equating$eqYx, x@equating$eqXy), max(x@equating$eqYx, x@equating$eqXy)), ylab="Equated values", xlab="Score values")
        plot(x@scores$X$x, x@equating$SEEYx, pch=16, ylim=c(0, max(c(x@equating$SEEYx, x@equating$SEEXy))), ylab="SEE from X to Y", xlab="Score values")
      }
}
)

setMethod("plot", signature(x="genseed"), function(x){
    ylimvect1<-c(-max(abs(x@out$eqYxD), 2*x@out$SEEDYx),max(abs(x@out$eqYxD), 2*x@out$SEEDYx))
    ylabel1<-"eqYx1-eqYx2 +/- 2*SEEDYx"
    xlabel<-"Score values"
    plot(0:(length(x@out$eqYxD)-1), x@out$eqYxD, ylim=ylimvect1, ylab=ylabel1, xlab=xlabel)
    par(new=TRUE)
    plot(0:(length(x@out$eqYxD)-1), 2*x@out$SEEDYx, pch=16, ylim=ylimvect1, ylab=ylabel1, xlab=xlabel)
    par(new=TRUE)
    plot(0:(length(x@out$eqYxD)-1), -2*x@out$SEEDYx, pch=16, ylim=ylimvect1, ylab=ylabel1, xlab=xlabel)
  }
)

setMethod("plot", signature(x="cdist"), function(x){
    xlab1<-"Score values test A/Y"
    ylab1<-"Mean of X|A or X|Y"
    xlab2<-"Score values test A/Y"
    ylab2<-"Variance of X|A or X|Y"
    xlab3<-"Score values test X"
    ylab3<-"Mean of A|X or Y|X"
    xlab4<-"Score values test X"
    ylab4<-"Variance of A|X or Y|X"
    par(mfrow=c(2,2))
    ylim1<-c(min(x@est1[,1], x@obs1[,1], na.rm=TRUE), max(x@est1[,1], x@obs1[,1], na.rm=TRUE))
    plot(0:(length(x@est1[,1])-1), x@est1[,1], ylim=ylim1, xlab=xlab1, ylab=ylab1, pch=16)
    par(new=TRUE)
    plot(0:(length(x@obs1[,1])-1), x@obs1[,1], ylim=ylim1,  xlab=xlab1, ylab=ylab1,pch=17)

    ylim2<-c(min(x@est1[,2], x@obs1[,2], na.rm=TRUE), max(x@est1[,2], x@obs1[,2], na.rm=TRUE))
    plot(0:(length(x@est1[,2])-1), x@est1[,2], ylim=ylim2, xlab=xlab2, ylab=ylab2, pch=16)
    par(new=TRUE)
    plot(0:(length(x@obs1[,2])-1), x@obs1[,2], ylim=ylim2, xlab=xlab2, ylab=ylab2, pch=17)

    ylim3<-c(min(x@est2[,1], x@obs2[,1], na.rm=TRUE), max(x@est2[,1], x@obs2[,1], na.rm=TRUE))
    plot(0:(length(x@est2[,1])-1), x@est2[,1], ylim=ylim3,xlab=xlab3, ylab=ylab3, pch=16)
    par(new=TRUE)
    plot(0:(length(x@obs2[,1])-1), x@obs2[,1], ylim=ylim3,xlab=xlab3, ylab=ylab3, pch=17)

    ylim4<-c(min(x@est2[,2], x@obs2[,2], na.rm=TRUE), max(x@est2[,2], x@obs2[,2], na.rm=TRUE))
    plot(0:(length(x@est2[,2])-1), x@est2[,2], ylim=ylim4,xlab=xlab4, ylab=ylab4, pch=16)
    par(new=TRUE)
    plot(0:(length(x@obs2[,2])-1), x@obs2[,2], ylim=ylim4, xlab=xlab4, ylab=ylab4, pch=17)
  }
)

#setGeneric("cdist", function(est, obs, xscores, ascores){standardGeneric ("cdist")})
#setMethod("cdist", signature(est="glm"), function(est){
#  cdist(est=matrix(est@glmP$fitted.values, nrow=est@xscores), obs=matrix(est@glmP$y, nrow=est@xscores), xscores=est@xscores, yscores=est@yscores)
#  }
#)

#setGeneric("FTres",function(obs, fit){standardGeneric ("FTres")})
#setMethod("FTres", signature(obs="glm"), function(obs){
#  FT<-0
#  FTchi<-0
#  Pchi<-0
#	for(i in 1:length(obs$y)){
#		FT[i]<-sqrt(obs$y[i])+sqrt(obs$y[i]+1)-sqrt(4*obs$fitted.values[i]+1)
#		FTchi<-FTchi+FT[i]^2
#		Pchi<-Pchi+(obs$y[i]-obs$fitted.values[i])^2/obs$fitted.values[i]
#	}
#	return(FT)
#}
#)

setGeneric("summary")
setMethod("summary", signature(object="keout"), function(object){
  cat(" Design:", object@type, "\n\n")
  cat(" Kernel:", object@kernel, "\n\n")
  cat(" Sample Sizes: \n")
  cat("   Test X:", object@scores$N, "\n")
  cat("   Test Y:", object@scores$M, "\n\n")
  cat(" Score Ranges: \n")
  cat("   Test X: \n       Min =", min(object@scores$X$x), "Max =", max(object@scores$X$x))
  cat("\n   Test Y: \n       Min =", min(object@scores$Y$y), "Max =", max(object@scores$Y$y))  
  if(is(object@scores$A, "data.frame"))
    cat("\n   Test A: \n       Min =", min(object@scores$A$a), "Max =", max(object@scores$A$a))
  cat("\n\n","Bandwidths Used: \n")
  print(object@h)
  cat("\n Equating Function and Standard Errors: \n")
  print(data.frame(Score=object@scores$X$x, eqYx=object@equating$eqYx, SEEYx=object@equating$SEEYx))
  cat("\n Comparing the Moments: \n")
  print(object@PRE)}
)

setGeneric("getEquating",function(object){standardGeneric ("getEquating")})
setMethod("getEquating","keout",
 function(object){
 return(object@equating)
}
)

setGeneric("getPre",function(object){standardGeneric ("getPre")})
setMethod("getPre","keout",
 function(object){
 return(object@PRE)
}
)

setGeneric("getType",function(object){standardGeneric ("getType")})
setMethod("getType","keout",
 function(object){
 return(object@type)
}
)

setGeneric("getScores",function(object){standardGeneric ("getScores")})
setMethod("getScores","keout",
 function(object){
 return(object@scores)
}
)

setGeneric("getH",function(object){standardGeneric ("getH")})
setMethod("getH","keout",
 function(object){
 return(object@h)
}
)

setGeneric("getEq",function(object){standardGeneric ("getEq")})
setMethod("getEq","keout",
 function(object){
 return(object@equating$eqYx)
}
)

setGeneric("getSee",function(object){standardGeneric ("getSee")})
setMethod("getSee","keout",
 function(object){
 return(object@equating$SEEYx)
}
)

setGeneric("getEqlin",function(object){standardGeneric ("getEqlin")})
setMethod("getEqlin","keout",
 function(object){
 if(object@linear)
  return(object@equating$eqYx)
 else
  return(object@equating$eqYxLIN)
}
)

setGeneric("getSeelin",function(object){standardGeneric ("getSeelin")})
setMethod("getSeelin","keout",
 function(object){
 if(object@linear)
  return(object@equating$SEEYx)
 else
  return(object@equating$SEEYxLIN)
}
)

setGeneric("getSeed",function(object){standardGeneric ("getSeed")})
setMethod("getSeed","keout",
 function(object){
 return(new("genseed", out=data.frame(eqYxD=(object@equating$eqYx-object@equating$eqYxLIN), SEEDYx=object@equating$SEEDYx)))
}
)

setGeneric("getEqirt",function(object){standardGeneric ("getEqirt")})
setMethod("getEqirt","keout",
 function(object){
 return(object@equating$eqYxIRT)
}
)

setGeneric("getSeeirt",function(object){standardGeneric ("getSeeirt")})
setMethod("getSeeirt","keout",
 function(object){
 return(object@equating$SEEYxIRT)
}
)

#adjust the covariance matrix of the item parameters according to the parametrization used
adjltm <- function(mat, pars, design, model){
  if(design=="CE" || design=="PSE"){
    if(model=="2pl"){
      pderltmP <- matrix(0, nrow=2*(length(pars$ax)+length(pars$aa)), ncol=2*(length(pars$ax)+length(pars$aa)))
      
      #print(bx)
      #print(aaP)
      
      for(i in 1:(length(pars$ax))){
        pderltmP[i,i] <- -1/pars$ax[i]
      }
      for(i in (length(pars$ax)+1):(length(pars$ax)+length(pars$aa))){
        pderltmP[i,i] <- -1/pars$aa[i-length(pars$ax)]
      }
      
      for(i in (length(pars$ax)+length(pars$aa)+1):(2*length(pars$ax)+length(pars$aa))){
        pderltmP[i,i-length(pars$aa)-length(pars$ax)] <- pars$bxltm[i-length(pars$ax)-length(pars$aa)]/(pars$ax[i-length(pars$ax)-length(pars$aa)])^2
        pderltmP[i,i] <- 1
      }
      
      for(i in (2*length(pars$ax)+length(pars$aa)+1):(2*(length(pars$ax)+length(pars$aa)))){
        pderltmP[i,i-length(pars$aa)-length(pars$ax)] <- pars$baltm[i-2*length(pars$ax)-length(pars$aa)]/(pars$aa[i-2*length(pars$ax)-length(pars$aa)])^2
        pderltmP[i,i] <- 1
      }
      return(t(pderltmP) %*% mat %*% pderltmP)
    }
       
    if(model=="3pl"){
      pderltmP <- matrix(0, nrow=3*(length(pars$ax)+length(pars$aa)), ncol=3*(length(pars$ax)+length(pars$aa)))
       
      for(i in 1:(length(pars$ax)+length(pars$aa))){
        pderltmP[i,i] <- 1
      }
       
      for(i in (length(pars$ax)+length(pars$aa)+1):(2*length(pars$ax)+length(pars$aa))){
        pderltmP[i,i] <- -1/pars$ax[i-length(pars$ax)-length(pars$aa)]
      }
      for(i in (2*length(pars$ax)+length(pars$aa)+1):(2*length(pars$ax)+2*length(pars$aa))){
        pderltmP[i,i] <- -1/pars$aa[i-2*length(pars$ax)-length(pars$aa)]
      }
         
      for(i in (2*length(pars$ax)+2*length(pars$aa)+1):(3*length(pars$ax)+2*length(pars$aa))){
        pderltmP[i,i-length(pars$ax)-length(pars$aa)] <- pars$bxltm[i-2*length(pars$ax)-2*length(pars$aa)]/(pars$ax[i-2*length(pars$ax)-2*length(pars$aa)])^2
        pderltmP[i,i] <- 1
      }
         
      for(i in (3*length(pars$ax)+2*length(pars$aa)+1):(3*length(pars$ax)+3*length(pars$aa))){
        pderltmP[i,i-length(pars$ax)-length(pars$aa)] <- pars$baltm[i-3*length(pars$ax)-2*length(pars$aa)]/(pars$aa[i-3*length(pars$ax)-2*length(pars$aa)])^2
        pderltmP[i,i] <- 1
      }
      #print(pderltmP)
      #print(aaP)
      return(t(pderltmP) %*% mat %*% pderltmP)
    }  
  }
  if(design=="EG"){
    if(model=="2pl"){
      pderltmP <- matrix(0, nrow=2*length(pars$ax), ncol=2*length(pars$ax))
      for(i in 1:(length(pars$ax))){
        pderltmP[i,i] <- -1/pars$ax[i]
      }
      for(i in (length(pars$ax)+1):(2*length(pars$ax))){
        pderltmP[i,i-length(pars$ax)] <- pars$bxltm[i-length(pars$ax)]/(pars$ax[i-length(pars$ax)])^2
        pderltmP[i,i] <- 1
      }
      return(t(pderltmP) %*% mat %*% pderltmP)
    }

    if(model=="3pl"){
      pderltmP <- matrix(0, nrow=3*length(pars$ax), ncol=3*length(pars$ax))
      for(i in 1:(length(pars$ax))){
        pderltmP[i,i] <- 1
      }
      for(i in (length(pars$ax)+1):(2*length(pars$ax))){
        pderltmP[i,i] <- -1/pars$ax[i-length(pars$ax)]
      }
      for(i in (2*length(pars$ax)+1):(3*length(pars$ax))){
        pderltmP[i,i-length(pars$ax)] <- pars$bxltm[i-2*length(pars$ax)]/(pars$ax[i-2*length(pars$ax)])^2
        pderltmP[i,i] <- 1
      }
      return(t(pderltmP) %*% mat %*% pderltmP)
    }
  }  
}

altoptdensity <- function(r, h, var, mean, eqx, x){
  res<-numeric(length(eqx))
  a<-sqrt(var/(var+h^2))
  for(i in 1:length(eqx)){
    ff<-0
    for(j in 1:length(r)){
        Rx<-(eqx[i]-a*x[j]-(1-a)*mean)/(a*h)
        ff<-ff+x[j]*r[j]*dnorm(Rx)/(a*h)
    }
    res[i]<-ff
  }
  return(res)
}

#continuization function
cdf<-function(r, h, mean, var, kernel, slog, bunif){    #r=estimated score probabilities, h=optimal h, mean of test, var of test
  a<-sqrt(var/(var+h^2))
  al<-sqrt(var/(var+((pi^2*slog^2)/3)*h^2))
  au<-sqrt(var/(var+((bunif^2)/3)*h^2))
  x<-c(0:(length(r)-1))
  cdf<-numeric(length(r))
  for(i in 1:length(r)){
    cdftemp<-0
    for(j in 1:length(r)){
      Rx<-(x[i]-a*x[j]-(1-a)*mean)/(a*h)
      if(kernel=="gaussian"){
        Rx<-(x[i]-a*x[j]-(1-a)*mean)/(a*h)
        cdftemp<-cdftemp+r[j]*pnorm(Rx)
      }
      if(kernel=="stdgaussian"){
        Rx<-(x[i]-x[j])/(h)
        cdftemp<-cdftemp+r[j]*pnorm(Rx)
      }
      if(kernel=="logistic"){
        Rx<-(x[i]-al*x[j]-(1-al)*mean)/(al*h)
        cdftemp<-cdftemp+r[j]*( 1 / (1+exp(-(Rx)/slog)) )
      }
      if(kernel=="uniform"){
        Rx<-(x[i]-au*x[j]-(1-au)*mean)/(au*h)
        cdftemp<-cdftemp+r[j]*punif(Rx, min=-bunif, max=bunif)
      }
    }
    cdf[i]<-cdftemp
  }
  return(cdf)
}

#continuization function for chain equating
cdfce<-function(r, ha, mean, var, eqa, a, kernel, slog, bunif){    #r=estimated score probabilities, h=optimal h, mean of test, var of test
  aa<-sqrt(var/(var+ha^2))
  al<-sqrt(var/(var+((pi^2*slog^2)/3)*ha^2))
  au<-sqrt(var/(var+((bunif^2)/3)*ha^2))
  cdf<-numeric(length(eqa))
  for(i in 1:length(eqa)){
    cdftemp<-0
    for(j in 1:length(a)){
      if(kernel=="gaussian"){
        Rx<-(eqa[i]-aa*a[j]-(1-aa)*mean)/(aa*ha)
        cdftemp<-cdftemp+r[j]*pnorm(Rx)
      }
      if(kernel=="stdgaussian"){
        Rx<-(eqa[i]-a[j])/(ha)
        cdftemp<-cdftemp+r[j]*pnorm(Rx)
      }
      if(kernel=="logistic"){
        Rx<-(eqa[i]-al*a[j]-(1-al)*mean)/(al*ha)
        cdftemp<-cdftemp+r[j]*(1/(1+exp(-(Rx)/slog)))
      }
      if(kernel=="uniform"){
        Rx<-(eqa[i]-au*a[j]-(1-au)*mean)/(au*ha)
        cdftemp<-cdftemp+r[j]*punif(Rx, min=-bunif, max=bunif)
      }
    }
    cdf[i]<-cdftemp
  }
  cdf
}

#calculates conditional means, variances, skewnesses and kurtoses of observed and pre-smoothed distributions
cdist<-function(est, obs, xscores, ascores){  															
	if(missing(xscores))
		xscores<-c(0:(dim(est)[1]-1))
	if(missing(ascores))
		ascores<-c(0:(dim(est)[2]-1))
	est1<-t(matrix(apply(est, 2, cmoments, x=xscores), ncol=length(ascores), dimnames=list(c("mean", "variance", "skewness", "kurtosis"))))
	obs1<-t(matrix(apply(obs, 2, cmoments, x=xscores), ncol=length(ascores), dimnames=list(c("mean", "variance", "skewness", "kurtosis"))))
	est2<-t(matrix(apply(est, 1, cmoments, x=ascores), ncol=length(xscores), dimnames=list(c("mean", "variance", "skewness", "kurtosis"))))
	obs2<-t(matrix(apply(obs, 1, cmoments, x=ascores), ncol=length(xscores), dimnames=list(c("mean", "variance", "skewness", "kurtosis"))))
	return(new("cdist", est1=est1, est2=est2, obs1=obs1, obs2=obs2))
}

#calculates the c-matrix used in SEE calculation (slower than cmatrixSG but uses less memory)
cmatris<-function(p, DM, N){
  
  QR<-matrix(0, nrow=length(p), ncol=dim(DM)[2])
  vector<-matrix(0, nrow=1, ncol=length(p))
  for(i in 1:length(p)){
    vectdiag<-vector
    vectdiag[i]<-sqrt(p[i])
    QR[i,]<-(vectdiag - sqrt(p[i]) %*% t(p)) %*% DM 
  }
  QRq<-qr.Q(qr(QR))
  res<-matrix(0, nrow=length(p), ncol=dim(DM)[2])
  for(i in 1:length(p)){
    vectdiag<-vector
    vectdiag[i]<-sqrt(p[i])
    res[i,]<-vectdiag%*%QRq
  }

  return((1/sqrt(N))*res)
}

#calculates the c-matrix used in SEE calculation (uses a lot of memory if DM is large)
cmatrixSG<-function(p, DM, N){  			#p=vector of estimated probabilities for possible score comb.
									#DM is the prepared design matrix specifying the log-lin. model
									#N=sample size
	Dp<-diag(sqrt(p))	
	QR<-(Dp-sqrt(p)%*%t(p))%*%DM
	return((1/sqrt(N))*Dp%*%qr.Q(qr(QR)))
}

#sample mean, variance, skewness and kurtosis for a probability or frequency vector r and score value vector x
cmoments<-function(r, x){
	mean<-(r/sum(r))%*%x
	var<-(x^2%*%(r/sum(r))-mean^2)
	skewness<-((r/sum(r))%*%(x-mean)^3)/var^(3/2)
	kurtosis<-(1/var^2)*(r/sum(r))%*%((x-mean)^4)
	return(c(mean, var, skewness, kurtosis))
}

#density for each score value
densityeq<-function(r, h, var, mean, eqx, x, kernel, slog, bunif){
  res<-numeric(length(eqx))
  a<-sqrt(var/(var+h^2))
  al<-sqrt(var/(var+((pi^2*slog^2)/3)*h^2))
  au<-sqrt(var/(var+((bunif^2)/3)*h^2))
  for(i in 1:length(eqx)){
    ff<-0
    if(kernel=="gaussian")
      ff <- sum(r*dnorm((eqx[i]-a*x-(1-a)*mean)/(a*h))/(a*h) )
    if(kernel=="stdgaussian")
      ff <- sum(r*dnorm((eqx[i]-x)/(h))/(h) )
    if(kernel=="logistic")
      ff <- sum(r*(exp(-((eqx[i]-al*x-(1-al)*mean)/(al*h))/slog) / (slog*(1+exp(-((eqx[i]-al*x-(1-al)*mean)/(al*h))/slog))^2) /(al*h)))
    if(kernel=="uniform")
      ff<-sum(r*dunif((eqx[i]-au*x-(1-au)*mean)/(au*h), min=-bunif, max=bunif)/(au*h))
    res[i]<-ff
  }
  return(res)
}

#calculates the derivatives of F w/ respect to r for the score values in eqx
dFdr<-function(r, h, var, mean, fprim, eqx, x, kernel, slog, bunif, altopt=FALSE, altfprim=0){        	
  a<-sqrt(var/(var+h^2))
  al<-sqrt(var/(var+((pi^2*slog^2)/3)*h^2))
  au<-sqrt(var/(var+((bunif^2)/3)*h^2))
  dFdrest<-matrix(0, length(eqx), ncol=length(x))
  #	if(kernel=="uniform")
  #		
  for(i in 1:length(eqx)){
    if(kernel=="gaussian"){
      if(altopt){
        Rx <- (eqx[i] - a * x - (1 - a) * mean) / (a * h)
        Mx <- -(1 /2) * eqx[i] * ((x - mean) / sqrt(var))^2 + (1 - a) * ((1/2) * mean * ((x - mean) / sqrt(var))^2 - x)
        dFdrest[i, ] <- pnorm(Rx) + Mx * fprim[i] + (1/2) * ((x - mean) / sqrt(var))^2 * altfprim[i]
      }
      else{
        Rx <- (eqx[i] - a * x - (1 - a) * mean) / (a * h)
        Mx <- (1 / 2) * (eqx[i] - mean) * (1 - a^2) * (((x - mean) / sqrt(var))^2) + (1 - a) * x
        dFdrest[i, ] <- pnorm(Rx) - Mx * fprim[i]
      }
    }
    if(kernel=="stdgaussian"){
      if(altopt){
        Rx <- (eqx[i] -  x) / (h)
        Mx <- - Rx * (1 /2) * (sqrt(var))*((x - mean) / sqrt(var))^2
        dFdrest[i, ] <- pnorm(Rx) + Mx * fprim[i]
      }
      else{
        Rx <- (eqx[i] -  x) / (h)
        dFdrest[i, ] <- pnorm(Rx)
      }
    }
    if(kernel=="logistic"){
      Rx<-(eqx[i]-al*x-(1-al)*mean)/(al*h)
      Mx<-(1/2)*(eqx[i]-mean)*(1-al^2)*(((x-mean)/sqrt(var))^2)+(1-al)*x
      dFdrest[i, ]<-(1/(1+exp(-(Rx)/slog)))-Mx*fprim[i]
    }
    if(kernel=="uniform"){
      Rx<-(eqx[i]-au*x-(1-au)*mean)/(au*h)
      Mx<-(1/2)*(eqx[i]-mean)*(1-au^2)*(((x-mean)/sqrt(var))^2)+(1-au)*x
      dFdrest[i,]<-punif(Rx, min=-bunif, max=bunif)-Mx*fprim[i]
    }
  }
return(dFdrest)									#We obtain a J x J matrix of derivatives evaluated at each value of eqx.
}

#calculate the equating coefficients
# eqcoefficients <- function(irt1, irt2, x, y, a, type="MM"){
#   kx <- length(x)
#   ky <- length(y)
#   ka <- length(a)
#   if(type=="MM"){
#     if(is(irt1)=="rasch" && is(irt2)=="rasch"){
#       betaB <- sum(coef(irt2)[(kx+1):(kx+ka),1])/sum(coef(irt1)[(ky+1):(ky+ka),1])
#       return(betaB)
#     }      
#     if(is(irt1)=="ltm" && is(irt2)=="ltm"){
#       betaA <- sum(coef(irt2)[(kx+1):(kx+ka),2])/sum(coef(irt1)[(ky+1):(ky+ka),2])
#       betaB <- mean(coef(irt1)[(kx+1):(kx+ka),1])-betaA*mean(coef(irt2)[(kx+1):(kx+ka),1])
#       return(c(betaA, betaB))
#     }      
#     if(is(irt1)=="tpm" && is(irt2)=="tpm"){
#       betaA <- sum(coef(irt2)[(kx+1):(kx+ka),3])/sum(coef(irt1)[(ky+1):(ky+ka),3])
#       betaB <- mean(coef(irt1)[(kx+1):(kx+ka),2])-betaA*mean(coef(irt2)[(kx+1):(kx+ka),2])
#       return(c(betaA, betaB))
#     }    
#   }
#   else return()
# }

#Let's say we want to equate X to Y. cdf=estimated
#cdf for test X, vector w/ J entries. r=estimated
#score probabilities for test Y. We specify mean
#var and optimal h for Y. We go through all entries in
#the estimated cdf for X, and calculate the corresponding
#score for test Y for that particular value. A vector w/
#J entries is returned.
eqinv<-function(cdf, r, x, var, mean, h, kernel, slog, bunif, smoothed=TRUE, IRT=FALSE){
	eqf<-numeric(length(cdf))
	maxx <- max(x)
	minx <- min(x)
	for(i in 1:length(cdf)){
		u<-cdf[i]
		ggg<-function(x1, r, mean, var, h, u, kernel, slog, bunif){
		  aa<-sqrt(var/(var+h^2))
		  al<-sqrt(var/(var+((pi^2*slog^2)/3)*h^2))
		  au<-sqrt(var/(var+((bunif^2)/3)*h^2))
			x<-c(0:(length(r)-1))
			cdftemp<-0
			for(j in 1:length(r)){
			  if(kernel=="gaussian"){
			    Rx<-(x1-aa*x[j]-(1-aa)*mean)/(aa*h)
			    cdftemp<-cdftemp+r[j]*pnorm(Rx)
			  }
			  if(kernel=="stdgaussian"){
			    Rx<-(x1-x[j])/(h)
			    cdftemp<-cdftemp+r[j]*pnorm(Rx)
			  }
			  if(kernel=="logistic"){
			    Rx<-(x1-al*x[j]-(1-al)*mean)/(al*h)
			    cdftemp<-cdftemp+r[j]*( 1 / (1+exp(-(Rx)/slog)) )
			  }
			  if(kernel=="uniform"){
			    Rx<-(x1-au*x[j]-(1-au)*mean)/(au*h)
          cdftemp<-cdftemp+r[j]*punif(Rx, min=-bunif, max=bunif)
			  }
			}
			cdftemp-u
		}
    if(smoothed==FALSE || IRT==TRUE){
      checkeq<-tryCatch(uniroot(ggg, c(-50, 200), tol=0.000001, r, mean, var, h, u, kernel, slog, bunif), error=function(err) stop("Could not calculate equating function, likely due to sparse data. Try values of hx and hy that are equal, or try imputing the data or use pre-smoothing."))
      eqf[i]<-checkeq$root
    }
    else
		  eqf[i]<-uniroot(ggg, c(-50, 200), tol=0.000001, r, mean, var, h, u, kernel, slog, bunif)$root
	}
	eqf
}

#Let's say we want to link X to A on P.
#cdf=estimated cdf for test X on P, vector w/ J entries. 
#t=estimated score probabilities for test A on P. 
#a=possible score values for A
#We specify mean, var and optimal h for A on P. 
#We go through all entries in the estimated cdf for X, and 
#calculate the corresponding score for test A for that particular value.
#A vector w/ J entries is returned.

eqinvCE<-function(cdf, t, x, a, var, mean, h, kernel, slog, bunif, smoothed=TRUE, IRT=FALSE){    	
  eqf<-0
  maxx <- max(x)
  minx <- min(x)
  for(i in 1:length(cdf)){
    u<-cdf[i]
    ggg<-function(a1, t, mean, var, h, u, a, kernel, slog, bunif){
      aa<-sqrt(var/(var+h^2))
      al<-sqrt(var/(var+((pi^2*slog^2)/3)*h^2))
      au<-sqrt(var/(var+((bunif^2)/3)*h^2))
      cdftemp<-0
      for(j in 1:length(t)){
        if(kernel=="gaussian"){
          Rx<-(a1-aa*a[j]-(1-aa)*mean)/(aa*h)
          cdftemp<-cdftemp+t[j]*pnorm(Rx)
        }
        if(kernel=="stdgaussian"){
          Rx<-(a1-a[j])/(h)
          cdftemp<-cdftemp+t[j]*pnorm(Rx)
        }
        if(kernel=="logistic"){
          Rx<-(a1-al*a[j]-(1-al)*mean)/(al*h)
          cdftemp<-cdftemp+t[j]*( 1 / (1+exp(-(Rx)/slog)) )
        }
        if(kernel=="uniform"){
          Rx<-(a1-au*a[j]-(1-au)*mean)/(au*h)
          cdftemp<-cdftemp+t[j]*punif(Rx, min=-bunif, max=bunif)
        }
      }
      cdftemp-u
    }
    if(smoothed==FALSE || IRT==TRUE){
      checkeq<-tryCatch(uniroot(ggg, c(minx-1000, maxx+1000), tol=0.000001, t, mean, var, h, u, a, kernel, slog, bunif), error=function(err) stop("Could not calculate equating function, likely due to sparse data. Try values of hx and hy that are equal, or try imputing the data or use pre-smoothing."))
      eqf[i]<-checkeq$root
    }
    else
      eqf[i]<-uniroot(ggg, c(minx-1000, maxx+1000), tol=0.000001, t, mean, var, h, u, a, kernel, slog, bunif)$root
  }
  return(eqf)
}

#calculate Freeman-Tukey residuals
FTres<-function(obs, fit){
  FT<-0
  FTchi<-0
	Pchi<-0
	for(i in 1:length(obs)){
		FT[i]<-sqrt(obs[i])+sqrt(obs[i]+1)-sqrt(4*fit[i]+1)
		FTchi<-FTchi+FT[i]^2
		Pchi<-Pchi+(obs[i]-fit[i])^2/fit[i]
	}
	return(FT)
}

#calculate SEED between two equating functions
genseed<-function(in1, in2, linear=FALSE){
    if(!is(in1, "keout")||!is(in2, "keout"))
      stop("Input objects must be of class 'keout'.")
    if(linear==TRUE)
      if(dim(in1@SEEvect@SEEYxLIN)[1]!=dim(in2@SEEvect@SEEYxLIN)[1])
        return(print("SEE vectors must be of equal length"))     
    else
      if(dim(in1@SEEvect@SEEYx)[1]!=dim(in2@SEEvect@SEEYx)[1])
        return(print("SEE vectors must be of equal length"))

    if(linear==TRUE){
      SEEDYx<-numeric(dim(in1@SEEvect@SEEYxLIN)[1])
      for(i in 1:dim(in1@SEEvect@SEEYxLIN)[1]){
        SEEDYx[i]<-sqrt(sum((in1@SEEvect@SEEYxLIN[i,]-in2@SEEvect@SEEYxLIN[i,])^2))
      }
      if("vector" %in% is(in1@equating$eqYx))
        out<-data.frame(eqYxD=in1@equating$eqYx-in2@equating$eqYx, SEEDYx)
      else
        out<-data.frame(eqYxD=in1@equating$eqYxLIN-in2@equating$eqYxLIN, SEEDYx)
      return(new("genseed", out=out))
    }
    else{
      SEEDYx<-numeric(dim(in1@SEEvect@SEEYx)[1])
      for(i in 1:dim(in1@SEEvect@SEEYx)[1]){
        SEEDYx[i]<-sqrt(sum((in1@SEEvect@SEEYx[i,]-in2@SEEvect@SEEYx[i,])^2))
      }
      out<-data.frame(eqYxD=in1@equating$eqYx-in2@equating$eqYx, SEEDYx)
      return(new("genseed", out=out))
    }
}

irtose <- function(design="CE", P, Q, x, y, a=0, qpoints, model="2pl", see="analytical", replications=50, kernel="gaussian", h=list(hx=0, hy=0, hxP=0, haP=0, hyQ=0, haQ=0), hlin=list(hxlin=0, hylin=0, hxPlin=0, haPlin=0, hyQlin=0, haQlin=0), KPEN=0, wpen=0.5, linear=FALSE, slog=1, bunif=1, altopt=FALSE, wS=0.5, eqcoef="mean-mean", robust=FALSE){
  
  #P - object from ltm with IRT model for group P, where the first items are main test, the last are the anchor test OR a matrix of responses w/out missing values, rows are individuals, columns are items, the main test first, then the anchor test
  #Q - equivalent to P, for group Q
  #x - score points test X
  #y - score points test Y
  #a - score points test A
  #qpoints - quadrature points to be used, if not specified defaults to the quadrature points used in the IRT model 
  #model - IRT model, "1pl", "2pl", "3pl" (start w/ 2pl)
  #eqcoef - (PSE only) denotes the method used estimating equating coefficients, "mean-mean", "mean-sigma", "mean-gmean", "Haebara", "Stocking-Lord"
  #see kequate() for rest args
  if(design=="CE"){
    if(model=="2pl"){
    if("ltm" %in% class(P)){
      bx <- as.numeric(coef.ltm(P)[,1])[1:(length(x)-1)]
      baP <- as.numeric(coef.ltm(P)[,1])[(length(x)):((length(x)+length(a)-2))]
      ax <- as.numeric(coef.ltm(P)[,2])[1:(length(x)-1)]
      aaP <- as.numeric(coef.ltm(P)[,2])[(length(x)):((length(x)+length(a)-2))]
      if(P$IRT.param){
        bxltm <- -ax*bx
        baPltm <- -aaP*baP
      } else{
        bxltm <- bx
        baPltm <- baP
        bx <- -bxltm/ax
        baP <- -baPltm/aaP
      }
      N <- dim(P$X)[1]
      ltmP <- P
      P <- ltmP$X
    } else{
      if(is.matrix(P)){
        if((length(a)+length(x)-2) != ncol(P))
          return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
        ltmP <- ltm(P ~ z1, IRT.param=FALSE)
        bx <- as.numeric(coef.ltm(ltmP)[,1])[1:(length(x)-1)]
        baP <- as.numeric(coef.ltm(ltmP)[,1])[(length(x)):((length(x)+length(a)-2))]
        ax <- as.numeric(coef.ltm(ltmP)[,2])[1:(length(x)-1)]
        aaP <- as.numeric(coef.ltm(ltmP)[,2])[(length(x)):((length(x)+length(a)-2))]
        bxltm <- bx
        baPltm <- baP
        bx <- -bxltm/ax
        baP <- -baPltm/aaP
        N <- dim(P)[1]
      } else{
        return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
      }
    }
    
    if("ltm" %in% class(Q)){
      by <- as.numeric(coef.ltm(Q)[,1])[1:(length(y)-1)]
      baQ <-  as.numeric(coef.ltm(Q)[,1])[(length(y)):((length(y)+length(a)-2))]
      ay <- as.numeric(coef.ltm(Q)[,2])[1:(length(y)-1)]
      aaQ <- as.numeric(coef.ltm(Q)[,2])[(length(y)):((length(y)+length(a)-2))]
      if(Q$IRT.param){
        byltm <- -ay*by
        baQltm <- -aaQ*baQ
      } else{
        byltm <- by
        baQltm <- baQ
        by <- -byltm/ay
        baQ <- -baQltm/aaQ
      }
      M <- dim(Q$X)[1]
      ltmQ <- Q
      Q <- ltmQ$X
    } else{
      if(is.matrix(Q)){
        if((length(a)+length(y)-2) != ncol(Q))
          return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
        ltmQ <- ltm(Q ~ z1, IRT.param=FALSE)
        by <- as.numeric(coef.ltm(ltmQ)[,1])[1:(length(y)-1)]
        baQ <-  as.numeric(coef.ltm(ltmQ)[,1])[(length(y)):((length(y)+length(a)-2))]
        ay <- as.numeric(coef.ltm(ltmQ)[,2])[1:(length(y)-1)]
        aaQ <- as.numeric(coef.ltm(ltmQ)[,2])[(length(y)):((length(y)+length(a)-2))]
        byltm <- by
        baQltm <- baQ
        by <- -byltm/ay
        baQ <- -baQltm/aaQ
        M <- dim(Q)[1]
      } else{
        return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
      }
    }
    }
    
    if(model=="3pl"){
      if("tpm" %in% class(P)){
        cx <- as.numeric(coef.tpm(P)[,1])[1:(length(x)-1)]
        caP <- as.numeric(coef.tpm(P)[,1])[(length(x)):((length(x)+length(a)-2))]
        bx <- as.numeric(coef.tpm(P)[,2])[1:(length(x)-1)]
        baP <-  as.numeric(coef.tpm(P)[,2])[(length(x)):((length(x)+length(a)-2))]
        ax <- as.numeric(coef.tpm(P)[,3])[1:(length(x)-1)]
        aaP <- as.numeric(coef.tpm(P)[,3])[(length(x)):((length(x)+length(a)-2))]
        if(P$IRT.param){
          bxltm <- -ax*bx
          baPltm <- -aaP*baP
        } else{
          bxltm <- bx
          baPltm <- baP
          bx <- -bxltm/ax
          baP <- -baPltm/aaP
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(a)+length(x)-2) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          cx <- as.numeric(coef.tpm(ltmP)[,1])[1:(length(x)-1)]
          caP <- as.numeric(coef.tpm(ltmP)[,1])[(length(x)):((length(x)+length(a)-2))]
          bx <- as.numeric(coef.tpm(ltmP)[,2])[1:(length(x)-1)]
          baP <- as.numeric(coef.tpm(ltmP)[,2])[(length(x)):((length(x)+length(a)-2))]
          ax <- as.numeric(coef.tpm(ltmP)[,3])[1:(length(x)-1)]
          aaP <- as.numeric(coef.tpm(ltmP)[,3])[(length(x)):((length(x)+length(a)-2))]
          bxltm <- bx
          baPltm <- baP
          bx <- -bxltm/ax
          baP <- -baPltm/aaP
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("tpm" %in% class(Q)){
        cy <- as.numeric(coef.tpm(Q)[,1])[1:(length(y)-1)]
        caQ <- as.numeric(coef.tpm(Q)[,1])[(length(y)):((length(y)+length(a)-2))]
        by <- as.numeric(coef.tpm(Q)[,2])[1:(length(y)-1)]
        baQ <-  as.numeric(coef.tpm(Q)[,2])[(length(y)):((length(y)+length(a)-2))]
        ay <- as.numeric(coef.tpm(Q)[,3])[1:(length(y)-1)]
        aaQ <- as.numeric(coef.tpm(Q)[,3])[(length(y)):((length(y)+length(a)-2))]
        if(Q$IRT.param){
          byltm <- -ay*by
          baQltm <- -aaQ*baQ
        } else{
          byltm <- by
          baQltm <- baQ
          by <- -byltm/ay
          baQ <- -baQltm/aaQ
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(a)+length(y)-2) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- tpm(Q, IRT.param=FALSE)
          cy <- as.numeric(coef.tpm(ltmQ)[,1])[1:(length(y)-1)]
          caQ <- as.numeric(coef.tpm(ltmQ)[,1])[(length(y)):((length(y)+length(a)-2))]
          by <- as.numeric(coef.tpm(ltmQ)[,2])[1:(length(y)-1)]
          baQ <-  as.numeric(coef.tpm(ltmQ)[,2])[(length(y)):((length(y)+length(a)-2))]
          ay <- as.numeric(coef.tpm(ltmQ)[,3])[1:(length(y)-1)]
          aaQ <- as.numeric(coef.tpm(ltmQ)[,3])[(length(y)):((length(y)+length(a)-2))]
          byltm <- by
          baQltm <- baQ
          by <- -byltm/ay
          baQ <- -baQltm/aaQ
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
    }
    
    if(missing(qpoints))
      qpoints <- -ltmP$GH$Z[,2]
    
    if(model=="1pl"){
      irtx <- probpl(qpoints, bx, model)
      irtaP <- probpl(qpoints, baP, model)
      irty <- probpl(qpoints, by, model)
      irtaQ <- probpl(qpoints, baQ, model)
      #pderX <- pderpl(irtx, qpoints, bx, model)
      #pderAP <- pderpl(irtaP, qpoints, baP, model)
      #pderY <- pderpl(irty, qpoints, by, model)
      #pderAQ <- pderpl(irtaQ, qpoints, baQ, model)
    }
    if(model=="2pl"){
      irtx <- probpl(qpoints, bx, model, a=ax)
      irtaP <- probpl(qpoints, baP, model, a=aaP)
      irty <- probpl(qpoints, by, model, a=ay)
      irtaQ <- probpl(qpoints, baQ, model, a=aaQ)
      #pderX <- pderpl(irtx, qpoints, bx, model, a=ax)
      #pderAP <- pderpl(irtaP, qpoints, baP, model, a=aaP)
      #pderY <- pderpl(irty, qpoints, by, model, a=ay)
      #pderAQ <- pderpl(irtaQ, qpoints, baQ, model, a=aaQ)
    }
    if(model=="3pl"){
      irtx <- probpl(qpoints, bx, a=ax, c=cx)
      irtaP <- probpl(qpoints, baP, a=aaP, c=caP)
      irty <- probpl(qpoints, by, a=ay, c=cy)
      irtaQ <- probpl(qpoints, baQ, a=aaQ, c=caQ)
      #pderX <- pderpl(irtx, qpoints, bx, a=ax, c=cx)
      #pderAP <- pderpl(irtaP, qpoints, baP, a=aaP, c=caP)
      #pderY <- pderpl(irty, qpoints, by, a=ay, c=cy)
      #pderAQ <- pderpl(irtaQ, qpoints, baQ, a=aaQ, c=caQ)
    }
    
    #irtx <- probpl(qpoints, bx, model, a=ax)
    #irtaP <- probpl(qpoints, baP, model, a=aaP)
    #irty <- probpl(qpoints, by, model, a=ay)
    #irtaQ <- probpl(qpoints, baQ, model, a=aaQ)
    
    rP <- LordWW(irtx, qpoints)
    tP <- LordWW(irtaP, qpoints)
    sQ <- LordWW(irty, qpoints)
    tQ <- LordWW(irtaQ, qpoints)
    
    #   covprobx <- 
    #   covprobaP <- 
    #   covproby <- 
    #   covprobQ <- 
    
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    } else ulimit<-4
    
    meanx <- x%*%rP
    meany <- y%*%sQ
    meanaP <- a%*%tP
    meanaQ <- a%*%tQ
    varx <- (N/(N-1))*(x^2%*%rP-meanx^2)
    vary <- (M/(M-1))*(y^2%*%sQ-meany^2)
    varaP <- (N/(N-1))*(a^2%*%tP-meanaP^2)
    varaQ <- (M/(M-1))*(a^2%*%tQ-meanaQ^2)
    if(linear){
      if(hlin$hxPlin==0)
        hxP<-as.numeric(1000*sqrt(varx))
      else
        hxP<-hlin$hxPlin
      if(hlin$hyQlin==0)
        hyQ<-as.numeric(1000*sqrt(vary))
      else
        hyQ<-hlin$hyQlin
      if(hlin$haPlin==0)
        haP<-as.numeric(1000*sqrt(varaP))
      else
        haP<-hlin$haPlin
      if(hlin$haQlin==0)
        haQ<-as.numeric(1000*sqrt(varaQ))
      else
        haQ<-hlin$haQlin
    }
    else{
      if(h$hxP==0){
        if(altopt)
          hxP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
      else{
        if(KPEN==0)
          hxP<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hxPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hxP<-optimize(PEN, interval=c(hxPPEN1min, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      } else{
        hxP <- h$hxP
      }
      if(h$hyQ==0){
        if(altopt)
          hyQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
      else{
        if(KPEN==0)
          hyQ<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hyQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hyQ<-optimize(PEN, interval=c(hyQPEN1min, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      } else{
        hyQ <- h$hyQ
      }
      if(h$haP==0){
        if(altopt)
          haP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varaP)
      else{
        if(KPEN==0)
          haP<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          haPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          haP<-optimize(PEN, interval=c(haPPEN1min, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      } else{
        haP <- h$haP
      }
      if(h$haQ==0){
        if(altopt)
          haQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(varaQ)
      else{
        if(KPEN==0)
          haQ<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          haQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          haQ<-optimize(PEN, interval=c(haQPEN1min, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      }
      else{
        haQ <- h$haQ
      }
    }
      hxPlin<-as.numeric(1000*sqrt(varx))
      hyQlin<-as.numeric(1000*sqrt(vary))
      haPlin<-as.numeric(1000*sqrt(varaP))
      haQlin<-as.numeric(1000*sqrt(varaQ))

    if(see=="bootstrap"){
      if(model=="2pl"){
        bootsee <- matrix(0, nrow=replications, ncol=length(x))
        for(i in 1:replications){
          Pboot <- P[sample(1:N, replace=TRUE),]
          Qboot <- Q[sample(1:M, replace=TRUE),]
          ltmQboot <- ltm(Pboot ~ z1, IRT.param=FALSE)
          ltmPboot <- ltm(Qboot ~ z1, IRT.param=FALSE)
          bootsee[i,] <- irtoseeq(ltmPboot, ltmQboot, x, y, a, qpoints, N, M, model, design, kernel, slog, bunif, KPEN, wpen, linear, altopt, h=h, hlin=hlin)
        }
      }
      if(model=="3pl"){
        bootsee <- matrix(0, nrow=replications, ncol=length(x))
        for(i in 1:replications){
          Pboot <- P[sample(1:N, replace=TRUE),]
          Qboot <- Q[sample(1:M, replace=TRUE),]
          ltmQboot <- tpm(Pboot, IRT.param=FALSE, control=list(optimizer="nlminb"))
          ltmPboot <- tpm(Qboot, IRT.param=FALSE, control=list(optimizer="nlminb"))
          bootsee[i,] <- irtoseeq(ltmPboot, ltmQboot, x, y, a, qpoints, N, M, model, design, kernel, slog, bunif, KPEN, wpen, linear, altopt, h=h, hlin=hlin)
        }
      }
      
      cdfxP <- cdf(rP, hxP, meanx, varx, kernel, slog, bunif)
      cdfyQ <- cdf(sQ, hyQ, meany, vary, kernel, slog, bunif)
      cdfaP <- cdf(tP, haP, meanaP, varaP, kernel, slog, bunif)
      cdfaQ <- cdf(tQ, haQ, meanaQ, varaQ, kernel, slog, bunif)
      eAx<-eqinvCE(cdfxP, tP, x, a, varaP, meanaP, haP, kernel, slog, bunif)		  #Linked scores from X to A on P
      cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a, kernel, slog, bunif)		  #cdf of the linked values on Q
      eYCEeAx<-eqinvCE(cdfeAxQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)	#from eAx on Q to Y
      eYa <- eqinvCE(cdfaQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)
      PREAx <- PREp(eAx, a, rP, tP)
      PREYa <- PREp(eYa, y, tQ, sQ)
      PRE <- data.frame(PREAx, PREYa)
      h <- data.frame(hxP, hyQ, haP, haQ, hxPlin, hyQlin, haPlin, haQlin)
      equating <- data.frame(eqYx=eYCEeAx, SEEYx=apply(bootsee, 2, sd))
      out<-new("keout", Pobs=P, Qobs=Q, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="IRT-OSE CE", equating=equating, irt=list(ltmP=ltmP, ltmQ=ltmQ), see=see, replications=replications)
      return(out)
      
    } else{
      if(model=="1pl"){
        #irtx <- probpl(qpoints, bx, model)
        #irtaP <- probpl(qpoints, baP, model)
        #irty <- probpl(qpoints, by, model)
        #irtaQ <- probpl(qpoints, baQ, model)
        pderX <- pderpl(irtx, qpoints, bx, model)
        pderAP <- pderpl(irtaP, qpoints, baP, model)
        pderY <- pderpl(irty, qpoints, by, model)
        pderAQ <- pderpl(irtaQ, qpoints, baQ, model)
      }
      if(model=="2pl"){
        #irtx <- probpl(qpoints, bx, model, a=ax)
        #irtaP <- probpl(qpoints, baP, model, a=aaP)
        #irty <- probpl(qpoints, by, model, a=ay)
        #irtaQ <- probpl(qpoints, baQ, model, a=aaQ)
        pderX <- pderpl(irtx, qpoints, bx, model, a=ax)
        pderAP <- pderpl(irtaP, qpoints, baP, model, a=aaP)
        pderY <- pderpl(irty, qpoints, by, model, a=ay)
        pderAQ <- pderpl(irtaQ, qpoints, baQ, model, a=aaQ)
        covalphaP <- vcov.ltm(ltmP, robust=robust)
        covalphaQ <- vcov.ltm(ltmQ, robust=robust)
        adjcovalphaP <- adjltm(covalphaP, pars=list(ax=ax, aa=aaP, bxltm=bxltm, baltm=baPltm), design, model="2pl")
        adjcovalphaQ <- adjltm(covalphaQ, pars=list(ax=ay, aa=aaQ, bxltm=byltm, baltm=baQltm), design, model="2pl")
      }
      if(model=="3pl"){
        #irtx <- probpl(qpoints, bx, a=ax, c=cx)
        #irtaP <- probpl(qpoints, baP, a=aaP, c=caP)
        #irty <- probpl(qpoints, by, a=ay, c=cy)
        #irtaQ <- probpl(qpoints, baQ, a=aaQ, c=caQ)
        pderX <- pderpl(irtx, qpoints, bx, a=ax, c=cx)
        pderAP <- pderpl(irtaP, qpoints, baP, a=aaP, c=caP)
        pderY <- pderpl(irty, qpoints, by, a=ay, c=cy)
        pderAQ <- pderpl(irtaQ, qpoints, baQ, a=aaQ, c=caQ)
        covalphaP <- vcov.tpm(ltmP)
        covalphaQ <- vcov.tpm(ltmQ)
        adjcovalphaP <- adjltm(covalphaP, pars=list(ax=ax, aa=aaP, bxltm=bxltm, baltm=baPltm), design, model="3pl")
        adjcovalphaQ <- adjltm(covalphaQ, pars=list(ax=ay, aa=aaQ, bxltm=byltm, baltm=baQltm), design, model="3pl")
      }
    }

    
#    pderltmP <- matrix(0, nrow=2*(length(ax)+length(aaP)), ncol=2*(length(ax)+length(aaP)))
#    pderltmQ <- matrix(0, nrow=2*(length(ay)+length(aaQ)), ncol=2*(length(ay)+length(aaQ)))
    
    #print(bx)
    #print(aaP)
#     
#     for(i in 1:(length(ax))){
#       pderltmP[i,i] <- -1/ax[i]
#     }
#     for(i in (length(ax)+1):(length(ax)+length(aaP))){
#       pderltmP[i,i] <- -1/aaP[i-length(ax)]
#     }
#     
#     for(i in (length(ax)+length(aaP)+1):(2*length(ax)+length(aaP))){
#       pderltmP[i,i-length(aaP)-length(ax)] <- bxltm[i-length(ax)-length(aaP)]/(ax[i-length(ax)-length(aaP)])^2
#       pderltmP[i,i] <- 1
#     }
#     
#     for(i in (2*length(ax)+length(aaP)+1):(2*(length(ax)+length(aaP)))){
#       pderltmP[i,i-length(aaP)-length(ax)] <- baPltm[i-2*length(ax)-length(aaP)]/(aaP[i-2*length(ax)-length(aaP)])^2
#       pderltmP[i,i] <- 1
#     }
#     
#     for(i in 1:(length(ay))){
#       pderltmQ[i,i] <- -1/ay[i]
#     }
#     
#     for(i in (length(ay)+1):(length(ay)+length(aaQ))){
#       pderltmQ[i,i] <- -1/aaQ[i-length(ay)]
#     }
#     
#     for(i in (length(ay)+length(aaQ)+1):(2*length(ay)+length(aaQ))){
#       pderltmQ[i,i-length(ay)-length(aaQ)] <- byltm[i-length(ay)-length(aaQ)]/(ay[i-length(ay)-length(aaQ)])^2
#       pderltmQ[i,i] <- 1
#     }
#     
#     for(i in (2*length(ay)+length(aaQ)+1):(2*(length(ay)+length(aaQ)))){
#       pderltmQ[i,i-length(ay)-length(aaQ)] <- baQltm[i-2*length(ay)-length(aaQ)]/(aaQ[i-2*length(ay)-length(aaQ)])^2
#       pderltmQ[i,i] <- 1
#     }
    
    #print(pderltmP)
    #print(pderltmQ)

    cdfxP <- cdf(rP, hxP, meanx, varx, kernel, slog, bunif)
    cdfyQ <- cdf(sQ, hyQ, meany, vary, kernel, slog, bunif)
    cdfaP <- cdf(tP, haP, meanaP, varaP, kernel, slog, bunif)
    cdfaQ <- cdf(tQ, haQ, meanaQ, varaQ, kernel, slog, bunif)
    
    #Cp <- cmatrixSG(as.vector(P), DMP, N)
    #Cq <- cmatrixSG(as.vector(Q), DMQ, M)
    #Want to link tests X and A (not equal length)
    #Want to find the values of the continuized cdf for A on Q for the "linked" values from X to A on P.
    
    eAx<-eqinvCE(cdfxP, tP, x, a, varaP, meanaP, haP, kernel, slog, bunif)		  #Linked scores from X to A on P
    cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a, kernel, slog, bunif)		  #cdf of the linked values on Q
    eYCEeAx<-eqinvCE(cdfeAxQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)	#from eAx on Q to Y
    
    eAy<-eqinvCE(cdfyQ, tQ, x, a, varaQ, meanaQ, haQ, kernel, slog, bunif)		#from Y to A on Q
    cdfeAyP<-cdfce(tP, haP, meanaP, varaP, eAy, a, kernel, slog, bunif)		#cdf of the linked values on P
    eXCEeAy<-eqinvCE(cdfeAyP, rP, x, x, varx, meanx, hxP, kernel, slog, bunif)	#from eAy on P to X
    
    FprimeA<-densityeq(rP, hxP, varx, meanx, x, x, kernel, slog, bunif)
    HpprimeA<-densityeq(tP, haP, varaP, meanaP, eAx, a, kernel, slog, bunif)
    HqprimeY<-densityeq(tQ, haQ, varaQ, meanaQ, eAx, a, kernel, slog, bunif)
    GprimeY<-densityeq(sQ, hyQ, vary, meany, eYCEeAx, y, kernel, slog, bunif)
    GprimeA<-densityeq(sQ, hyQ, vary, meany, y, y, kernel, slog, bunif)
    HqprimeA<-densityeq(tQ, haQ, varaQ, meanaQ, eAy, a, kernel, slog, bunif)
    
    if(altopt){
      altFprimeA <- altoptdensity(rP, hxP, varx, meanx, x, x)
      altHpprimeA <- altoptdensity(tP, haP, varaP, meanaP, eAx, a)
      altHqprimeY <- altoptdensity(tQ, haQ, varaQ, meanaQ, eAx, a)
      altGprimeY <- altoptdensity(sQ, hyQ, vary, meany, eYCEeAx, y)
      altGprimeA <- altoptdensity(sQ, hyQ, vary, meany, y, y)
      altHqprimeA <- altoptdensity(tQ, haQ, varaQ, meanaQ, eAy, a)
      dFdrPeA <- dFdr(rP, hxP, varx, meanx, FprimeA, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altFprimeA)
      dHpdtPeA <- dFdr(tP, haP, varaP, meanaP, HpprimeA, eAx, a, kernel, slog, bunif, altopt=altopt, altfprim=altHpprimeA)
      dHqdteY <- dFdr(tQ, haQ, varaQ, meanaQ, HqprimeY, eAx, a, kernel, slog, bunif, altopt=altopt, altfprim=altHqprimeY)
      dGdseY <- dFdr(sQ, hyQ, vary, meany, GprimeY, eYCEeAx, y, kernel, slog, bunif, altopt=altopt, altfprim=altGprimeY)
      dGdsQeA <- dFdr(sQ, hyQ, vary, meany, GprimeA, y, y, kernel, slog, bunif, altopt=altopt, altfprim=altGprimeA)
      dHqdtPeA <- dFdr(tQ, haQ, varaQ, meanaQ, HqprimeA, eAy, a, kernel, slog, bunif, altopt=altopt, altfprim=altHqprimeA)    
    }
    else{
      dFdrPeA<-dFdr(rP, hxP, varx, meanx, FprimeA, x, x, kernel, slog, bunif)
      dHpdtPeA<-dFdr(tP, haP, varaP, meanaP, HpprimeA, eAx, a, kernel, slog, bunif)
      dHqdteY<-dFdr(tQ, haQ, varaQ, meanaQ, HqprimeY, eAx, a, kernel, slog, bunif) #check
      dGdseY<-dFdr(sQ, hyQ, vary, meany, GprimeY, eYCEeAx, y, kernel, slog, bunif)
      dGdsQeA<-dFdr(sQ, hyQ, vary, meany, GprimeA, y, y, kernel, slog, bunif)
      dHqdtPeA<-dFdr(tQ, haQ, varaQ, meanaQ, HqprimeA, eAy, a, kernel, slog, bunif)
    }
    
    JeA <- matrix(0, nrow=length(x), ncol=(length(x)+length(a)))
    JeA[,1:length(x)] <- dFdrPeA
    JeA[,(length(x)+1):(length(x)+length(a))] <- (-dHpdtPeA)
    JeA <- (HqprimeY/GprimeY)*(1/HpprimeA)*JeA
    
    JeY <- matrix(0, nrow=length(x), ncol=(length(y)+length(a)))
    JeY[,1:(length(y))] <- (-dGdseY)
    JeY[,(length(y)+1):(length(y)+length(a))] <- dHqdteY
    JeY <- (1/GprimeY)*JeY
    
    JeYCE <- matrix(0, nrow=length(x), ncol=(length(x)+2*length(a)+length(y)))
    JeYCE[,1:(length(x)+length(a))] <- JeA
    JeYCE[,(length(x)+length(a)+1):(length(x)+2*length(a)+length(y))] <- JeY
    
    eYa <- eqinvCE(cdfaQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)
    PREAx <- PREp(eAx, a, rP, tP)
    PREYa <- PREp(eYa, y, tQ, sQ)
    PRE <- data.frame(PREAx, PREYa)
    h <- data.frame(hxP, hyQ, haP, haQ, hxPlin, hyQlin, haPlin, haQlin)
    
    #print(pderltmP)
    #print(sqrt(diag(adjcovalphaP)))
    #print(sqrt(diag(adjcovalphaQ)))
    cholalphaPQ <- matrix(0, nrow=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]), ncol=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]))
    cholalphaPQ[1:dim(adjcovalphaP)[1],1:dim(adjcovalphaP)[1]] <- chol(adjcovalphaP)
    cholalphaPQ[(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]),(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1])]<- chol(adjcovalphaQ)
    #covmatPQ <- matrix(0, nrow=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]), ncol=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]))
    #covmatPQ[1:dim(adjcovalphaP)[1],1:dim(adjcovalphaP)[1]] <- adjcovalphaP
    #covmatPQ[(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]),(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1])] <- adjcovalphaQ
    
    kX <- length(x)-1
    kA <-  length(a)-1
    kY <- length(y)-1
    kA <- length(a)-1
    
    #Put the partial ders in the right place, to match with the order of item pars in the IRT model (2pl: first b par, then a par; 3pl: first c par, then b par, last a par)
    if(model=="2pl"){
      Jalpha <- matrix(0, nrow=2*(kY+2*kA+kX), ncol=(kX+2*kA+kY+4))
      Jalpha[1:kX, 1:(kX+1)] <- pderX[1:kX,]
      Jalpha[(kX+1):(kX+kA),(kX+2):(kX+kA+2)] <- pderAP[1:kA,]
      Jalpha[(kX+kA+1):(2*kX+kA),1:(kX+1)] <- pderX[(kX+1):(2*kX),]
      Jalpha[(2*kX+kA+1):(2*kX+2*kA),(kX+2):(kX+kA+2)] <- pderAP[(kA+1):(2*kA),]
      Jalpha[(2*kX+2*kA+1):(2*kX+2*kA+kY),(kX+kA+3):(kX+kA+kY+3)] <- pderY[1:kY,]
      Jalpha[(2*kX+2*kA+kY+1):(2*kX+3*kA+kY),(kX+kA+kY+4):(kX+2*kA+kY+4)] <- pderAQ[1:kA,]
      Jalpha[(2*kX+3*kA+kY+1):(2*kX+3*kA+2*kY),(kX+kA+3):(kX+kA+kY+3)] <- pderY[(kY+1):(2*kY),]
      Jalpha[(2*kX+3*kA+2*kY+1):(2*kX+4*kA+2*kY),(kX+kA+kY+4):(kX+2*kA+kY+4)] <- pderAQ[(kA+1):(2*kA),]
    }
    
    #check order of 3pl pders, fix, be done
    if(model=="3pl"){
      Jalpha <- matrix(0, nrow=3*(kY+2*kA+kX), ncol=(kX+2*kA+kY+4))
      Jalpha[1:kX, 1:(kX+1)] <- pderX[1:kX,]
      Jalpha[(kX+1):(kX+kA),(kX+2):(kX+kA+2)] <- pderAP[1:kA,]
      Jalpha[(kX+kA+1):(2*kX+kA),1:(kX+1)] <- pderX[(kX+1):(2*kX),]
      Jalpha[(2*kX+kA+1):(2*kX+2*kA),(kX+2):(kX+kA+2)] <- pderAP[(kA+1):(2*kA),]
      Jalpha[(2*kX+2*kA+1):(3*kX+2*kA),1:(kX+1)] <- pderX[(2*kX+1):(3*kX),]
      Jalpha[(3*kX+2*kA+1):(3*kX+3*kA),(kX+2):(kX+kA+2)] <- pderAP[(2*kA+1):(3*kA),]
    
      Jalpha[(3*kX+3*kA+1):(3*kX+3*kA+kY),(kX+kA+3):(kX+kA+kY+3)] <- pderY[1:kY,]
      Jalpha[(3*kX+3*kA+kY+1):(3*kX+4*kA+kY),(kX+kA+kY+4):(kX+2*kA+kY+4)] <- pderAQ[1:kA,]
      Jalpha[(3*kX+4*kA+kY+1):(3*kX+4*kA+2*kY),(kX+kA+3):(kX+kA+kY+3)] <- pderY[(kY+1):(2*kY),]
      Jalpha[(3*kX+4*kA+2*kY+1):(3*kX+5*kA+2*kY),(kX+kA+kY+4):(kX+2*kA+kY+4)] <- pderAQ[(kA+1):(2*kA),]
      Jalpha[(3*kX+5*kA+2*kY+1):(3*kX+5*kA+3*kY),(kX+kA+3):(kX+kA+kY+3)] <- pderY[(2*kY+1):(3*kY),]
      Jalpha[(3*kX+5*kA+3*kY+1):(3*kX+6*kA+3*kY),(kX+kA+kY+4):(kX+2*kA+kY+4)] <- pderAQ[(2*kA+1):(3*kA),]
    }
    #print(dim(JeYCE))
    #print(dim(Jalpha))
    #print(JeYCE)
    #print(Jalpha)
    #print(c(rP, tP, sQ, tQ))
    #print(sqrt(diag(t(Jalpha) %*% t(cholalphaPQ) %*% cholalphaPQ %*% Jalpha)))
    #SEEYx <- numeric(length(x))
    #SEEYxmat <- t(JeYCE) %*% Jalpha %*% cholalphaPQ %*% t
    pdereqYx <- JeYCE %*% t(Jalpha)
    coveqYx <- pdereqYx %*% t(cholalphaPQ) %*% cholalphaPQ %*% t(pdereqYx)
    #return(eYCEeAx)
    output<-data.frame(eqYx=eYCEeAx, SEEYx=sqrt(diag(coveqYx)))
    #return(list(equating=output, coveqYx=coveqYx)) 
    out<-new("keout", coveqYx=coveqYx, pdereqYx=pdereqYx, Pobs=P, Qobs=Q, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="IRT-OSE CE", equating=output, irt=list(covalphaP=adjcovalphaP, covalphaQ=adjcovalphaQ, ltmP=ltmP, ltmQ=ltmQ), see=see)
    return(out)
    #SEEYxmat <- 
    #SEEYxmat[,1:dim(Up)[2]] <- (HqprimeY/GprimeY)*(1/HpprimeA)*(dFdrPeA%*%Up-dHpdtPeA%*%Vp)                                            #Fill matrix
    #SEEYxmat[,(dim(Up)[2]+1):(dim(Up)[2]+dim(Uq)[2])] <- (1/GprimeY)*(dHqdteY%*%Vq-dGdseY%*%Uq)
    
  }
  if(design=="EG"){
    if(model=="2pl"){
      if("ltm" %in% class(P)){
        bx <- as.numeric(coef.ltm(P)[,1])[1:(length(x)-1)]
        ax <- as.numeric(coef.ltm(P)[,2])[1:(length(x)-1)]
        if(P$IRT.param){
          bxltm <- -ax*bx
        } else{
          bxltm <- bx
          bx <- -bxltm/ax
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(x)-1) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          bx <- as.numeric(coef.ltm(ltmP)[,1])[1:(length(x)-1)]
          ax <- as.numeric(coef.ltm(ltmP)[,2])[1:(length(x)-1)]
          bxltm <- bx
          bx <- -bxltm/ax
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("ltm" %in% class(Q)){
        by <- as.numeric(coef.ltm(Q)[,1])[1:(length(y)-1)]
        ay <- as.numeric(coef.ltm(Q)[,2])[1:(length(y)-1)]
        if(Q$IRT.param){
          byltm <- -ay*by
        } else{
          byltm <- by
          by <- -byltm/ay
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(y)-1) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- ltm(Q ~ z1, IRT.param=FALSE)
          by <- as.numeric(coef.ltm(ltmQ)[,1])[1:(length(y)-1)]
          ay <- as.numeric(coef.ltm(ltmQ)[,2])[1:(length(y)-1)]
          byltm <- by
          by <- -byltm/ay
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      if(missing(qpoints))
        qpoints <- -ltmP$GH$Z[,2]
      irtx <- probpl(qpoints, bx, model, a=ax)
      irty <- probpl(qpoints, by, model, a=ay)
    }
    
    if(model=="3pl"){
      if("tpm" %in% class(P)){
        cx <- as.numeric(coef.tpm(P)[,1])[1:(length(x)-1)]
        bx <- as.numeric(coef.tpm(P)[,2])[1:(length(x)-1)]
        ax <- as.numeric(coef.tpm(P)[,3])[1:(length(x)-1)]
        if(P$IRT.param){
          bxltm <- -ax*bx
        } else{
          bxltm <- bx
          bx <- -bxltm/ax
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(x)-1) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          cx <- as.numeric(coef.tpm(ltmP)[,1])[1:(length(x)-1)]
          bx <- as.numeric(coef.tpm(ltmP)[,2])[1:(length(x)-1)]
          ax <- as.numeric(coef.tpm(ltmP)[,3])[1:(length(x)-1)]
          bxltm <- bx
          bx <- -bxltm/ax
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("tpm" %in% class(Q)){
        cy <- as.numeric(coef.tpm(Q)[,1])[1:(length(y)-1)]
        by <- as.numeric(coef.tpm(Q)[,2])[1:(length(y)-1)]
        ay <- as.numeric(coef.tpm(Q)[,3])[1:(length(y)-1)]
        if(Q$IRT.param){
          byltm <- -ay*by
        } else{
          byltm <- by
          by <- -byltm/ay
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(y)-1) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- tpm(Q, IRT.param=FALSE)
          cy <- as.numeric(coef.tpm(ltmQ)[,1])[1:(length(y)-1)]
          by <- as.numeric(coef.tpm(ltmQ)[,2])[1:(length(y)-1)]
          ay <- as.numeric(coef.tpm(ltmQ)[,3])[1:(length(y)-1)]
          byltm <- by
          by <- -byltm/ay
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      if(missing(qpoints))
        qpoints <- -ltmP$GH$Z[,2]
      irtx <- probpl(qpoints, bx, a=ax, c=cx)
      irty <- probpl(qpoints, by, a=ay, c=cy)
    }
     if(see=="bootstrap"){
      if(model=="2pl"){
        bootsee <- matrix(0, nrow=replications, ncol=length(x))
        for(i in 1:replications){
          Pboot <- P[sample(1:N, replace=TRUE),]
          Qboot <- Q[sample(1:M, replace=TRUE),]
          ltmQboot <- ltm(Pboot ~ z1, IRT.param=FALSE)
          ltmPboot <- ltm(Qboot ~ z1, IRT.param=FALSE)
          bootsee[i,] <- irtoseeq(ltmPboot, ltmQboot, x, y, a, qpoints, N, M, model, design, kernel, slog, bunif, KPEN, wpen, linear, altopt, h=h, hlin=hlin)
        }
      }
      if(model=="3pl"){
        bootsee <- matrix(0, nrow=replications, ncol=length(x))
        for(i in 1:replications){
          Pboot <- P[sample(1:N, replace=TRUE),]
          Qboot <- Q[sample(1:M, replace=TRUE),]
          ltmQboot <- tpm(Pboot, IRT.param=FALSE, control=list(optimizer="nlminb"))
          ltmPboot <- tpm(Qboot, IRT.param=FALSE, control=list(optimizer="nlminb"))
          bootsee[i,] <- irtoseeq(ltmPboot, ltmQboot, x, y, a, qpoints, N, M, model, design, kernel, slog, bunif, KPEN, wpen, linear, altopt, h=h, hlin=hlin)
        }
      }
#       
#       cdfxP <- cdf(rP, hxP, meanx, varx, kernel, slog, bunif)
#       cdfyQ <- cdf(sQ, hyQ, meany, vary, kernel, slog, bunif)
#       cdfaP <- cdf(tP, haP, meanaP, varaP, kernel, slog, bunif)
#       cdfaQ <- cdf(tQ, haQ, meanaQ, varaQ, kernel, slog, bunif)
#       eAx<-eqinvCE(cdfxP, tP, x, a, varaP, meanaP, haP, kernel, slog, bunif)      #Linked scores from X to A on P
#       cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a, kernel, slog, bunif)		  #cdf of the linked values on Q
#       eYCEeAx<-eqinvCE(cdfeAxQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)	#from eAx on Q to Y
#       eYa <- eqinvCE(cdfaQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)
#       PREAx <- PREp(eAx, a, rP, tP)
#       PREYa <- PREp(eYa, y, tQ, sQ)
#       PRE <- data.frame(PREAx, PREYa)
#       h <- data.frame(hxP, hyQ, haP, haQ, hxPlin, hyQlin, haPlin, haQlin)
#       equating <- data.frame(eqYx=eYCEeAx, SEEYx=apply(bootsee, 2, sd))
#       out<-new("keout", Pobs=P, Qobs=Q, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="IRT-OSE CE", equating=equating, irt=list(ltmP=ltmP, ltmQ=ltmQ), see=see, replications=replications)
#       return(out)
      r <- LordWW(irtx, qpoints)
      s <- LordWW(irty, qpoints)
      if(kernel=="uniform"){
        ulimit<-(1/(2*bunif*(1-0.61803)))
        KPEN<-0
      } else ulimit<-4
      
      meanx <- x%*%r
      meany <- y%*%s
      
      varx <- (N/(N-1))*(x^2%*%r-meanx^2)
      vary <- (M/(M-1))*(y^2%*%s-meany^2)
      
      if(linear){
        if(hlin$hxlin==0)
          hx<-as.numeric(1000*sqrt(varx))
        else
          hx<-hlin$hxlin
        if(hlin$hylin==0)
          hy<-as.numeric(1000*sqrt(vary))
        else
          hy<-hlin$hylin
      } else{
        if(h$hx==0){
          if(altopt){
            hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
          } else{
            if(KPEN==0){
              hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
            } else{
              hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
              hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
            }
          }
        }
        if(h$hy==0){
          if(altopt){
            hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
          } else{
            if(KPEN==0){
              hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
            } else{
              hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
              hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
            }
          }
        }
      }
      
      cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)       						#Continuize the estimated cdf:s for X and Y.
      cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
      eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)    						#Calculate the equated score values.
      
      PREYx<-PREp(eqYx, y, r, s)
      PRE<-data.frame(PREYx)
      h<-data.frame(hx, hy)
      output <- data.frame(eqYx=eqYx, SEEYx=apply(bootsee, 2, sd))
      out <- new("keout", Pobs=P, Qobs=Q, scores=list(X=data.frame(x, r=r, cdfx=cdfx), Y=data.frame(y, s=s, cdfy=cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="IRT-OSE EG", equating=output, irt=list(ltmP=ltmP, ltmQ=ltmQ), see=see, replications=replications)
      return(out)
#       
     } else{
      if(model=="1pl"){
        pderX <- pderpl(irtx, qpoints, bx, model)
        pderY <- pderpl(irty, qpoints, by, model)
      }
      if(model=="2pl"){
        pderX <- pderpl(irtx, qpoints, bx, model, a=ax)
        pderY <- pderpl(irty, qpoints, by, model, a=ay)
        covalphaP <- vcov.ltm(ltmP, robust=robust)
        covalphaQ <- vcov.ltm(ltmQ, robust=robust)
        adjcovalphaP <- adjltm(covalphaP, pars=list(ax=ax, bxltm=bxltm), design, model="2pl")
        adjcovalphaQ <- adjltm(covalphaQ, pars=list(ax=ay, bxltm=byltm), design, model="2pl")
      }
      if(model=="3pl"){
        pderX <- pderpl(irtx, qpoints, bx, a=ax, c=cx)
        pderY <- pderpl(irty, qpoints, by, a=ay, c=cy)
        covalphaP <- vcov.tpm(ltmP)
        covalphaQ <- vcov.tpm(ltmQ)
        adjcovalphaP <- adjltm(covalphaP, pars=list(ax=ax, bxltm=bxltm), design, model="3pl")
        adjcovalphaQ <- adjltm(covalphaQ, pars=list(ax=ay, bxltm=byltm), design, model="3pl")
      }
    }
    if(model %in% c("1pl", "2pl", "3pl")){
      r <- LordWW(irtx, qpoints)
      s <- LordWW(irty, qpoints)
    }
    
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    } else ulimit<-4
    
    meanx <- x%*%r
    meany <- y%*%s
    
    varx <- (N/(N-1))*(x^2%*%r-meanx^2)
    vary <- (M/(M-1))*(y^2%*%s-meany^2)
    
    if(linear){
      if(hlin$hxlin==0)
        hx<-as.numeric(1000*sqrt(varx))
      else
        hx<-hlin$hxlin
      if(hlin$hylin==0)
        hy<-as.numeric(1000*sqrt(vary))
      else
        hy<-hlin$hylin
    } else{
      if(h$hx==0){
        if(altopt){
          hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
        } else{
            if(KPEN==0){
              hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
            } else{
            hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      } else{
        hx <- h$hx
      }
      if(h$hy==0){
        if(altopt){
          hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
        } else{
          if(KPEN==0){
            hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          } else{
            hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      } else{
        hy <- h$hy
      }
    }
    
    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)     							#Continuize the estimated cdf:s for X and Y.
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)  							#Calculate the equated score values.
    
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    #if(irtx!=0 && irty!=0){
    #  irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    #}
    
    gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)							#G' for eY
    fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)							#F' for eY
    #gprimeX<-densityeq(s, hy, vary, meany, y, y)
    #fprimeX<-densityeq(r, hx, varx, meanx, eqXy, x)
    
    if(altopt){
      altfprim <- altoptdensity(r, hx, varx, meanx, x, x)
      altgprim <- altoptdensity(s, hy, vary, meanx, eqYx, y)
    }
    if(altopt){
      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altfprim)						#Matrices of derivatives. JxJ.
      dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif, altopt=altopt, altfprim=altgprim)	
    } else{
      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
      dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)						#KxK
    }
    
    JeY <- matrix(0, nrow=length(x), ncol=(length(x)+length(y)))
    JeY[,1:(length(x))] <- dFdreYEG
    JeY[,(length(x)+1):(length(x)+length(y))] <- -dGdseYEG
    JeY <- (1/gprimeY)*JeY
    cholalphaPQ <- matrix(0, nrow=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]), ncol=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]))
    cholalphaPQ[1:dim(adjcovalphaP)[1],1:dim(adjcovalphaP)[1]] <- chol(adjcovalphaP)
    cholalphaPQ[(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]),(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1])]<- chol(adjcovalphaQ)
    
    kX <- length(x)-1
    kY <- length(y)-1
    
    if(model=="2pl"){
      Jalpha <- matrix(0, nrow=2*(kY+kX), ncol=(kX+kY+2))
      Jalpha[1:(2*kX), 1:(kX+1)] <- pderX
      Jalpha[(2*kX+1):(2*kX+2*kY),(kX+2):(kX+kY+2)] <- pderY    
    }
    if(model=="3pl"){
      Jalpha <- matrix(0, nrow=3*(kY+kX), ncol=(kX+kY+2))
      Jalpha[1:(3*kX), 1:(kX+1)] <- pderX
      Jalpha[(3*kX+1):(3*kX+3*kY),(kX+2):(kX+kY+2)] <- pderY
    }
    
    pdereqYx <- JeY %*% t(Jalpha)
    coveqYx <- pdereqYx %*% t(cholalphaPQ) %*% cholalphaPQ %*% t(pdereqYx)
    #return(eYCEeAx)
    output <- data.frame(eqYx=eqYx, SEEYx=sqrt(diag(coveqYx)))
    #return(list(equating=output, coveqYx=coveqYx)) 
    out <- new("keout", coveqYx=coveqYx, pdereqYx=pdereqYx, Pobs=P, Qobs=Q, scores=list(X=data.frame(x, r=r, cdfx=cdfx), Y=data.frame(y, s=s, cdfy=cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="IRT-OSE EG", equating=output, irt=list(covalphaP=adjcovalphaP, covalphaQ=adjcovalphaQ, ltmP=ltmP, ltmQ=ltmQ), see=see)
    return(out)
    
  } 
  if(design=="PSE"){
    if(model=="2pl"){
      if("ltm" %in% class(P)){
        bx <- as.numeric(coef.ltm(P)[,1])[1:(length(x)-1)]
        baP <- as.numeric(coef.ltm(P)[,1])[(length(x)):((length(x)+length(a)-2))]
        ax <- as.numeric(coef.ltm(P)[,2])[1:(length(x)-1)]
        aaP <- as.numeric(coef.ltm(P)[,2])[(length(x)):((length(x)+length(a)-2))]
        if(P$IRT.param){
          bxltm <- -ax*bx
          baPltm <- -aaP*baP
        } else{
          bxltm <- bx
          baPltm <- baP
          bx <- -bxltm/ax
          baP <- -baPltm/aaP
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(a)+length(x)-2) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          bx <- as.numeric(coef.ltm(ltmP)[,1])[1:(length(x)-1)]
          baP <- as.numeric(coef.ltm(ltmP)[,1])[(length(x)):((length(x)+length(a)-2))]
          ax <- as.numeric(coef.ltm(ltmP)[,2])[1:(length(x)-1)]
          aaP <- as.numeric(coef.ltm(ltmP)[,2])[(length(x)):((length(x)+length(a)-2))]
          bxltm <- bx
          baPltm <- baP
          bx <- -bxltm/ax
          baP <- -baPltm/aaP
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("ltm" %in% class(Q)){
        by <- as.numeric(coef.ltm(Q)[,1])[1:(length(y)-1)]
        baQ <-  as.numeric(coef.ltm(Q)[,1])[(length(y)):((length(y)+length(a)-2))]
        ay <- as.numeric(coef.ltm(Q)[,2])[1:(length(y)-1)]
        aaQ <- as.numeric(coef.ltm(Q)[,2])[(length(y)):((length(y)+length(a)-2))]
        if(Q$IRT.param){
          byltm <- -ay*by
          baQltm <- -aaQ*baQ
        } else{
          byltm <- by
          baQltm <- baQ
          by <- -byltm/ay
          baQ <- -baQltm/aaQ
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(a)+length(y)-2) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- ltm(Q ~ z1, IRT.param=FALSE)
          by <- as.numeric(coef.ltm(ltmQ)[,1])[1:(length(y)-1)]
          baQ <-  as.numeric(coef.ltm(ltmQ)[,1])[(length(y)):((length(y)+length(a)-2))]
          ay <- as.numeric(coef.ltm(ltmQ)[,2])[1:(length(y)-1)]
          aaQ <- as.numeric(coef.ltm(ltmQ)[,2])[(length(y)):((length(y)+length(a)-2))]
          byltm <- by
          baQltm <- baQ
          by <- -byltm/ay
          baQ <- -baQltm/aaQ
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
    }
    
    if(model=="3pl"){
      if("tpm" %in% class(P)){
        cx <- as.numeric(coef.tpm(P)[,1])[1:(length(x)-1)]
        caP <- as.numeric(coef.tpm(P)[,1])[(length(x)):((length(x)+length(a)-2))]
        bx <- as.numeric(coef.tpm(P)[,2])[1:(length(x)-1)]
        baP <-  as.numeric(coef.tpm(P)[,2])[(length(x)):((length(x)+length(a)-2))]
        ax <- as.numeric(coef.tpm(P)[,3])[1:(length(x)-1)]
        aaP <- as.numeric(coef.tpm(P)[,3])[(length(x)):((length(x)+length(a)-2))]
        if(P$IRT.param){
          bxltm <- -ax*bx
          baPltm <- -aaP*baP
        } else{
          bxltm <- bx
          baPltm <- baP
          bx <- -bxltm/ax
          baP <- -baPltm/aaP
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(a)+length(x)-2) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          cx <- as.numeric(coef.tpm(ltmP)[,1])[1:(length(x)-1)]
          caP <- as.numeric(coef.tpm(ltmP)[,1])[(length(x)):((length(x)+length(a)-2))]
          bx <- as.numeric(coef.tpm(ltmP)[,2])[1:(length(x)-1)]
          baP <- as.numeric(coef.tpm(ltmP)[,2])[(length(x)):((length(x)+length(a)-2))]
          ax <- as.numeric(coef.tpm(ltmP)[,3])[1:(length(x)-1)]
          aaP <- as.numeric(coef.tpm(ltmP)[,3])[(length(x)):((length(x)+length(a)-2))]
          bxltm <- bx
          baPltm <- baP
          bx <- -bxltm/ax
          baP <- -baPltm/aaP
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("tpm" %in% class(Q)){
        cy <- as.numeric(coef.tpm(Q)[,1])[1:(length(y)-1)]
        caQ <- as.numeric(coef.tpm(Q)[,1])[(length(y)):((length(y)+length(a)-2))]
        by <- as.numeric(coef.tpm(Q)[,2])[1:(length(y)-1)]
        baQ <-  as.numeric(coef.tpm(Q)[,2])[(length(y)):((length(y)+length(a)-2))]
        ay <- as.numeric(coef.tpm(Q)[,3])[1:(length(y)-1)]
        aaQ <- as.numeric(coef.tpm(Q)[,3])[(length(y)):((length(y)+length(a)-2))]
        if(Q$IRT.param){
          byltm <- -ay*by
          baQltm <- -aaQ*baQ
        } else{
          byltm <- by
          baQltm <- baQ
          by <- -byltm/ay
          baQ <- -baQltm/aaQ
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(a)+length(y)-2) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- tpm(Q, IRT.param=FALSE)
          cy <- as.numeric(coef.tpm(ltmQ)[,1])[1:(length(y)-1)]
          caQ <- as.numeric(coef.tpm(ltmQ)[,1])[(length(y)):((length(y)+length(a)-2))]
          by <- as.numeric(coef.tpm(ltmQ)[,2])[1:(length(y)-1)]
          baQ <-  as.numeric(coef.tpm(ltmQ)[,2])[(length(y)):((length(y)+length(a)-2))]
          ay <- as.numeric(coef.tpm(ltmQ)[,3])[1:(length(y)-1)]
          aaQ <- as.numeric(coef.tpm(ltmQ)[,3])[(length(y)):((length(y)+length(a)-2))]
          byltm <- by
          baQltm <- baQ
          by <- -byltm/ay
          baQ <- -baQltm/aaQ
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
    }
    
    if(missing(qpoints))
      qpoints <- -ltmP$GH$Z[,2]
    
    if(model=="1pl"){
      irtpars <- list(bx=bx, by=by, baP=baP, baQ=baQ)
      irtrP <- probpl(qpoints, bx, model)
      irtrQ <- probpl(betaA*qpoints+betaB, bx, model)
      irtsQ <- probpl(qpoints, by, model)
      irtsP <- probpl(betaA*qpoints+betaB, by, model)
      #pderX <- pderpl(irtx, qpoints, bx, model)
      #pderAP <- pderpl(irtaP, qpoints, baP, model)
      #pderY <- pderpl(irty, qpoints, by, model)
      #pderAQ <- pderpl(irtaQ, qpoints, baQ, model)
    }
    if(model=="2pl"){
      ltmPcf <- ltmP$coefficients
      ltmPcov <- vcov.ltm(ltmP, robust=robust)
      ltmQcf <- ltmQ$coefficients
      ltmQcov <- vcov.ltm(ltmQ, robust=robust)
      
      dimnames(ltmPcf)[[1]] <- c(sprintf("X%03d", 1:(length(x)-1)), sprintf("A%03d", 1:(length(a)-1)))
      dimnames(ltmQcf)[[1]] <- c(sprintf("Y%03d", 1:(length(y)-1)), sprintf("A%03d", 1:(length(a)-1)))
      avoidprint <- capture.output({modPQ <- modIRT(coef=list(test1=ltmPcf, test2=ltmQcf), var=list(test1=ltmPcov, test2=ltmQcov), names=paste("test", 1:2, sep=""), ltparam=TRUE)})
      eqc <- direc(modPQ[2], modPQ[1], method=eqcoef)
      
      betaA <- eqc$A
      betaB <- eqc$B
      
      irtpars <- list(ax=ax, ay=ay, aaP=aaP, aaQ=aaQ, bx=bx, by=by, baP=baP, baQ=baQ)
      irtrP <- probpl(qpoints, bx, model, a=ax)
      irtrQ <- probpl(betaA*qpoints+betaB, bx, model, a=ax)
      irtsQ <- probpl(qpoints, by, model, a=ay)
      irtsP <- probpl((qpoints-betaB)/betaA, by, model, a=ay)
      #irtaQ <- probpl(qpoints, baQ, model, a=aaQ)
      #pderX <- pderpl(irtx, qpoints, bx, model, a=ax)
      #pderAP <- pderpl(irtaP, qpoints, baP, model, a=aaP)
      #pderY <- pderpl(irty, qpoints, by, model, a=ay)
      #pderAQ <- pderpl(irtaQ, qpoints, baQ, model, a=aaQ)
      
      covalphaP <- ltmPcov
      covalphaQ <- ltmQcov
      adjcovalphaP <- adjltm(covalphaP, pars=list(ax=ax, aa=aaP, bxltm=bxltm, baltm=baPltm), "PSE", model="2pl")
      adjcovalphaQ <- adjltm(covalphaQ, pars=list(ax=ay, aa=aaQ, bxltm=byltm, baltm=baQltm), "PSE", model="2pl")
      
    }
    if(model=="3pl"){
      ltmPcf <- ltmP$coefficients
      ltmPcov <- vcov(ltmP)
      ltmQcf <- ltmQ$coefficients
      ltmQcov <- vcov(ltmQ)
      
      dimnames(ltmPcf)[[1]] <- c(sprintf("X%03d", 1:(length(x)-1)), sprintf("A%03d", 1:(length(a)-1)))
      dimnames(ltmQcf)[[1]] <- c(sprintf("Y%03d", 1:(length(y)-1)), sprintf("A%03d", 1:(length(a)-1)))
      avoidprint <- capture.output({modPQ <- modIRT(coef=list(test1=ltmPcf, test2=ltmQcf), var=list(test1=ltmPcov, test2=ltmQcov), names=paste("test", 1:2, sep=""), ltparam=TRUE, lparam=TRUE)})
      eqc <- direc(modPQ[2], modPQ[1], method=eqcoef)
      
      betaA <- eqc$A
      betaB <- eqc$B
      
      irtpars <- list(ax=ax, ay=ay, aaP=aaP, aaQ=aaQ, bx=bx, by=by, baP=baP, baQ=baQ, cx=cx, cy=cy, caP=caP, caQ=caQ)
      irtrP <- probpl(qpoints, bx, a=ax, c=cx)
      irtrQ <- probpl(betaA*qpoints+betaB, bx, a=ax, c=cx)
      irtsQ <- probpl(qpoints, by, a=ay, c=cy)
      irtsP <- probpl((qpoints-betaB)/betaA, by, a=ay, c=cy)
      
      covalphaP <- vcov.tpm(ltmP)
      covalphaQ <- vcov.tpm(ltmQ)
      adjcovalphaP <- adjltm(covalphaP, pars=list(ax=ax, aa=aaP, bxltm=bxltm, baltm=baPltm), design, model="3pl")
      adjcovalphaQ <- adjltm(covalphaQ, pars=list(ax=ay, aa=aaQ, bxltm=byltm, baltm=baQltm), design, model="3pl")
      #irtaQ <- probpl(qpoints, baQ, a=aaQ, c=caQ)
      #pderX <- pderpl(irtx, qpoints, bx, a=ax, c=cx)
      #pderAP <- pderpl(irtaP, qpoints, baP, a=aaP, c=caP)
      #pderY <- pderpl(irty, qpoints, by, a=ay, c=cy)
      #pderAQ <- pderpl(irtaQ, qpoints, baQ, a=aaQ, c=caQ)
    }
    
    #irtx <- probpl(qpoints, bx, model, a=ax)
    #irtaP <- probpl(qpoints, baP, model, a=aaP)
    #irty <- probpl(qpoints, by, model, a=ay)
    #irtaQ <- probpl(qpoints, baQ, model, a=aaQ)
    
    rP <- LordWW(irtrP, qpoints)
    rQ <- LordWW(irtrQ, qpoints)
    sQ <- LordWW(irtsQ, qpoints)
    sP <- LordWW(irtsP, qpoints)
    
    r <- wS*rP+(1-wS)*rQ
    s <- wS*sP+(1-wS)*sQ

    
    pdermats <- pderrspse(irtpars, x, y, a, irtrP, irtrQ, irtsP, irtsQ, qpoints, eqc, wS, model=model)
    
    
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    } else ulimit<-4
    
    meanx <- x%*%r
    meany <- y%*%s
    
    varx <- (N/(N-1))*(x^2%*%r-meanx^2)
    vary <- (M/(M-1))*(y^2%*%s-meany^2)
    
    if(linear){
      if(hlin$hxlin==0)
        hx<-as.numeric(1000*sqrt(varx))
      else
        hx<-hlin$hxlin
      if(hlin$hylin==0)
        hy<-as.numeric(1000*sqrt(vary))
      else
        hy<-hlin$hylin
    } else{
      if(h$hx==0){
        if(altopt){
          hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
        } else{
          if(KPEN==0){
            hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          } else{
            hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      } else{
        hx <- h$hx
      }
      if(h$hy==0){
        if(altopt){
          hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
        } else{
          if(KPEN==0){
            hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          } else{
            hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      } else{
        hy <- h$hy
      }
    }
    
    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)       						#Continuize the estimated cdf:s for X and Y.
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)  							#Calculate the equated score values.
    
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    #if(irtx!=0 && irty!=0){
    #  irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    #}
    
    gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)							#G' for eY
    fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)							#F' for eY
    #gprimeX<-densityeq(s, hy, vary, meany, y, y)
    #fprimeX<-densityeq(r, hx, varx, meanx, eqXy, x)
    
    if(altopt){
      altfprim <- altoptdensity(r, hx, varx, meanx, x, x)
      altgprim <- altoptdensity(s, hy, vary, meanx, eqYx, y)
    }
    if(altopt){
      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altfprim)						#Matrices of derivatives. JxJ.
      dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif, altopt=altopt, altfprim=altgprim)	
    } else{
      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
      dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)						#KxK
    }
    
    
    JeY <- matrix(0, nrow=length(x), ncol=(length(x)+length(y)))
    JeY[,1:(length(x))] <- dFdreYEG
    JeY[,(length(x)+1):(length(x)+length(y))] <- -dGdseYEG
    JeY <- (1/gprimeY)*JeY
    #print(JeY)
    cholalphaPQ <- matrix(0, nrow=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]), ncol=(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]))
    cholalphaPQ[1:dim(adjcovalphaP)[1],1:dim(adjcovalphaP)[1]] <- chol(adjcovalphaP)
    cholalphaPQ[(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1]),(dim(adjcovalphaP)[1]+1):(dim(adjcovalphaP)[1]+dim(adjcovalphaQ)[1])]<- chol(adjcovalphaQ)
    
    #print(pdermats$paXaYbetaAB)
    #print(pdermats$prPrQsPsQ)
    #print(pdermats$prSsS)
    
    #plot(sqrt(diag(pdermats$prPrQsPsQ %*% pdermats$paXaYbetaAB %*% t(cholalphaPQ) %*% cholalphaPQ %*% t(pdermats$paXaYbetaAB) %*% t(pdermats$prPrQsPsQ) )))
    Jalpha <- pdermats$prSsS %*% pdermats$prPrQsPsQ %*% pdermats$paXaYbetaAB
    pdereqYx <- JeY %*% Jalpha
    
    #print(Jalpha)
    #plot(sqrt(diag(Jalpha %*% t(cholalphaPQ) %*% cholalphaPQ  %*% t(Jalpha))))
    coveqYx <- pdereqYx %*% t(cholalphaPQ) %*% cholalphaPQ %*% t(pdereqYx)
    output <- data.frame(eqYx=eqYx, SEEYx=sqrt(diag(coveqYx)))
    #return(list(equating=output, coveqYx=coveqYx))     
    out <- new("keout", coveqYx=coveqYx, pdereqYx=pdereqYx, Pobs=P, Qobs=Q, scores=list(X=data.frame(x, r=r, cdfx=cdfx), Y=data.frame(y, s=s, cdfy=cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="IRT-OSE PSE", equating=output, irt=list(covalphaP=adjcovalphaP, covalphaQ=adjcovalphaQ, ltmP=ltmP, ltmQ=ltmQ), see=see)
    return(out)
  }
  
  else return("Currently only CE, PSE and EG have been implemented.")
  
}

irtoseeq <- function(P, Q, x, y, a, qpoints, N, M, model="2pl", design="CE", kernel="gaussian", slog=1, bunif=1, KPEN=0, wpen=0.5, linear=FALSE, altopt=FALSE, h=list(hx=0, hy=0, hxP=0, haP=0, hyQ=0, haQ=0), hlin=list(hxlin=0, hylin=0, hxPlin=0, haPlin=0, hyQlin=0, haQlin=0)){
  
  
  #P - object from ltm with IRT model for group P, where the first items are main test, the last are the anchor test OR a matrix of responses w/out missing values, rows are individuals, columns are items, the main test first, then the anchor test
  #Q - equivalent to P, for group Q
  #x - score points test X
  #y - score points test Y
  #a - score points test A
  #qpoints - quadrature points to be used, if not specified defaults to IRT model if provided, otherwise 21 points over (-5,5) weighted according to ~N(0,1)
  #model - IRT model, "1pl", "2pl", "3pl" (start w/ 2pl)
  #see kequate() for rest args
  
  if(design=="CE"){
  if("ltm" %in% class(P)){
    bx <- as.numeric(coef.ltm(P)[,1])[1:(length(x)-1)]
    baP <- as.numeric(coef.ltm(P)[,1])[(length(x)):((length(x)+length(a)-2))]
    ax <- as.numeric(coef.ltm(P)[,2])[1:(length(x)-1)]
    aaP <- as.numeric(coef.ltm(P)[,2])[(length(x)):((length(x)+length(a)-2))]
    if(P$IRT.param){
      bxltm <- -ax*bx
      baPltm <- -aaP*baP
    } else{
      bxltm <- bx
      baPltm <- baP
      bx <- -bxltm/ax
      baP <- -baPltm/aaP
    }
    ltmP <- P
    P <- ltmP$X
  } 
  
  if("ltm" %in% class(Q)){
    by <- as.numeric(coef.ltm(Q)[,1])[1:(length(y)-1)]
    baQ <-  as.numeric(coef.ltm(Q)[,1])[(length(y)):((length(y)+length(a)-2))]
    ay <- as.numeric(coef.ltm(Q)[,2])[1:(length(y)-1)]
    aaQ <- as.numeric(coef.ltm(Q)[,2])[(length(y)):((length(y)+length(a)-2))]
    if(Q$IRT.param){
      byltm <- -ay*by
      baQltm <- -aaQ*baQ
    } else{
      byltm <- by
      baQltm <- baQ
      by <- -byltm/ay
      baQ <- -baQltm/aaQ
    }
    ltmQ <- Q
    Q <- ltmQ$X
  }
  
  if("tpm" %in% class(P)){
    cx <- as.numeric(coef.tpm(P)[,1])[1:(length(x)-1)]
    caP <- as.numeric(coef.tpm(P)[,1])[(length(x)):((length(x)+length(a)-2))]
    bx <- as.numeric(coef.tpm(P)[,2])[1:(length(x)-1)]
    baP <-  as.numeric(coef.tpm(P)[,2])[(length(x)):((length(x)+length(a)-2))]
    ax <- as.numeric(coef.tpm(P)[,3])[1:(length(x)-1)]
    aaP <- as.numeric(coef.tpm(P)[,3])[(length(x)):((length(x)+length(a)-2))]
    if(P$IRT.param){
      bxltm <- -ax*bx
      baPltm <- -aaP*baP
    } else{
      bxltm <- bx
      baPltm <- baP
      bx <- -bxltm/ax
      baP <- -baPltm/aaP
    }
    N <- dim(P$X)[1]
    ltmP <- P
    P <- ltmP$X
  }
  
  if("tpm" %in% class(Q)){
    cy <- as.numeric(coef.tpm(Q)[,1])[1:(length(y)-1)]
    caQ <- as.numeric(coef.tpm(Q)[,1])[(length(y)):((length(y)+length(a)-2))]
    by <- as.numeric(coef.tpm(Q)[,2])[1:(length(y)-1)]
    baQ <-  as.numeric(coef.tpm(Q)[,2])[(length(y)):((length(y)+length(a)-2))]
    ay <- as.numeric(coef.tpm(Q)[,3])[1:(length(y)-1)]
    aaQ <- as.numeric(coef.tpm(Q)[,3])[(length(y)):((length(y)+length(a)-2))]
    if(Q$IRT.param){
      byltm <- -ay*by
      baQltm <- -aaQ*baQ
    } else{
      byltm <- by
      baQltm <- baQ
      by <- -byltm/ay
      baQ <- -baQltm/aaQ
    }
    M <- dim(Q$X)[1]
    ltmQ <- Q
    Q <- ltmQ$X
  }
  
  if(model=="1pl"){
    irtx <- probpl(qpoints, bx, model)
    irtaP <- probpl(qpoints, baP, model)
    irty <- probpl(qpoints, by, model)
    irtaQ <- probpl(qpoints, baQ, model)
    #pderX <- pderpl(irtx, qpoints, bx, model)
    #pderAP <- pderpl(irtaP, qpoints, baP, model)
    #pderY <- pderpl(irty, qpoints, by, model)
    #pderAQ <- pderpl(irtaQ, qpoints, baQ, model)
  }
  
  if(model=="2pl"){
    irtx <- probpl(qpoints, bx, model, a=ax)
    irtaP <- probpl(qpoints, baP, model, a=aaP)
    irty <- probpl(qpoints, by, model, a=ay)
    irtaQ <- probpl(qpoints, baQ, model, a=aaQ)
    #pderX <- pderpl(irtx, qpoints, bx, model, a=ax)
    #pderAP <- pderpl(irtaP, qpoints, baP, model, a=aaP)
    #pderY <- pderpl(irty, qpoints, by, model, a=ay)
    #pderAQ <- pderpl(irtaQ, qpoints, baQ, model, a=aaQ)
  }
  
  if(model=="3pl"){
    irtx <- probpl(qpoints, bx, a=ax, c=cx)
    irtaP <- probpl(qpoints, baP, a=aaP, c=caP)
    irty <- probpl(qpoints, by, a=ay, c=cy)
    irtaQ <- probpl(qpoints, baQ, a=aaQ, c=caQ)
    #pderX <- pderpl(irtx, qpoints, bx, a=ax, c=cx)
    #pderAP <- pderpl(irtaP, qpoints, baP, a=aaP, c=caP)
    #pderY <- pderpl(irty, qpoints, by, a=ay, c=cy)
    #pderAQ <- pderpl(irtaQ, qpoints, baQ, a=aaQ, c=caQ)
  }
  
  rP <- LordWW(irtx, qpoints)
  tP <- LordWW(irtaP, qpoints)
  sQ <- LordWW(irty, qpoints)
  tQ <- LordWW(irtaQ, qpoints)
  
  
  if(kernel=="uniform"){
    ulimit<-(1/(2*bunif*(1-0.61803)))
    KPEN<-0
  } else ulimit<-4
  
  meanx <- x%*%rP
  meany <- y%*%sQ
  meanaP <- a%*%tP
  meanaQ <- a%*%tQ
  varx <- (N/(N-1))*(x^2%*%rP-meanx^2)
  vary <- (M/(M-1))*(y^2%*%sQ-meany^2)
  varaP <- (N/(N-1))*(a^2%*%tP-meanaP^2)
  varaQ <- (M/(M-1))*(a^2%*%tQ-meanaQ^2)
  if(linear){
    if(hlin$hxPlin==0)
      hxP<-as.numeric(1000*sqrt(varx))
    else
      hxP<-hlin$hxPlin
    if(hlin$hyQlin==0)
      hyQ<-as.numeric(1000*sqrt(vary))
    else
      hyQ<-hlin$hyQlin
    if(hlin$haPlin==0)
      haP<-as.numeric(1000*sqrt(varaP))
    else
      haP<-hlin$haPlin
    if(hlin$haQlin==0)
      haQ<-as.numeric(1000*sqrt(varaQ))
    else
      haQ<-hlin$haQlin
  }
  else{
    if(h$hxP==0)
      if(altopt)
        hxP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
    else{
      if(KPEN==0)
        hxP<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hxPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hxP<-optimize(PEN, interval=c(hxPPEN1min, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(h$hyQ==0)
      if(altopt)
        hyQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
    else{
      if(KPEN==0)
        hyQ<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hyQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hyQ<-optimize(PEN, interval=c(hyQPEN1min, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(h$haP==0)
      if(altopt)
        haP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varaP)
    else{
      if(KPEN==0)
        haP<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        haPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        haP<-optimize(PEN, interval=c(haPPEN1min, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(h$haQ==0)
      if(altopt)
        haQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(varaQ)
    else{
      if(KPEN==0)
        haQ<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        haQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        haQ<-optimize(PEN, interval=c(haQPEN1min, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
  }
  
  cdfxP <- cdf(rP, hxP, meanx, varx, kernel, slog, bunif)
  cdfyQ <- cdf(sQ, hyQ, meany, vary, kernel, slog, bunif)
  cdfaP <- cdf(tP, haP, meanaP, varaP, kernel, slog, bunif)
  cdfaQ <- cdf(tQ, haQ, meanaQ, varaQ, kernel, slog, bunif)
  
  #Cp <- cmatrixSG(as.vector(P), DMP, N)
  #Cq <- cmatrixSG(as.vector(Q), DMQ, M)
  #Want to link tests X and A (not equal length)
  #Want to find the values of the continuized cdf for A on Q for the "linked" values from X to A on P.
  
  eAx<-eqinvCE(cdfxP, tP, x, a, varaP, meanaP, haP, kernel, slog, bunif)		  #Linked scores from X to A on P
  cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a, kernel, slog, bunif)		  #cdf of the linked values on Q
  eYCEeAx<-eqinvCE(cdfeAxQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)
  return(eYCEeAx)
  }
  if(design=="EG"){
    if(model=="2pl"){
      if("ltm" %in% class(P)){
        bx <- as.numeric(coef.ltm(P)[,1])[1:(length(x)-1)]
        ax <- as.numeric(coef.ltm(P)[,2])[1:(length(x)-1)]
        if(P$IRT.param){
          bxltm <- -ax*bx
        } else{
          bxltm <- bx
          bx <- -bxltm/ax
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(x)-1) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          bx <- as.numeric(coef.ltm(ltmP)[,1])[1:(length(x)-1)]
          ax <- as.numeric(coef.ltm(ltmP)[,2])[1:(length(x)-1)]
          bxltm <- bx
          bx <- -bxltm/ax
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("ltm" %in% class(Q)){
        by <- as.numeric(coef.ltm(Q)[,1])[1:(length(y)-1)]
        ay <- as.numeric(coef.ltm(Q)[,2])[1:(length(y)-1)]
        if(Q$IRT.param){
          byltm <- -ay*by
        } else{
          byltm <- by
          by <- -byltm/ay
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(y)-1) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- ltm(Q ~ z1, IRT.param=FALSE)
          by <- as.numeric(coef.ltm(ltmQ)[,1])[1:(length(y)-1)]
          ay <- as.numeric(coef.ltm(ltmQ)[,2])[1:(length(y)-1)]
          byltm <- by
          by <- -byltm/ay
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      if(missing(qpoints))
        qpoints <- -ltmP$GH$Z[,2]
      irtx <- probpl(qpoints, bx, model, a=ax)
      irty <- probpl(qpoints, by, model, a=ay)
    }
    
    if(model=="3pl"){
      if("tpm" %in% class(P)){
        cx <- as.numeric(coef.tpm(P)[,1])[1:(length(x)-1)]
        bx <- as.numeric(coef.tpm(P)[,2])[1:(length(x)-1)]
        ax <- as.numeric(coef.tpm(P)[,3])[1:(length(x)-1)]
        if(P$IRT.param){
          bxltm <- -ax*bx
        } else{
          bxltm <- bx
          bx <- -bxltm/ax
        }
        N <- dim(P$X)[1]
        ltmP <- P
        P <- ltmP$X
      } else{
        if(is.matrix(P)){
          if((length(x)-1) != ncol(P))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmP <- ltm(P ~ z1, IRT.param=FALSE)
          cx <- as.numeric(coef.tpm(ltmP)[,1])[1:(length(x)-1)]
          bx <- as.numeric(coef.tpm(ltmP)[,2])[1:(length(x)-1)]
          ax <- as.numeric(coef.tpm(ltmP)[,3])[1:(length(x)-1)]
          bxltm <- bx
          bx <- -bxltm/ax
          N <- dim(P)[1]
        } else{
          return("Unsupported input. P must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      
      if("tpm" %in% class(Q)){
        cy <- as.numeric(coef.tpm(Q)[,1])[1:(length(y)-1)]
        by <- as.numeric(coef.tpm(Q)[,2])[1:(length(y)-1)]
        ay <- as.numeric(coef.tpm(Q)[,3])[1:(length(y)-1)]
        if(Q$IRT.param){
          byltm <- -ay*by
        } else{
          byltm <- by
          by <- -byltm/ay
        }
        M <- dim(Q$X)[1]
        ltmQ <- Q
        Q <- ltmQ$X
      } else{
        if(is.matrix(Q)){
          if((length(y)-1) != ncol(Q))
            return("Unsupported input. Input matrices must have rows denoting individuals and columns denoting items.")
          ltmQ <- tpm(Q, IRT.param=FALSE)
          cy <- as.numeric(coef.tpm(ltmQ)[,1])[1:(length(y)-1)]
          by <- as.numeric(coef.tpm(ltmQ)[,2])[1:(length(y)-1)]
          ay <- as.numeric(coef.tpm(ltmQ)[,3])[1:(length(y)-1)]
          byltm <- by
          by <- -byltm/ay
          M <- dim(Q)[1]
        } else{
          return("Unsupported input. Q must be either an object created by the package ltm or a matrix of responses.")
        }
      }
      if(missing(qpoints))
        qpoints <- -ltmP$GH$Z[,2]
      irtx <- probpl(qpoints, bx, a=ax, c=cx)
      irty <- probpl(qpoints, by, a=ay, c=cy)
    }
    r <- LordWW(irtx, qpoints)
    s <- LordWW(irty, qpoints)
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    } else ulimit<-4
    
    meanx <- x%*%r
    meany <- y%*%s
    
    varx <- (N/(N-1))*(x^2%*%r-meanx^2)
    vary <- (M/(M-1))*(y^2%*%s-meany^2)
    
    if(linear){
      if(hlin$hxlin==0)
        hx<-as.numeric(1000*sqrt(varx))
      else
        hx<-hlin$hxlin
      if(hlin$hylin==0)
        hy<-as.numeric(1000*sqrt(vary))
      else
        hy<-hlin$hylin
    } else{
      if(h$hx==0){
        if(altopt){
          hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
        } else{
          if(KPEN==0){
            hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          } else{
            hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      }
      if(h$hy==0){
        if(altopt){
          hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
        } else{
          if(KPEN==0){
            hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          } else{
            hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      }
    }
    
    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)       						#Continuize the estimated cdf:s for X and Y.
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)  							#Calculate the equated score values.
    return(eqYx)
  }
  
}


#tabulate frequency data from test scores on individuals
kefreq<-function(in1, xscores, in2, ascores){
    if(missing(ascores))
      if(missing(in2)){
        frequency<-as.vector(table(factor(in1, levels=xscores, ordered=TRUE)))
        return(data.frame(X=xscores, frequency))
      }  
      else{
        frequency<-as.vector(table(factor(in1, levels=xscores, ordered=TRUE), factor(in2, levels=xscores, ordered=TRUE)))
        return(data.frame(X=rep(xscores, length(xscores)), Y=rep(xscores, each=length(xscores)), frequency))
      }
    if(!missing(ascores) & !missing(in2)){
      frequency<-as.vector(table(factor(in1, levels=xscores, ordered=TRUE),factor(in2, levels=ascores, ordered=TRUE)))
      return(data.frame(X=rep(xscores, length(ascores)), A=rep(ascores, each=length(xscores)), frequency))
    }
    else
      return
}

#general implementation of IRT observed score equating in the KE framework
#the ability estimates from the IRT model are used to create score probabilities for each score on the two tests
#the resulting score probabilities are treated as though coming from an EG design
keirt<-function(irtr, irts, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif){
      if(kernel=="uniform"){
        ulimit<-(1/(2*bunif*(1-0.61803)))
        KPEN<-0
      }
      else
        ulimit<-4
      rirt<-LordW(irtr)
      sirt<-LordW(irts)
      meanxirt<-x%*%rirt
      meanyirt<-y%*%sirt
      varxirt<-(N/(N-1))*(x^2%*%rirt-meanxirt^2)
      varyirt<-(M/(M-1))*(y^2%*%sirt-meanyirt^2)
      if(linear){
        hxirt<-as.numeric(1000*sqrt(varxirt))
        hyirt<-as.numeric(1000*sqrt(varyirt))
      }
      
      else{
        if(KPEN==0){
          hxirt<-optimize(PEN, interval=c(0, ulimit), r=rirt, x, var=varxirt, mean=meanxirt, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hyirt<-optimize(PEN, interval=c(0, ulimit), r=sirt, y, var=varyirt, mean=meanyirt, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
        else{
          hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=rirt, x, var=varxirt, mean=meanxirt, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=sirt, y, var=varyirt, mean=meanyirt, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hxirt<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=rirt, x, var=varxirt, mean=meanxirt, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hyirt<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=sirt, y, var=varyirt, mean=meanyirt, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
        hxirtlin<-as.numeric(1000*sqrt(varxirt))
        hyirtlin<-as.numeric(1000*sqrt(varyirt))
      }
      
      cdfxirt<-cdf(rirt, hxirt, meanxirt, varxirt, kernel, slog, bunif)
      cdfyirt<-cdf(sirt, hyirt, meanyirt, varyirt, kernel, slog, bunif)
      eqYxIRT<-eqinvCE(cdfxirt, sirt, x, y, varyirt, meanyirt, hyirt, kernel, slog, bunif, IRT=TRUE)
      
      gprimeYirt<-densityeq(sirt, hyirt, varyirt, meanyirt, eqYxIRT, y, kernel, slog, bunif)    					#G' for eY
		  fprimeYirt<-densityeq(rirt, hxirt, varxirt, meanxirt, x, x, kernel, slog, bunif)							#F' for eY

      dFdreYEGirt<-dFdr(rirt, hxirt, varxirt, meanxirt, fprimeYirt, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
	  	dGdseYEGirt<-dFdr(sirt, hyirt, varyirt, meanyirt, gprimeYirt, eqYxIRT, y, kernel, slog, bunif)						#KxK

  		Urirt<-1/sqrt(N)*(diag(sqrt(rirt))-rirt%*%t(sqrt(rirt)))										#Ur, Vs are C-matrices from the factorization of the covariance matrix
	  	Vrirt<-matrix(0, ncol=length(x), nrow=length(y))								#Us- and Vr-matrices are zero matrices for EG.
		  Usirt<-matrix(0, ncol=length(y), nrow=length(x))			
	  	Vsirt<-1/sqrt(M)*(diag(sqrt(sirt))-sirt%*%t(sqrt(sirt)))

      SEEYxirt<-SEE_EY(gprimeYirt, dFdreYEGirt, dGdseYEGirt, Urirt, Vrirt, Usirt, Vsirt)
      PREYxIRT<-PREp(eqYxIRT, y, rirt, sirt)
      
      if(!linear){
        SEED_EG<-SEED(fprimeYirt, gprimeYirt, dFdreYEGirt, dGdseYEGirt, Urirt, Vrirt, Usirt, Vsirt, rirt, sirt, meanxirt, varxirt, meanyirt, varyirt, x, y, hxirtlin, hyirtlin, slog, bunif)
        output<-data.frame(eqYxIRT=eqYxIRT, SEEYxIRT=SEEYxirt, eqYxIRTLIN=SEED_EG@SEED$eqYxLIN, SEEYxIRTLIN=SEED_EG@SEED$SEEYxLIN, SEEDYxIRT=SEED_EG@SEED$SEEDYx)
        return(new("keout", Cr=Urirt, Cs=Vsirt, SEEvect=SEED_EG@SEEvect, scores=list(X=data.frame(x, rirt, cdfxirt), Y=data.frame(y, sirt, cdfyirt), M=M, N=N), linear=linear, PRE=data.frame(PREYxIRT), h=data.frame(hyirt=hyirt, hxirt=hxirt, SEED_EG@hlin), kernel=kernel, type="IRT equipercentile", equating=output))
      }
      else{
        output<-data.frame(eqYxIRT=eqYxIRT, SEEYxIRT=SEEYxirt, cdfxirt=cdfxirt, cdfyirt=cdfyirt)
        return(new("keout", Cr=Urirt, Cs=Vsirt, scores=list(X=data.frame(x, rirt, cdfxirt), Y=data.frame(y, sirt, cdfyirt), M=M, N=N), linear=linear, PRE=data.frame(PREYxIRT), h=data.frame(hyirt=hyirt, hxirt=hxirt), kernel=kernel, type="IRT linear", equating=output))
      }
}

kequate<-function(design, ...){
  if(typeof(design)!="character")
    stop("Error. design must be a character vector.")
  if(! design %in% c("CB", "EG", "SG", "NEAT_CE", "NEAT_PSE", "NEC"))
	  stop("Error. design must be CB, EG, SG, NEAT_CE, NEAT_PSE or NEC.")
  if(design=="CB")
    return(kequateCB(...))
  if(design=="EG")
    return(kequateEG(...))
  if(design=="SG")
    return(kequateSG(...))
  if(design %in% c("NEAT_PSE", "NEC"))
    return(kequateNEAT_PSE(...))
  if(design=="NEAT_CE")
    return(kequateNEAT_CE(...))
}

kequateCB<-function(x, y, P12, P21, DM12, DM21, N, M, hx=0, hy=0, hxlin=0, hylin=0, wcb=1/2, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE, kernel="gaussian", slog=1, bunif=0.5, altopt=FALSE){
  stopifnot(is(smoothed, "logical"))
  if(smoothed==FALSE){
    if(kernel=="uniform")
      ulimit<-(1/(2*bunif*(1-0.61803)))
    else
      ulimit<-4
    r1<-rowSums(P12)
    r2<-rowSums(P21)
    s1<-colSums(P12)
    s2<-colSums(P21)
    
    #we weight the two populations
    r<-wcb*r1+(1-wcb)*r2
    s<-wcb*s1+(1-wcb)*s2
    
    meanx<-x%*%r
    varx<-(N/(N-1))*(x^2%*%r-meanx^2)
    meany<-y%*%s
    vary<-(N/(N-1))*(y^2%*%s-meany^2)
    
    if(linear){
      if(hxlin==0)
        hx<-as.numeric(1000*sqrt(varx))
      else
        hx<-hxlin
      if(hylin==0)
        hy<-as.numeric(1000*sqrt(vary))
      else
        hy<-hylin
    }
    else{
      if(hx==0)
        if(altopt)
          hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
      else{
        if(KPEN==0)
          hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      if(hy==0)
        if(altopt)
          hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
      else{
        if(KPEN==0)
          hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    } 
    
    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif, smoothed=FALSE)
    
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    if(irtx!=0 && irty!=0)
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    
    if(!linear){
      output<-data.frame(eqYx)
      if(irtx!=0 && irty!=0){
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
        out<-new("keout", scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Unsmoothed CB equipercentile", equating=output)
        return(out)
      }
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h), kernel=kernel, type="Unsmoothed CB equipercentile", equating=output)
      return(out)
    }
    
    output<-data.frame(eqYx)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin, irtout@h), kernel=kernel, type="Unsmoothed CB linear", equating=output)
      return(out)
    }
    
    out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), kernel=kernel, type="Unsmoothed CB linear", equating=output)
    return(out)
  }
  
  if(is(P12, "glm")){
    if(is(P12$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmP12<-P12
    N<-sum(glmP12$y)
    P12<-matrix(glmP12$fitted.values/N, nrow=length(x))
    DM12<-glmP12$x[,-1]
    P12obs<-matrix(glmP12$y/N, nrow=length(x))
  }
  else
    P12obs<-matrix(0)
  if(is(P21, "glm")){
    if(is(P21$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmP21<-P21
    M<-sum(glmP21$y)
    P21<-matrix(glmP21$fitted.values/M, nrow=length(x))
    DM21<-glmP21$x[,-1]
    P21obs<-matrix(glmP21$y/M, nrow=length(x))
  }
  else
    P21obs=matrix(0)
  if(is(P12, "numeric"))
    P12<-matrix(P12, nrow=length(x))
  if(is(P21, "numeric"))
    P21<-matrix(P21, nrow=length(x))
  stopifnot(is(x, "numeric"), is(y, "numeric"), is(P12,"matrix"), is(P21,"matrix"), is(DM12, "matrix"), is(DM21,"matrix"), 
            is(N,"numeric"), is(M,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
  
  if(kernel=="uniform")
    ulimit<-(1/(2*bunif*(1-0.61803)))
  else
    ulimit<-4
  r1<-rowSums(P12)
  r2<-rowSums(P21)
  s1<-colSums(P12)
  s2<-colSums(P21)
  
  #we weight the two populations
  r<-wcb*r1+(1-wcb)*r2
  s<-wcb*s1+(1-wcb)*s2
  
  meanx<-x%*%r
  varx<-(N/(N-1))*(x^2%*%r-meanx^2)
  meany<-y%*%s
  vary<-(N/(N-1))*(y^2%*%s-meany^2)
  
  if(linear){
    if(hxlin==0)
      hx<-as.numeric(1000*sqrt(varx))
    else
      hx<-hxlin
    if(hylin==0)
      hy<-as.numeric(1000*sqrt(vary))
    else
      hy<-hylin
  }
  else{
    if(hx==0)
      if(altopt)
        hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
    else{
      if(KPEN==0)
        hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(hy==0)
      if(altopt)
        hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
    else{
      if(KPEN==0)
        hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    hxlin<-as.numeric(1000*sqrt(varx))
    hylin<-as.numeric(1000*sqrt(vary))
  }
  
  cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
  cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
  if(length(x)>100){
    Cp12<-cmatris(as.vector(P12), DM12, N)
    Cp21<-cmatris(as.vector(P21), DM21, M)
  } else{
    Cp12<-cmatrixSG(as.vector(P12), DM12, N)
    Cp21<-cmatrixSG(as.vector(P21), DM21, M)
  }
  
  eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)
  
  gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)
  fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)
  
  dFdreYSG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)
  dGdseYSG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)
  
  U12<-matrix(0, length(x), ncol=dim(DM12)[2])
  i<-1
  while(i<length(x)*length(y)){
    U12<-Cp12[i:(i+length(x)-1),]+U12
    i<-i+length(x)
  }
  U21<-matrix(0, length(x), ncol=dim(DM21)[2])
  i<-1
  while(i<length(x)*length(y)){
    U21<-Cp21[i:(i+length(x)-1),]+U21
    i<-i+length(x)
  }
  V21<-matrix(0, length(y), ncol=dim(DM21)[2])
  I<-t(matrix(1, length(x), ncol=1))
  i<-1
  j<-1
  while(i<(length(x)*length(y))){
    V21[j,1:dim(DM21)[2]]<-I%*%Cp21[i:(length(x)+i-1),]
    i<-i+length(x)
    j<-j+1
  }
  V12<-matrix(0, length(y), ncol=dim(DM12)[2])
  I<-t(matrix(1, length(x), ncol=1))
  i<-1
  j<-1
  while(i<(length(x)*length(y))){
    V12[j,1:dim(DM12)[2]]<-I%*%Cp12[i:(length(x)+i-1),]
    i<-i+length(x)
    j<-j+1
  }
  Ur<-wcb*U12
  Us<-(1-wcb)*U21
  Vr<-(1-wcb)*V12
  Vs<-wcb*V21
  
  SEEYx<-numeric(length(x))
  SEEYxmat<-matrix(0, length(x), dim(Ur)[2]+dim(Us)[2])                                                         #Prepare matrix containing the SEE-vectors
  SEEYxmat[,1:dim(Ur)[2]]<-(1/gprimeY)*(dFdreYSG%*%Ur-dGdseYSG%*%Vr)                                            #Fill matrix
  SEEYxmat[,(dim(Ur)[2]+1):(dim(Ur)[2]+dim(Us)[2])]<-(1/gprimeY)*(dFdreYSG%*%Us-dGdseYSG%*%Vs)    
  SEEYx<-(sqrt(rowSums(SEEYxmat^2)))
  PREYx<-PREp(eqYx, y, r, s)
  PRE<-data.frame(PREYx)
  h<-data.frame(hx, hy)
  
  if(irtx!=0 && irty!=0){
    irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
  }
  
  if(!linear){
    CBlin<-kequate("CB", x=x, y=y, P12=P12, P21=P21, N=N, M=M, DM12, DM21, N, M, hxlin=hxlin, hylin=hxlin, wcb=wcb, KPEN=KPEN, wpen=wpen, linear=TRUE, irtx=irtx, irty=irty, kernel="gaussian")
    SEEvectors<-new("SEEvect", SEEYx=SEEYxmat, SEEYxLIN=CBlin@SEEvect@SEEYxLIN)
    SEEDYx<-sqrt(rowSums((SEEYxmat-SEEvectors@SEEYxLIN)^2))
    output<-data.frame(eqYx, SEEYx, SEEDYx, eqYxLIN=CBlin@equating$eqYx, SEEYxLIN=CBlin@equating$SEEYx)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEEDYx, eqYxLIN=CBlin@equating$eqYx, SEEYxLIN=CBlin@equating$SEEYx)
      out<-new("keout", Cp=Cp12, Cq=Cp21, SEEvect=SEEvectors, Pest=P12, Pobs=P12obs, Qest=P21, Qobs=P21obs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="CB equipercentile", equating=output)
      return(out)
    }
    out<-new("keout", Cp=Cp12, Cq=Cp21, SEEvect=SEEvectors, Pest=P12, Pobs=P12obs, Qest=P21, Qobs=P21obs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h), kernel=kernel, type="CB equipercentile", equating=output)
    return(out)
  }
  
  SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEEYxmat)
  output<-data.frame(eqYx, SEEYx)
  out<-new("keout", Cp=Cp12, Cq=Cp21, SEEvect=SEEvectors, Pest=P12, Pobs=P12obs, Qest=P21, Qobs=P21obs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), kernel=kernel, type="CB linear", equating=output)
  return(out)
}

kequateEG<-function(x, y, r, s, DMP, DMQ, N, M, hx=0, hy=0, hxlin=0, hylin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE, kernel="gaussian", slog=1, bunif=0.5, altopt=FALSE, tlinear=FALSE){
  stopifnot(is(smoothed, "logical"))
  if(smoothed==FALSE){
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    }
    else
      ulimit<-4
    meanx<-x%*%r
    varx<-(N/(N-1))*(x^2%*%r-meanx^2)
    meany<-y%*%s
    vary<-(M/(M-1))*(y^2%*%s-meany^2)
    if(linear){
      if(hxlin==0)
        hx<-as.numeric(1000*sqrt(varx))
      else
        hx<-hxlin
      if(hylin==0)
        hy<-as.numeric(1000*sqrt(vary))
      else
        hy<-hylin
    }
    else{
      if(hx==0)
        if(altopt)
          hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
      else{
        if(KPEN==0)
          hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      if(hy==0)
        if(altopt)
          hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
      else{
        if(KPEN==0)
          hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    }
    
    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif, smoothed=FALSE)
    
    gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)  						#G' for eY
    fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)							#F' for eY
    
    if(altopt){
      altfprim <- altoptdensity(r, hx, varx, meanx, x, x)
      altgprim <- altoptdensity(s, hy, vary, meanx, eqYx, y)
    }
    if(altopt){
      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altfprim)						#Matrices of derivatives. JxJ.
      dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif, altopt=altopt, altfprim=altgprim)	
    }
    else{
      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
      dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)						#KxK
    }
    
    Ur<-1/sqrt(N)*(diag(sqrt(r))-r%*%t(sqrt(r)))										#Ur, Vs are C-matrices from pre-smooting model for EG.
    Vr<-matrix(0, ncol=length(x), nrow=length(y))								#Us- and Vr-matrices are zero matrices for EG.
    Us<-matrix(0, ncol=length(y), nrow=length(x))			
    Vs<-1/sqrt(M)*(diag(sqrt(s))-s%*%t(sqrt(s)))
    
    SEEYx<-SEE_EY(gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs)
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    output<-data.frame(eqYx, SEEYx)
    
    if(irtx!=0 && irty!=0){
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    }
    if(!linear){
      SEED_EG<-SEED(fprimeY, gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin, slog, bunif)
      output<-data.frame(eqYx, SEEYx, SEED_EG@SEED)
      if(irtx!=0 && irty!=0){
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEED_EG@SEED)
        out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEED_EG@SEEvect, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin, irtout@h), kernel=kernel, type="Equipercentile EG without pre-smoothing", equating=output)
        return(out)
      }
      out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEED_EG@SEEvect, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin), kernel=kernel, type="Equipercentile EG without pre-smoothing", equating=output)
      return(out)
    }
    SEE_EYlin<-matrix(nrow=length(x),ncol=(length(t(dFdreYEG[,1])%*%Ur)+length(t(dFdreYEG[,1])%*%Us)))
    for(i in 1:length(x)){
      SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYEG[i,]%*%Ur-dGdseYEG[i,]%*%Vr, dFdreYEG[i,]%*%Us-dGdseYEG[i,]%*%Vs)
    }
    SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEE_EYlin)
    output<-data.frame(eqYx, SEEYx)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT)
      out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEEvectors, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Linear EG without pre-smoothing", equating=output)
      return(out)
    }
    if(tlinear){
      tlinout<-tradlinear(x, y, r, s, N, M)
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), kernel=kernel, type="Traditional linear EG without pre-smoothing", equating=tlinout)
      return(out)
    }
    out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEEvectors, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Linear EG without pre-smoothing", equating=output)
    return(out)
  }
  
  if(is(r, "glm")){
    if(is(r$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmP<-r
    N<-sum(glmP$y)
    r<-glmP$fitted.values/N
    DMP<-glmP$x[,-1]
    Pobs<-matrix(glmP$y/N)
  }
  else
    Pobs<-matrix(0)
  if(is(s, "glm")){
    if(is(s$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmQ<-s
    M<-sum(glmQ$y)
    s<-glmQ$fitted.values/M
    DMQ<-glmQ$x[,-1]
    Qobs<-matrix(glmQ$y/M)
  }
  else
    Qobs<-matrix(0)
  stopifnot(is(x, "numeric"), is(r,"numeric"), is(s,"numeric"), is(DMP, "matrix"), is(DMQ, "matrix"), 
            is(N,"numeric"), is(M,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
  
  if(kernel=="uniform"){
    ulimit<-(1/(2*bunif*(1-0.61803)))
    KPEN<-0
  }
  else
    ulimit<-4
  
  meanx<-x%*%r
  varx<-(N/(N-1))*(x^2%*%r-meanx^2)
  meany<-y%*%s
  vary<-(M/(M-1))*(y^2%*%s-meany^2)
  #Set continuization parameter in the linear case.
  if(linear){
    if(hxlin==0)
      hx<-as.numeric(1000*sqrt(varx))
    else
      hx<-hxlin
    if(hylin==0)
      hy<-as.numeric(1000*sqrt(vary))
    else
      hy<-hylin
  }
  else{
    if(hx==0)
      if(altopt)
        hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
    else{
      if(KPEN==0)
        hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(hy==0)
      if(altopt)
        hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
    else{
      if(KPEN==0)
        hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
  }
  
  cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif) 									#Continuize the estimated cdf:s for X and Y.
  cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
  if(length(x)>100){
    Cr<-cmatris(r, DMP, N)										#Calculare c-matrices
    Cs<-cmatris(s, DMQ, M)
  } else{
    Cr<-cmatrixSG(r, DMP, N)  									#Calculare c-matrices
    Cs<-cmatrixSG(s, DMQ, M)
  }
  
  eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)								#Calculate the equated score values.
  
  PREYx<-PREp(eqYx, y, r, s)
  PRE<-data.frame(PREYx)
  h<-data.frame(hx, hy)
  
  if(irtx!=0 && irty!=0){
    irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
  }
  
  gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)							#G' for eY
  fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)							#F' for eY
  #gprimeX<-densityeq(s, hy, vary, meany, y, y)
  #fprimeX<-densityeq(r, hx, varx, meanx, eqXy, x)
  
  if(altopt){
    altfprim <- altoptdensity(r, hx, varx, meanx, x, x)
    altgprim <- altoptdensity(s, hy, vary, meanx, eqYx, y)
  }
  if(altopt){
    dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altfprim)						#Matrices of derivatives. JxJ.
    dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif, altopt=altopt, altfprim=altgprim)	
  }
  else{
    dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
    dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)						#KxK
  }
  
  Ur<-Cr													#Ur, Vs are C-matrices from pre-smooting model for EG.
  Vr<-matrix(0, ncol=dim(DMP)[2], nrow=length(y))								#Us- and Vr-matrices are zero matrices for EG.
  Us<-matrix(0, ncol=dim(DMQ)[2], nrow=length(x))			
  Vs<-Cs
  
  SEEYx<-SEE_EY(gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs)					#Std errors for each score value. Vector w/ length(r) elements.
  
  if(!linear){
    SEED_EG<-SEED(fprimeY, gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin, slog, bunif)
    output<-data.frame(eqYx, SEEYx, SEED_EG@SEED)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEED_EG@SEED)
      out<-new("keout", Cr=Cr, Cs=Cs, SEEvect=SEED_EG@SEEvect, Pest=matrix(r), Pobs=Pobs, Qest=matrix(s), Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin, irtout@h), kernel=kernel, type="EG equipercentile", equating=output)
      return(out)
    }
    out<-new("keout", Cr=Cr, Cs=Cs, SEEvect=SEED_EG@SEEvect, Pest=matrix(r), Pobs=Pobs, Qest=matrix(s), Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin), kernel=kernel, type="EG equipercentile", equating=output)
    return(out)
  }
  SEE_EYlin<-matrix(nrow=length(x),ncol=(length(t(dFdreYEG[,1])%*%Ur)+length(t(dFdreYEG[,1])%*%Us)))
  for(i in 1:length(x)){
    SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYEG[i,]%*%Ur-dGdseYEG[i,]%*%Vr, dFdreYEG[i,]%*%Us-dGdseYEG[i,]%*%Vs)
  }
  SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEE_EYlin)
  output<-data.frame(eqYx, SEEYx)
  if(irtx!=0 && irty!=0){
    output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT)
    out<-new("keout", Cr=Cr, Cs=Cs, SEEvect=SEEvectors, Pest=matrix(r), Pobs=Pobs, Qest=matrix(s), Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="EG linear", equating=output)
    return(out)
  }
  if(tlinear){
    tlinout<-tradlinear(x, y, r, s, N, M)
    out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), kernel=kernel, type="EG traditional linear", equating=tlinout)
    return(out)
  }
  out<-new("keout", Cr=Cr, Cs=Cs, SEEvect=SEEvectors, Pest=matrix(r), Pobs=Pobs, Qest=matrix(s), Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="EG linear", equating=output)
  return(out)
}

kequateNEAT_CE<-function(x, y, a, P, Q, DMP, DMQ, N, M, hxP=0, hyQ=0, haP=0, haQ=0, hxPlin=0, hyQlin=0, haPlin=0, haQlin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE, kernel="gaussian", slog=1, bunif=0.5, altopt=FALSE){
  stopifnot(is(smoothed, "logical"))
  if(smoothed==FALSE){
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    }
    else
      ulimit<-4
    rP<-rowSums(P)
    tP<-colSums(P)
    sQ<-rowSums(Q)
    tQ<-colSums(Q)
    meanx<-x%*%rP
    meany<-y%*%sQ
    meanaP<-a%*%tP
    meanaQ<-a%*%tQ
    varx<-(N/(N-1))*(x^2%*%rP-meanx^2)
    vary<-(M/(M-1))*(y^2%*%sQ-meany^2)
    varaP<-(N/(N-1))*(a^2%*%tP-meanaP^2)
    varaQ<-(M/(M-1))*(a^2%*%tQ-meanaQ^2)
    if(linear){
      if(hxPlin==0){
        hxP<-as.numeric(1000*sqrt(varx))
      }
      else{
        hxP<-hxPlin
      }
      if(hyQlin==0){
        hyQ<-as.numeric(1000*sqrt(vary))
      }
      else{
        hyQ<-hyQlin
      }
      if(haPlin==0){
        haP<-as.numeric(1000*sqrt(varaP))
      }
      else{
        haP<-haPlin
      }
      if(haQlin==0){
        haQ<-as.numeric(1000*sqrt(varaQ))
      }
      else{
        haQ<-haQlin
      }
    }
    else{
      if(hxP==0)
        if(altopt)
          hxP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
        else{
          if(KPEN==0)
            hxP<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          else{
            hxPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hxP<-optimize(PEN, interval=c(hxPPEN1min, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      if(hyQ==0)
        if(altopt)
          hyQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
        else{
          if(KPEN==0)
            hyQ<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          else{
            hyQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            hyQ<-optimize(PEN, interval=c(hyQPEN1min, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      if(haP==0)
        if(altopt)
          haP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varaP)
        else{
          if(KPEN==0)
            haP<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          else{
            haPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          haP<-optimize(PEN, interval=c(haPPEN1min, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      if(haQ==0)
        if(altopt)
          haQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(varaQ)
        else{
          if(KPEN==0)
            haQ<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          else{
            haQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
            haQ<-optimize(PEN, interval=c(haQPEN1min, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
          }
        }
      hxPlin<-as.numeric(1000*sqrt(varx))
      hyQlin<-as.numeric(1000*sqrt(vary))
      haPlin<-as.numeric(1000*sqrt(varaP))
      haQlin<-as.numeric(1000*sqrt(varaQ))
    }
    
    cdfxP<-cdf(rP, hxP, meanx, varx, kernel, slog, bunif)
    cdfyQ<-cdf(sQ, hyQ, meany, vary, kernel, slog, bunif)
    cdfaP<-cdf(tP, haP, meanaP, varaP, kernel, slog, bunif)
    cdfaQ<-cdf(tQ, haQ, meanaQ, varaQ, kernel, slog, bunif)
    
    eAx<-eqinvCE(cdfxP, tP, x, a, varaP, meanaP, haP, kernel, slog, bunif, smoothed=FALSE)  	  #Linked scores from X to A on P
    cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a, kernel, slog, bunif)		  #cdf of the linked values on Q
    eYCEeAx<-eqinvCE(cdfeAxQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif, smoothed=FALSE)	#from eAx on Q to Y            
    eYa<-eqinvCE(cdfaQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif, smoothed=FALSE)
    PREAx<-PREp(eAx, a, rP, tP)
    PREYa<-PREp(eYa, y, tQ, sQ)
    PRE<-data.frame(PREAx, PREYa)
    h<-data.frame(hxP, hyQ, haP, haQ, hxPlin, hyQlin, haPlin, haQlin)
    
    if(irtx!=0 && irty!=0)
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    
    if(!linear){
      output<-data.frame(eqYx=eYCEeAx)      
      if(irtx!=0 && irty!=0){
        output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
        out<-new("keout", scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Unsmoothed NEAT CE equipercentile", equating=output)
        return(out)
      }
      out<-new("keout", scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Unsmoothed NEAT CE equipercentile", equating=output)
      return(out)
    }
    h<-data.frame(hxP, hyQ, haP, haQ)
    output<-data.frame(eqYx=eYCEeAx)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
      out<-new("keout", scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Unsmoothed NEAT CE linear", equating=output)
      return(out)
    }
    out<-new("keout", scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Unsmoothed NEAT CE linear", equating=output)
    return(out)
  }
  
  if(is(P, "glm")){
    if(is(P$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmP<-P
    N<-sum(glmP$y)
    P<-matrix(glmP$fitted.values/N, nrow=length(x))
    DMP<-glmP$x[,-1]
    Pobs<-matrix(glmP$y/N, nrow=length(x))
  }
  else{
    Pobs<-matrix(0)
  }
  if(is(Q, "glm")){
    if(is(Q$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmQ<-Q
    M<-sum(glmQ$y)
    Q<-matrix(glmQ$fitted.values/M, nrow=length(y))
    DMQ<-glmQ$x[,-1]
    Qobs<-matrix(glmQ$y/M, nrow=length(y))
  }
  else{
    Qobs=matrix(0)
  }
  if(is(P, "numeric"))
    P<-matrix(P, nrow=length(x))
  if(is(Q, "numeric"))
    Q<-matrix(Q, nrow=length(y))
  stopifnot(is(x, "numeric"), is(y, "numeric"), is(a, "numeric"), is(P,"matrix"), is(Q,"matrix"), is(DMP, "matrix"), is(DMQ,"matrix"), 
            is(N,"numeric"), is(M,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
  
  if(kernel=="uniform"){
    ulimit<-(1/(2*bunif*(1-0.61803)))
    KPEN<-0
  }
  else
    ulimit<-4
  rP<-rowSums(P)
  tP<-colSums(P)
  sQ<-rowSums(Q)
  tQ<-colSums(Q)
  
  meanx<-x%*%rP
  meany<-y%*%sQ
  meanaP<-a%*%tP
  meanaQ<-a%*%tQ
  varx<-(N/(N-1))*(x^2%*%rP-meanx^2)
  vary<-(M/(M-1))*(y^2%*%sQ-meany^2)
  varaP<-(N/(N-1))*(a^2%*%tP-meanaP^2)
  varaQ<-(M/(M-1))*(a^2%*%tQ-meanaQ^2)
  if(linear){
    if(hxPlin==0)
      hxP<-as.numeric(1000*sqrt(varx))
    else
      hxP<-hxPlin
    if(hyQlin==0)
      hyQ<-as.numeric(1000*sqrt(vary))
    else
      hyQ<-hyQlin
    if(haPlin==0)
      haP<-as.numeric(1000*sqrt(varaP))
    else
      haP<-haPlin
    if(haQlin==0)
      haQ<-as.numeric(1000*sqrt(varaQ))
    else
      haQ<-haQlin
  }
  else{
    if(hxP==0)
      if(altopt)
        hxP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
      else{
        if(KPEN==0)
          hxP<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hxPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hxP<-optimize(PEN, interval=c(hxPPEN1min, ulimit), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    if(hyQ==0)
      if(altopt)
        hyQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
      else{
        if(KPEN==0)
          hyQ<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hyQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hyQ<-optimize(PEN, interval=c(hyQPEN1min, ulimit), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    if(haP==0)
      if(altopt)
        haP <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varaP)
      else{
        if(KPEN==0)
          haP<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          haPPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          haP<-optimize(PEN, interval=c(haPPEN1min, ulimit), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    if(haQ==0)
      if(altopt)
        haQ <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(varaQ)
      else{
        if(KPEN==0)
          haQ<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          haQPEN1min<-optimize(PEN, interval=c(0, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          haQ<-optimize(PEN, interval=c(haQPEN1min, ulimit), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    hxPlin<-as.numeric(1000*sqrt(varx))
    hyQlin<-as.numeric(1000*sqrt(vary))
    haPlin<-as.numeric(1000*sqrt(varaP))
    haQlin<-as.numeric(1000*sqrt(varaQ))
  }
  
  cdfxP<-cdf(rP, hxP, meanx, varx, kernel, slog, bunif)
  cdfyQ<-cdf(sQ, hyQ, meany, vary, kernel, slog, bunif)
  cdfaP<-cdf(tP, haP, meanaP, varaP, kernel, slog, bunif)
  cdfaQ<-cdf(tQ, haQ, meanaQ, varaQ, kernel, slog, bunif)
  
  if(length(x)>100){
    Cp<-cmatris(as.vector(P), DMP, N)
    Cq<-cmatris(as.vector(Q), DMQ, M)
  } else{
    Cp<-cmatrixSG(as.vector(P), DMP, N)
    Cq<-cmatrixSG(as.vector(Q), DMQ, M)
  }
  #Want to link tests X and A (not equal length)
  #Want to find the values of the continuized cdf for A on Q for the "linked" values from X to A on P.
  
  eAx<-eqinvCE(cdfxP, tP, x, a, varaP, meanaP, haP, kernel, slog, bunif)		  #Linked scores from X to A on P
  cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a, kernel, slog, bunif)		  #cdf of the linked values on Q
  eYCEeAx<-eqinvCE(cdfeAxQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)	#from eAx on Q to Y
  
  eAy<-eqinvCE(cdfyQ, tQ, x, a, varaQ, meanaQ, haQ, kernel, slog, bunif)		#from Y to A on Q
  cdfeAyP<-cdfce(tP, haP, meanaP, varaP, eAy, a, kernel, slog, bunif)		#cdf of the linked values on P
  eXCEeAy<-eqinvCE(cdfeAyP, rP, x, x, varx, meanx, hxP, kernel, slog, bunif)	#from eAy on P to X
  
  FprimeA<-densityeq(rP, hxP, varx, meanx, x, x, kernel, slog, bunif)
  HpprimeA<-densityeq(tP, haP, varaP, meanaP, eAx, a, kernel, slog, bunif)
  HqprimeY<-densityeq(tQ, haQ, varaQ, meanaQ, eAx, a, kernel, slog, bunif)
  GprimeY<-densityeq(sQ, hyQ, vary, meany, eYCEeAx, y, kernel, slog, bunif)
  GprimeA<-densityeq(sQ, hyQ, vary, meany, y, y, kernel, slog, bunif)
  HqprimeA<-densityeq(tQ, haQ, varaQ, meanaQ, eAy, a, kernel, slog, bunif)
  
  if(altopt){
    altFprimeA <- altoptdensity(rP, hxP, varx, meanx, x, x)
    altHpprimeA <- altoptdensity(tP, haP, varaP, meanaP, eAx, a)
    altHqprimeY <- altoptdensity(tQ, haQ, varaQ, meanaQ, eAx, a)
    altGprimeY <- altoptdensity(sQ, hyQ, vary, meany, eYCEeAx, y)
    altGprimeA <- altoptdensity(sQ, hyQ, vary, meany, y, y)
    altHqprimeA <- altoptdensity(tQ, haQ, varaQ, meanaQ, eAy, a)
    dFdrPeA <- dFdr(rP, hxP, varx, meanx, FprimeA, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altFprimeA)
    dHpdtPeA <- dFdr(tP, haP, varaP, meanaP, HpprimeA, eAx, a, kernel, slog, bunif, altopt=altopt, altfprim=altHpprimeA)
    dHqdteY <- dFdr(tQ, haQ, varaQ, meanaQ, HqprimeY, eAx, a, kernel, slog, bunif, altopt=altopt, altfprim=altHqprimeY)
    dGdseY <- dFdr(sQ, hyQ, vary, meany, GprimeY, eYCEeAx, y, kernel, slog, bunif, altopt=altopt, altfprim=altGprimeY)
    dGdsQeA <- dFdr(sQ, hyQ, vary, meany, GprimeA, y, y, kernel, slog, bunif, altopt=altopt, altfprim=altGprimeA)
    dHqdtPeA <- dFdr(tQ, haQ, varaQ, meanaQ, HqprimeA, eAy, a, kernel, slog, bunif, altopt=altopt, altfprim=altHqprimeA)    
  }
  else{
    dFdrPeA<-dFdr(rP, hxP, varx, meanx, FprimeA, x, x, kernel, slog, bunif)
    dHpdtPeA<-dFdr(tP, haP, varaP, meanaP, HpprimeA, eAx, a, kernel, slog, bunif)
    dHqdteY<-dFdr(tQ, haQ, varaQ, meanaQ, HqprimeY, eAx, a, kernel, slog, bunif) #check
    dGdseY<-dFdr(sQ, hyQ, vary, meany, GprimeY, eYCEeAx, y, kernel, slog, bunif)
    dGdsQeA<-dFdr(sQ, hyQ, vary, meany, GprimeA, y, y, kernel, slog, bunif)
    dHqdtPeA<-dFdr(tQ, haQ, varaQ, meanaQ, HqprimeA, eAy, a, kernel, slog, bunif)
  }
  
  eYa<-eqinvCE(cdfaQ, sQ, x, y, vary, meany, hyQ, kernel, slog, bunif)
  PREAx<-PREp(eAx, a, rP, tP)
  PREYa<-PREp(eYa, y, tQ, sQ)
  PRE<-data.frame(PREAx, PREYa)
  h<-data.frame(hxP, hyQ, haP, haQ, hxPlin, hyQlin, haPlin, haQlin)
  
  Up<-matrix(0, length(x), ncol=dim(DMP)[2])
  i<-1
  while(i<dim(DMP)[1]){
    Up<-Cp[i:(i+length(x)-1),]+Up
    i<-i+length(x)
  }
  Uq<-matrix(0, length(y), ncol=dim(DMQ)[2])
  i<-1
  while(i<dim(DMQ)[1]){
    Uq<-Cq[i:(i+length(y)-1),]+Uq
    i<-i+length(y)
  }
  Vp<-matrix(0, length(a), ncol=dim(DMP)[2])
  I<-t(matrix(1, length(x), ncol=1))
  i<-1
  j<-1
  while(i<dim(DMP)[1]){
    Vp[j,1:dim(DMP)[2]]<-I%*%Cp[i:(length(x)+i-1),]
    i<-i+length(x)
    j<-j+1
  }
  
  Vq<-matrix(0, length(a), ncol=dim(DMQ)[2])
  I<-t(matrix(1, length(y), ncol=1))
  i<-1
  j<-1
  while(i<dim(DMQ)[1]){
    Vq[j,1:dim(DMQ)[2]]<-I%*%Cq[i:(length(y)+i-1),]
    i<-i+length(y)
    j<-j+1
  }
  
  SEEYx<-numeric(length(x))
  SEEYxmat<-matrix(0, length(x), dim(Up)[2]+dim(Uq)[2])                                                         #Prepare matrix containing the SEE-vectors
  SEEYxmat[,1:dim(Up)[2]]<-(HqprimeY/GprimeY)*(1/HpprimeA)*(dFdrPeA%*%Up-dHpdtPeA%*%Vp)                                            #Fill matrix
  SEEYxmat[,(dim(Up)[2]+1):(dim(Up)[2]+dim(Uq)[2])]<-(1/GprimeY)*(dHqdteY%*%Vq-dGdseY%*%Uq)    
  SEEYx<-(sqrt(rowSums(SEEYxmat^2)))                                                                            #SEE for equating x to Y
  
  if(irtx!=0 && irty!=0){
    irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
  }
  
  if(!linear){
    CElin<-kequate("NEAT_CE", a=a, x=x, y=y, P=P, Q=Q, N=N, M=M, DMP=DMP, DMQ=DMQ, hxPlin=hxPlin, hyQlin=hxPlin, haPlin=hxPlin, haQlin=hxPlin, KPEN=KPEN, linear=TRUE, kernel="gaussian")
    SEEvectors<-new("SEEvect", SEEYx=SEEYxmat, SEEYxLIN=CElin@SEEvect@SEEYxLIN)
    SEEDYx<-sqrt(rowSums((SEEYxmat-SEEvectors@SEEYxLIN)^2))
    output<-data.frame(eqYx=eYCEeAx, SEEYx=SEEYx, SEEDYx, eqYxLIN=CElin@equating$eqYx, SEEYxLIN=CElin@equating$SEEYx)      
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx=SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEEDYx, eqYxLIN=CElin@equating$eqYx,  SEEYxLIN=CElin@equating$SEEYx)
      out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="NEAT CE equipercentile", equating=output)
      return(out)
    }
    out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="NEAT CE equipercentile", equating=output)
    return(out)
  }
  
  h<-data.frame(hxP, hyQ, haP, haQ)
  SEEvectlin<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEEYxmat)
  output<-data.frame(eqYx=eYCEeAx, SEEYx=SEEYx)
  if(irtx!=0 && irty!=0){
    output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx=SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT)
    out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectlin, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="NEAT CE linear", equating=output)
    return(out)      
  }
  out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectlin, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r=rP, cdfx=cdfxP), Y=data.frame(y, s=sQ, cdfy=cdfyQ), A=data.frame(a, tP, cdfaP, tQ, cdfaQ), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="NEAT CE linear", equating=output)
  return(out)
}

kequateNEAT_PSE<-function(x, y, P, Q, DMP, DMQ, N, M, w=0.5, hx=0, hy=0, hxlin=0, hylin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE, kernel="gaussian", slog=1, bunif=0.5, altopt=FALSE){
  #    stopifnot((is(smoothed, "logical"))
  if(smoothed==FALSE){
    
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    }
    else
      ulimit<-4
    tP<-colSums(P)
    tQ<-colSums(Q)
    sumr<-numeric(dim(P)[1])
    for(i in 1:dim(P)[2])
      sumr<-sumr+(w+(1-w)*(tQ[i]/tP[i]))*P[,i] 
    sums<-numeric(dim(Q)[1])
    for(i in 1:dim(Q)[2])
      sums<-sums+(1-w+w*(tP[i]/tQ[i]))*Q[,i]
    r<-sumr
    s<-sums
    
    meanx<-x%*%r
    varx<-(N/(N-1))*(x^2%*%r-meanx^2)
    meany<-y%*%s
    vary<-(M/(M-1))*(y^2%*%s-meany^2)
    
    if(linear){
      if(hxlin==0){
        hx<-as.numeric(1000*sqrt(varx))
      }
      else{
        hx<-hxlin
      }
      if(hylin==0){
        hy<-as.numeric(1000*sqrt(vary))
      }
      else{
        hy<-hylin
      }
    }
    else{
      if(hx==0)
        if(altopt)
          hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
      else{
        if(KPEN==0)
          hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      if(hy==0)
        if(altopt)
          hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
      else{
        if(KPEN==0)
          hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    }
    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif, smoothed=FALSE)
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    if(irtx!=0 && irty!=0)
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    
    if(!linear){
      output<-data.frame(eqYx)
      if(irtx!=0 && irty!=0){
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
        out<-new("keout", scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Equipercentile NEAT PSE without pre-smoothing", equating=output)
        return(out)
      }
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Equipercentile NEAT PSE without pre-smoothing", equating=output)
      return(out)
    }
    output<-data.frame(eqYx)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Linear NEAT PSE without pre-smoothing", equating=output)
      return(out)
    }
    out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Linear NEAT PSE without pre-smoothing", equating=output)
    return(out)
  }
  
  if(is(P, "glm")){
    if(is(P$x, "list")){
      stop("The design matrix must be included in the glm object.")
    }
    glmP<-P
    N<-sum(glmP$y)
    P<-matrix(glmP$fitted.values/N, nrow=length(x))
    DMP<-glmP$x[,-1]
    Pobs<-matrix(glmP$y/N, nrow=length(x))
  }
  else{
    Pobs<-matrix(0)
  }
  if(is(Q, "glm")){
    if(is(Q$x, "list")){
      stop("The design matrix must be included in the glm object.")
    }
    glmQ<-Q
    M<-sum(glmQ$y)
    Q<-matrix(glmQ$fitted.values/M, nrow=length(y))
    DMQ<-glmQ$x[,-1]
    Qobs<-matrix(glmQ$y/M, nrow=length(y))
  }
  else{
    Qobs<-matrix(0)
  }
  if(is(P, "numeric"))
    P<-matrix(P, nrow=length(x))
  if(is(Q, "numeric"))
    Q<-matrix(Q, nrow=length(y))
  stopifnot(is(x, "numeric"), is(P,"matrix"), is(Q,"matrix"), is(DMP, "matrix"), is(DMQ,"matrix"), is(N,"numeric"), 
            is(w, "numeric"), is(M,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
  
  if(kernel=="uniform"){
    ulimit<-(1/(2*bunif*(1-0.61803)))
    KPEN<-0
  }
  else
    ulimit<-4
  tP<-colSums(P)
  tQ<-colSums(Q)
  sumr<-numeric(dim(P)[1])
  for(i in 1:dim(P)[2])
    sumr<-sumr+(w+(1-w)*(tQ[i]/tP[i]))*P[,i] 
  sums<-numeric(dim(Q)[1])
  for(i in 1:dim(Q)[2])
    sums<-sums+(1-w+w*(tP[i]/tQ[i]))*Q[,i]
  r<-sumr
  s<-sums
  
  meanx<-x%*%r
  varx<-(N/(N-1))*(x^2%*%r-meanx^2)
  meany<-y%*%s
  vary<-(M/(M-1))*(y^2%*%s-meany^2)
  
  if(linear){
    if(hxlin==0){
      hx<-as.numeric(1000*sqrt(varx))
    }
    else{
      hx<-hxlin
    }
    if(hylin==0){
      hy<-as.numeric(1000*sqrt(vary))
    }
    else{
      hy<-hylin
    }
  }
  else{
    if(hx==0)
      if(altopt)
        hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
    else{
      if(KPEN==0)
        hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(hy==0)
      if(altopt)
        hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
    else{
      if(KPEN==0)
        hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
  }  
  cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
  cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
  if(length(x)>100){
    Cp<-cmatris(as.vector(P), DMP, N)
    Cq<-cmatris(as.vector(Q), DMQ, M)
  } else{
    Cp<-cmatrixSG(as.vector(P), DMP, N)
    Cq<-cmatrixSG(as.vector(Q), DMQ, M)
  }
  
  eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)  							#Calculate the equated score values.
  #eqXy<-eqinv(cdfy, r, x, varx, meanx, hx, kernel, slog, bunif)
  
  gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)							#G' for eY
  fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)							#F' for eY
  
  if(altopt){
    altfprim <- altoptdensity(r, hx, varx, meanx, x, x)
    altgprim <- altoptdensity(s, hy, vary, meanx, eqYx, y)
  }
  if(altopt){
    dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altfprim)  					#Matrices of derivatives. JxJ.
    dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif, altopt=altopt, altfprim=altgprim)	
  }
  else{
    dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
    dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)						#KxK
  }
  Up<-matrix(0, length(x), ncol=dim(Cp)[2])
  Ups<-matrix(0, length(x), ncol=dim(Cp)[2])
  Uq<-matrix(0, length(y), ncol=dim(Cq)[2])
  Uqs<-matrix(0, length(y), ncol=dim(Cq)[2])
  Vp<-matrix(0, dim(Cp)[1]/length(x), ncol=dim(Cp)[2])
  Vq<-matrix(0, dim(Cq)[1]/length(y), ncol=dim(Cq)[2])
  
  i<-1
  while(i<=(dim(Cp)[1])){
    Up<-Up+Cp[i:(length(x)+i-1),]
    i<-i+length(x)
  }
  i<-1
  j<-1
  while(i<=(dim(Cp)[1])){
    Ups<-Ups+(tQ[j]/tP[j])*Cp[i:(i+length(x)-1),]
    i<-i+length(x)
    j<-j+1
  }
  
  i<-1
  while(i<=(dim(Cq)[1])){
    Uq<-Uq+Cq[i:(i+length(y)-1),]
    i<-i+length(y)
  }
  i<-1
  j<-1
  while(i<=(dim(Cq)[1])){
    Uqs<-Uqs+(tP[j]/tQ[j])*Cq[i:(i+length(y)-1),]
    i<-i+length(y)
    j<-j+1
  }
  i<-1
  j<-0
  while(i<=dim(Cp)[1]/length(x)){
    Vp[i,]<-t(matrix(1,length(x), ncol=1))%*%Cp[(j+1):(j+length(x)),]
    i<-i+1
    j<-j+length(x)
  }
  i<-1
  j<-0
  while(i<=dim(Cq)[1]/length(y)){
    Vq[i,]<-t(matrix(1,length(y), ncol=1))%*%Cq[(j+1):(j+length(y)),]
    i<-i+1
    j<-j+length(y)
  }
  
  Ur<-matrix(0, length(x), ncol=dim(Cp)[2])
  Vr<-matrix(0, length(y), ncol=dim(Cp)[2])
  Us<-matrix(0, length(x), ncol=dim(Cq)[2])
  Vs<-matrix(0, length(y), ncol=dim(Cq)[2])
  
  sumUr<-matrix(0, length(x), ncol=dim(Cp)[2])
  for(i in 1:(dim(Cp)[1]/length(x)))
    sumUr<-sumUr+(tQ[i]/tP[i])*(1/tP[i])*P[,i]%*%t(Vp[i,])
  sumVr<-matrix(0, length(y), ncol=dim(Cp)[2])
  for(i in 1:(dim(Cp)[1]/length(x)))
    sumVr<-sumVr+(1/tQ[i])*Q[,i]%*%t(Vp[i,])
  sumUs<-matrix(0, length(x), ncol=dim(Cq)[2])
  for(i in 1:(dim(Cq)[1]/length(y)))
    sumUs<-sumUs+(1/tP[i])*P[,i]%*%t(Vq[i,])
  sumVs<-matrix(0, length(y), ncol=dim(Cq)[2])
  for(i in 1:(dim(Cq)[1]/length(y)))
    sumVs<-sumVs+(tP[i]/tQ[i])*(1/tQ[i])*Q[,i]%*%t(Vq[i,])
  
  Ur<-w*Up+(1-w)*Ups-(1-w)*sumUr
  Vr<-w*sumVr
  Us<-(1-w)*sumUs
  Vs<-(1-w)*Uq+w*Uqs-w*sumVs
  
  SEEYx<-SEE_EY(gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs)
  PREYx<-PREp(eqYx, y, r, s)
  PRE<-data.frame(PREYx)
  h<-data.frame(hx, hy)
  
  if(irtx!=0 && irty!=0){
    irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
  }
  
  if(!linear){
    SEED_NEAT<-SEED(fprimeY, gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin, slog, bunif)
    output<-data.frame(eqYx, SEEYx, SEED_NEAT@SEED)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEED_NEAT@SEED)
      out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEED_NEAT@SEEvect, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_NEAT@hlin, irtout@h), kernel=kernel, type="NEAT/NEC PSE equipercentile", equating=output)
      return(out)
    }
    out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEED_NEAT@SEEvect,Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_NEAT@hlin), kernel=kernel, type="NEAT/NEC PSE equipercentile", equating=output)
    return(out)
  }
  SEE_EYlin<-matrix(nrow=length(x),ncol=(length(dFdreYEG[1,]%*%Ur)+length(dFdreYEG[1,]%*%Us)))
  for(i in 1:length(x)){
    SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYEG[i,]%*%Ur-dGdseYEG[i,]%*%Vr, dFdreYEG[i,]%*%Us-dGdseYEG[i,]%*%Vs)
  }
  SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEE_EYlin)
  output<-data.frame(eqYx, SEEYx)
  if(irtx!=0 && irty!=0){
    output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT)
    out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="NEAT/NEC PSE linear", equating=output)
    return(out)
  }
  out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="NEAT/NEC PSE linear", equating=output)
  return(out)
}

kequateSG<-function(x, y, P, DM, N, hx=0, hy=0, hxlin=0, hylin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE, kernel="gaussian", slog=1, bunif=0.5, altopt=FALSE){
  #stopifnot((is(smoothed, "logical"))
  if(smoothed==FALSE){
    if(kernel=="uniform"){
      ulimit<-(1/(2*bunif*(1-0.61803)))
      KPEN<-0
    }
    else
      ulimit<-4
    if(!is.matrix(P)){
      P<-matrix(P, nrow=length(x))
    }
    r<-rowSums(P)
    s<-colSums(P)
    M<-N
    meanx<-x%*%r
    varx<-(N/(N-1))*(x^2%*%r-meanx^2)
    meany<-y%*%s
    vary<-(N/(N-1))*(y^2%*%s-meany^2)
    if(linear){
      if(hxlin==0){
        hx<-as.numeric(1000*sqrt(varx))
      }
      else{
        hx<-hxlin
      }
      if(hylin==0){
        hy<-as.numeric(1000*sqrt(vary))
      }
      else{
        hy<-hylin
      }
    }
    else{
      if(hx==0)
        if(altopt)
          hx <- 9 / sqrt(100 * N^(2/5) - 81) * sqrt(varx)
      else{
        if(KPEN==0)
          hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
      if(hy==0)
        if(altopt)
          hy <- 9 / sqrt(100 * M^(2/5) - 81) * sqrt(vary)
      else{
        if(KPEN==0)
          hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        else{
          hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
          hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
        }
      }
    }

    cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
    cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
    
    eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif, smoothed=FALSE)
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    if(irtx!=0 && irty!=0)
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
    
    if(!linear){
      output<-data.frame(eqYx)
      if(irtx!=0 && irty!=0){
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
        out<-new("keout", scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Equipercentile SG without pre-smoothing", equating=output)
        return(out)
      }
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Equipercentile SG without pre-smoothing", equating=output)
      return(out)
    }
    output<-data.frame(eqYx)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
      out<-new("keout", scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, irtout@h), kernel=kernel, type="Linear SG without pre-smoothing", equating=output)
      return(out)
    }
    out<-new("keout", scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=h, kernel=kernel, type="Linear SG without pre-smoothing", equating=output)
    return(out)
  }
  
  if(is(P, "glm")){
    if(is(P$x, "list"))
      stop("The design matrix must be included in the glm object.")
    glmP<-P
    N<-sum(glmP$y)
    P<-matrix(glmP$fitted.values/N, nrow=length(x))
    DM<-glmP$x[,-1]
    Pobs<-matrix(glmP$y/N, nrow=length(x))
  }
  else
    Pobs<-matrix(0)
  stopifnot(is(x, "numeric"), is(P,"matrix"), is(DM, "matrix"), is(N,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
  
  if(kernel=="uniform"){
    ulimit<-(1/(2*bunif*(1-0.61803)))
    KPEN<-0
  }
  else
    ulimit<-4

  J<-dim(P)[1]
  K<-dim(P)[2]
  r<-rowSums(P)
  s<-colSums(P)
  M<-N
  meanx<-x%*%r
  varx<-(N/(N-1))*(x^2%*%r-meanx^2)
  meany<-y%*%s
  vary<-(N/(N-1))*(y^2%*%s-meany^2)
  
  if(linear){
    if(hxlin==0){
      hx<-as.numeric(1000*sqrt(varx))
    }
    else{
      hx<-hxlin
    }
    if(hylin==0){
      hy<-as.numeric(1000*sqrt(vary))
    }
    else{
      hy<-hylin
    }
  }
  else{
    if(hx==0)
      if(altopt)
        hx <- 9 / sqrt(100 * N^(2/5) - 81) *  sqrt(varx)
    else{
      if(KPEN==0)
        hx<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hxPEN1min<-optimize(PEN, interval=c(0, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hx<-optimize(PEN, interval=c(hxPEN1min, ulimit), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
    if(hy==0)
      if(altopt)
        hy <- 9 / sqrt(100 * M^(2/5) - 81) *  sqrt(vary)
    else{
      if(KPEN==0)
        hy<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      else{
        hyPEN1min<-optimize(PEN, interval=c(0, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=0, kernel=kernel, slog=slog, bunif=bunif)$minimum
        hy<-optimize(PEN, interval=c(hyPEN1min, ulimit), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN, kernel=kernel, slog=slog, bunif=bunif)$minimum
      }
    }
  }
  
  cdfx<-cdf(r, hx, meanx, varx, kernel, slog, bunif)
  cdfy<-cdf(s, hy, meany, vary, kernel, slog, bunif)
  if(length(x)>100){
    Cp<-cmatris(as.vector(P), DM, N)
  } else{
    Cp<-cmatrixSG(as.vector(P), DM, N)
  }
  
  eqYx<-eqinvCE(cdfx, s, x, y, vary, meany, hy, kernel, slog, bunif)
  
  gprimeY<-densityeq(s, hy, vary, meany, eqYx, y, kernel, slog, bunif)
  fprimeY<-densityeq(r, hx, varx, meanx, x, x, kernel, slog, bunif)
  
  if(altopt){
    altfprim <- altoptdensity(r, hx, varx, meanx, x, x)
    altgprim <- altoptdensity(s, hy, vary, meanx, eqYx, y)
  }
  if(altopt){
    dFdreYSG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif, altopt=altopt, altfprim=altfprim)  					#Matrices of derivatives. JxJ.
    dGdseYSG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif, altopt=altopt, altfprim=altgprim)	
  }
  else{
    dFdreYSG<-dFdr(r, hx, varx, meanx, fprimeY, x, x, kernel, slog, bunif)						#Matrices of derivatives. JxJ.
    dGdseYSG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y, kernel, slog, bunif)						#KxK
  }

  Ur<-matrix(0, length(x), ncol=dim(DM)[2])
  i<-1
  while(i<length(x)*length(y)){
    Ur<-Cp[i:(i+length(x)-1),]+Ur
    i<-i+length(x)
  }
  
  Vr<-matrix(0, length(y), ncol=dim(DM)[2])
  I<-t(matrix(1, length(x), ncol=1))
  i<-1
  j<-1
  while(i<(length(x)*length(y))){
    Vr[j,1:dim(DM)[2]]<-I%*%Cp[i:(length(x)+i-1),]
    i<-i+length(x)
    j<-j+1
  }
  
  Us<-matrix(0, ncol=dim(DM)[2], nrow=length(x))
  Vs<-matrix(0, ncol=dim(DM)[2], nrow=length(y))
  
  SEEYx<-SEE_EY(gprimeY, dFdreYSG, dGdseYSG, Ur, Vr, Us, Vs)
  PREYx<-PREp(eqYx, y, r, s)
  PRE<-data.frame(PREYx)
  h<-data.frame(hx, hy)
  
  if(irtx!=0 && irty!=0){
    irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear, kernel, slog, bunif)
  }
  if(!linear){
    SEED_SG<-SEED(fprimeY, gprimeY, dFdreYSG, dGdseYSG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin, slog, bunif)
    output<-data.frame(eqYx, SEEYx, SEED_SG@SEED)
    if(irtx!=0 && irty!=0){
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEED_SG@SEED)
      out<-new("keout", Cp=Cp, SEEvect=SEED_SG@SEEvect, Pest=P, Pobs=Pobs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_SG@hlin, irtout@h), kernel=kernel, type="SG equipercentile", equating=output)
      return(out)
    }
    out<-new("keout", Cp=Cp, SEEvect=SEED_SG@SEEvect, Pest=P, Pobs=Pobs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(h, SEED_SG@hlin), kernel=kernel, type="SG equipercentile", equating=output)
    return(out)
  }
   SEE_EYlin<-matrix(nrow=length(x),ncol=(length(dFdreYSG[1,]%*%Ur)+length(dFdreYSG[1,]%*%Us)))
   for(i in 1:length(x)){
     SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYSG[i,]%*%Ur-dGdseYSG[i,]%*%Vr, dFdreYSG[i,]%*%Us-dGdseYSG[i,]%*%Vs)
   }
  SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEE_EYlin)
  output<-data.frame(eqYx, SEEYx)
  if(irtx!=0 && irty!=0){
    output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT)
    out<-new("keout", Cp=Cp, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, scores=list(X=data.frame(x, r, cdfx, cdfxirt=irtout@scores$X$cdfxirt), Y=data.frame(y, s, cdfy, cdfyirt=irtout@scores$Y$cdfyirt), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hx, hy, irtout@h), kernel=kernel, type="SG linear", equating=output)
    return(out)
  }
  out<-new("keout", Cp=Cp, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, scores=list(X=data.frame(x, r, cdfx), Y=data.frame(y, s, cdfy), M=M, N=N), linear=linear, PRE=PRE, h=data.frame(hx, hy), kernel=kernel, type="SG linear", equating=output)
  return(out)
}

linint<-function(x, a, r, s){
  Px<-numeric(length(x))
  Fx<-numeric(length(x))
  Gy<-numeric(length(a)+1)
  yU<-numeric(length(x))
  eqYx<-numeric(length(x))
  Gy[1]<-0
  for(i in 1:length(x))
    Fx[i]<-sum(r[1:i])
  for(i in 1:length(a))
    Gy[i+1]<-sum(s[1:i])
  Px[1]<-100*(0.5*Fx[1])
  for(i in 2:length(x))
    Px[i]<-100*(Fx[i-1]+(0.5*(Fx[i]-Fx[i-1])))
  for(i in 1:length(x)){
    yU[i]<-length(a)-length(Gy[Gy>(Px[i]/100)])
    eqYx[i]<-yU[i]-0.5+(Px[i]/100-Gy[yU[i]+1])/(Gy[yU[i]+2]-Gy[yU[i]+1])
  }
  return(eqYx)
}

LordW<-function(p){
  if(!is.matrix(p))
		p<-as.matrix(p)
  n<-dim(p)[1]
  N<-dim(p)[2]
  test<-array(0, c(n+1, N, n))
  q<-1-p
  l<-1
  test[1,,l]<-q[1,]
  test[2,,l]<-p[1,]
  testR<-test[1,,l]
  testR2<-test[2,,l]
  
  for(i in 2:n){
    for(r in 1:(i+1)){
      if(r==1)
        testR<-testR*q[i,]
      if(r>1 && r<(i+1))
        test[r,,l+1]<-test[r,,l]*q[i,]+test[r-1,,l]*p[i,]
      if(r==(i+1)){
        testR2<-testR2*p[i,]
        test[r,,l+1]<-testR2
      }
    }
    test[1,,l+1]<-testR
    l<-l+1
  }
	if(N==1)
		return(test[,,n])
	else
		return(rowSums(test[,,n])/N)
}

LordWW <- function(p, qpoints){
  if(!is.matrix(p))
    p<-as.matrix(p)
  n<-dim(p)[1]
  N<-dim(p)[2]
  test<-array(0, c(n+1, N, n))
  q<-1-p
  l<-1
  test[1,,l]<-q[1,]
  test[2,,l]<-p[1,]
  testR<-test[1,,l]
  testR2<-test[2,,l]
  
  for(i in 2:n){
    for(r in 1:(i+1)){
      if(r==1)
        testR<-testR*q[i,]
      if(r>1 && r<(i+1))
        test[r,,l+1]<-test[r,,l]*q[i,]+test[r-1,,l]*p[i,]
      if(r==(i+1)){
        testR2<-testR2*p[i,]
        test[r,,l+1]<-testR2
      }
    }
    test[1,,l+1]<-testR
    l<-l+1
  }
  #Weight each quadrature point, ~N(0,1) assumption on the ability (from IRT model)
  test[,,n] <- t(t(test[,,n]) * (dnorm(qpoints)/sum(dnorm(qpoints))))
  
  if(N==1)
    return(test[,,n])
  else
    return(rowSums(test[,,n]))
}

LordWWc <- function(p, qpoints){
  if(!is.matrix(p))
    p<-as.matrix(p)
  n<-dim(p)[1]
  N<-dim(p)[2]
  test<-array(0, c(n+1, N, n))
  q<-1-p
  l<-1
  test[1,,l]<-q[1,]
  test[2,,l]<-p[1,]
  testR<-test[1,,l]
  testR2<-test[2,,l]
  
  for(i in 2:n){
    for(r in 1:(i+1)){
      if(r==1)
        testR<-testR*q[i,]
      if(r>1 && r<(i+1))
        test[r,,l+1]<-test[r,,l]*q[i,]+test[r-1,,l]*p[i,]
      if(r==(i+1)){
        testR2<-testR2*p[i,]
        test[r,,l+1]<-testR2
      }
    }
    test[1,,l+1]<-testR
    l<-l+1
  }
  return(as.matrix(test[,,n]))
}

pderpl <- function(irt, qpoints, b, model = "3pl", a = 0, c = 0){
  #irt - the probabilities to answer each item (rows) correctly for the quadrature points (columns) considered
  #qpoints - the quadrature points considered
  #b - the difficulty parameter for each item
  #model - "1pl", "2pl" or "3pl"
  #a - the discrimination parameter for each item
  #c - the guessing parameter for each item
  cprobs <- array(0, dim = c(length(irt[1,]), length(b), (length(b)+1)))
  probs <- matrix(0, ncol = length(irt[1,]), nrow = (length(b)+1))
  #cprobs - probabilities to achieve each score while answering each question correctly, for each ability level
  #probs - probability to achieve each score for each ability level (local equating score probs)

  probs <- LordWWc(irt, qpoints)
  for(j in 1:length(irt[,1])){
    input <- as.matrix(irt)
    input[j,] <- 1
    cprobs[,j,] <- t(LordWWc(input, qpoints))
  }
  #print(cprobs)
  if(model=="1pl"){
    pBpalpha <- matrix(0, nrow = length(b), ncol = (length(b)+1))
    Bx <- matrix(0, nrow=length(b), ncol=length(qpoints))
    #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point
    for(i in 1:(length(b)+1)){
      Bx <- (dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(cprobs[,,i] - probs[i,]) * irt
      #Weight each quadrature point under ~N(0,1) assumption
      pBpalpha[,i] <- rowSums(Bx)
      #Put the partial derivatives for each score value in the final matrix
    }
  }
  
  if(model=="2pl"){
    pBpalpha <- matrix(0, nrow = 2*length(b), ncol = (length(b)+1))
    #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
    Bx <- matrix(0, nrow=(2*length(b)), ncol=length(qpoints))
    #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
    for(i in 1:(length(b)+1)){
      tmat <- cprobs[,,i] - probs[i,]
      if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
      Bx[1:length(b),] <- t(tmat) * irt  * (-a)
      Bx[(length(b)+1):(length(b)+length(a)),] <- t(tmat * t(irt) * qpoints) - t(tmat) * irt * b
      Bx <- t((dnorm(qpoints) / (sum(dnorm(qpoints)))) * t(Bx))
      #Weight each quadrature point under ~N(0,1) assumption
      pBpalpha[,i] <- rowSums(Bx)
      #Put the partial derivatives for each score value in the final matrix
    }
  }
  
  if(model=="3pl"){
    pBpalpha <- matrix(0, nrow = 3*length(b), ncol = (length(b)+1))
    #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
    Bx <- matrix(0, nrow=(3*length(b)), ncol=length(qpoints))
    #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
    for(i in 1:(length(b)+1)){
      tmat <- cprobs[,,i] - probs[i,]
      if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
      Bx[1:length(c),] <- t(tmat) * (1/(1-c))
      Bx[(length(c)+1):(length(c)+length(b)),] <- t(tmat) * (((irt - c) * (-a)) / (1-c))
      Bx[(length(c)+length(b)+1):(length(c)+length(b)+length(a)),] <- t(tmat * t((irt - c)/ (1-c)) * qpoints) + t(tmat) * (((irt - c) * (-b)) / (1-c)) 
      Bx <- t((dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(Bx))
      #Weight each quadrature point under ~N(0,1) assumption
      pBpalpha[,i] <- rowSums(Bx)
      #Put the partial derivatives for each score value in the final matrix
    }
  }
  
  return(pBpalpha)
}

pderplpse <- function(irt, qpoints, b, pder, model = "3pl", a = 0, c = 0, betaA, betaB){
  #irt - the probabilities to answer each item (rows) correctly for the quadrature points (columns) considered
  #qpoints - the quadrature points considered
  #pder - which partial derivatives to calculate, "rQaX", "sPaY", "rQeqc", "sPeqc"
  #b - the difficulty parameter for each item
  #model - "1pl", "2pl" or "3pl"
  #a - the discrimination parameter for each item
  #c - the guessing parameter for each item
  cprobs <- array(0, dim = c(length(irt[1,]), length(b), (length(b)+1)))
  probs <- matrix(0, ncol = length(irt[1,]), nrow = (length(b)+1))
  #cprobs - probabilities to achieve each score while answering each question correctly, for each ability level
  #probs - probability to achieve each score for each ability level (local equating score probs)
  
  if(pder=="rQaX"){    
    probs <- LordWWc(irt, betaA * qpoints + betaB)
    for(j in 1:length(irt[,1])){
      input <- as.matrix(irt)
      input[j,] <- 1
      cprobs[,j,] <- t(LordWWc(input, betaA * qpoints + betaB))
    }
    
    if(model=="2pl"){
      pBpalpha <- matrix(0, nrow = 2*length(b), ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=(2*length(b)), ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1:length(b),] <- t(tmat) * irt  * (-a)
        Bx[(length(b)+1):(length(b)+length(a)),] <- t(tmat * t(irt) * betaA * qpoints) - t(tmat) * irt * (b - betaB)
        Bx <- t((dnorm(qpoints) / (sum(dnorm(qpoints)))) * t(Bx))
        #Weight each quadrature point under ~N(0,1) assumption
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    
    if(model=="3pl"){
      pBpalpha <- matrix(0, nrow = 3*length(b), ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=(3*length(b)), ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1:length(c),] <- t(tmat) * (1/(1-c))
        Bx[(length(c)+1):(length(c)+length(b)),] <- t(tmat) * ((irt -  c) * (-a) / (1-c))
        Bx[(length(c)+length(b)+1):(length(c)+length(b)+length(a)),] <- t(tmat * t((irt - c) / (1-c)) * betaA * qpoints) + t(tmat) * ((irt - c) / (1-c)) * (betaB - b)
        Bx <- t((dnorm(qpoints) / (sum(dnorm(qpoints)))) * t(Bx))
        #Weight each quadrature point under ~N(0,1) assumption
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    
    return(pBpalpha)
  }
  
  if(pder=="sPaY"){
    probs <- LordWWc(irt, (qpoints - betaB) / betaA)
    for(j in 1:length(irt[,1])){
      input <- as.matrix(irt)
      input[j,] <- 1
      cprobs[,j,] <- t(LordWWc(input, (qpoints - betaB) / betaA))
    }
    
    if(model=="2pl"){
      pBpalpha <- matrix(0, nrow = 2*length(b), ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=(2*length(b)), ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1:length(b),] <- t(tmat) * irt  * (-a)
        Bx[(length(b)+1):(length(b)+length(a)),] <- t(tmat * t(irt) * (qpoints / betaA)) + t(tmat) * irt * ((-betaA * b - betaB) / betaA)
        Bx <- t((dnorm(qpoints) / (sum(dnorm(qpoints)))) * t(Bx))
        #Weight each quadrature point under ~N(0,1) assumption
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    
    if(model=="3pl"){
      pBpalpha <- matrix(0, nrow = 3*length(b), ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=(3*length(b)), ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1:length(c),] <- t(tmat) * (1/(1-c))
        Bx[(length(c)+1):(length(c)+length(b)),] <- t(tmat) * (((irt - c) * (-a)) / (1-c))
        Bx[(length(c)+length(b)+1):(length(c)+length(b)+length(a)),] <- t(tmat * t((irt - c)/ (1 - c)) * qpoints / betaA) + t(tmat) * ((irt - c) / (1 - c))*((-betaA * b - betaB) / betaA)
        Bx <- t((dnorm(qpoints) / (sum(dnorm(qpoints)))) * t(Bx))
        #Weight each quadrature point under ~N(0,1) assumption
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    return(pBpalpha)
  }
  if(pder=="rQeqc"){
    probs <- LordWWc(irt, betaA * qpoints + betaB)
    for(j in 1:length(irt[,1])){
      input <- as.matrix(irt)
      input[j,] <- 1
      cprobs[,j,] <- t(LordWWc(input, betaA * qpoints + betaB))
    }
    
    if(model=="2pl"){
      pBpalpha <- matrix(0, nrow = 2, ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=2, ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1,] <- colSums(t(tmat * t(irt * a) * qpoints))
        Bx[2,] <- colSums(t(tmat) * (irt * a))
        Bx <- t((dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(Bx))
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    
    if(model=="3pl"){
      pBpalpha <- matrix(0, nrow = 2, ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=2, ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx <- matrix(0, nrow=2, ncol=length(qpoints))
        Bx[1,] <- colSums(t(tmat * t(((irt - c) * a) / (1 - c)) * qpoints))
        Bx[2,] <- colSums(t(tmat) * (((irt - c) * a) / (1 - c)))
        
        #Weight each quadrature point under ~N(0,1) assumption
        Bx <- t((dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(Bx))
        
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    
    #pBpalpha <- matrix(0, nrow = 2, ncol = (length(b)+1))
    return(pBpalpha)
  }
  
  if(pder=="sPeqc"){
    probs <- LordWWc(irt, (qpoints - betaB) / betaA)
    for(j in 1:length(irt[,1])){
      input <- as.matrix(irt)
      input[j,] <- 1
      cprobs[,j,] <- t(LordWWc(input, (qpoints - betaB) / betaA))
    }
    
    if(model=="2pl"){
      pBpalpha <- matrix(0, nrow = 2, ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        Bx <- matrix(0, nrow=2, ncol=length(qpoints))
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1,] <- colSums(t(tmat * t(irt * a) * -(qpoints - betaB)/(betaA^2)))
        Bx[2,] <- colSums(t(tmat) * (irt * a) * (-1 / betaA))
        Bx <- t((dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(Bx))
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    
    if(model=="3pl"){
      pBpalpha <- matrix(0, nrow = 2, ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx <- matrix(0, nrow=2, ncol=length(qpoints))
        Bx[1,] <- colSums(t(tmat * t(((irt - c) * a) / (1 - c)) * -(qpoints - betaB) / (betaA^2)))
        Bx[2,] <- colSums(t(tmat) * (((irt - c) * a) / (1 - c)) *(-1/betaA))
        Bx <- t((dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(Bx))
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
    }
    #pBpalpha <- matrix(0, nrow = 2, ncol = (length(b)+1))
    return(pBpalpha)
  }
  if(pder=="sQaY"){
    cprobs <- array(0, dim = c(length(irt[1,]), length(b), (length(b)+1)))
    probs <- matrix(0, ncol = length(irt[1,]), nrow = (length(b)+1))
    #cprobs - probabilities to achieve each score while answering each question correctly, for each ability level
    #probs - probability to achieve each score for each ability level (local equating score probs)
    probs <- LordWWc(irt, qpoints)
    for(j in 1:length(irt[,1])){
      input <- as.matrix(irt)
      input[j,] <- 1
      cprobs[,j,] <- t(LordWWc(input, qpoints))
    }
    
    if(model=="2pl"){
      pBpalpha <- matrix(0, nrow = 2*length(b), ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=(2*length(b)), ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1:length(b),] <- t(tmat) * irt  * (-a)
        Bx[(length(b)+1):(length(b)+length(a)),] <- t(tmat * t(irt) * qpoints) - t(tmat) * irt * b
        Bx <- t((dnorm(qpoints) / (sum(dnorm(qpoints)))) * t(Bx))
        #Weight each quadrature point under ~N(0,1) assumption
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
      return(pBpalpha)
    }
    
    if(model=="3pl"){
      pBpalpha <- matrix(0, nrow = 3*length(b), ncol = (length(b)+1))
      #pBalpha - Matrix of partial derivatives w/ resp to item parameters across all quadrature points, for each score value
      Bx <- matrix(0, nrow=(3*length(b)), ncol=length(qpoints))
      #Bx Matrix of partial derivatives w/ resp to item parameters for each quadrature point, first c, then b, last a
      for(i in 1:(length(b)+1)){
        tmat <- cprobs[,,i] - probs[i,]
        if(!is.matrix(tmat)) tmat <- t(as.matrix(tmat))
        Bx[1:length(c),] <- t(tmat) * (1/(1-c))
        Bx[(length(c)+1):(length(c)+length(b)),] <- t(tmat) * (((irt - c) * (-a)) / (1-c))
        Bx[(length(c)+length(b)+1):(length(c)+length(b)+length(a)),] <- t(tmat * t((irt - c) / (1-c)) * qpoints)  + t(tmat) * (((irt - c) * (-b)) / (1-c)) 
        Bx <- t((dnorm(qpoints)/(sum(dnorm(qpoints)))) * t(Bx))
        #Weight each quadrature point under ~N(0,1) assumption
        pBpalpha[,i] <- rowSums(Bx)
        #Put the partial derivatives for each score value in the final matrix
      }
      return(pBpalpha)
    }
  }
  
}

# pdereqcoef <- function(irtpars, model, type, kx, ky, ka, betaA, betaB){
#   if(type="MM"){
#     if(model=="3pl"){
#       pBApaAP <- numeric(3*ka)
#       pBBpaAP <- numeric(3*ka)
#       pBApaAQ <- numeric(3*ka)
#       pBBpaAQ <- numeric(3*ka)
#       
#       pBBpaAP[(ka+1):(2*ka)] <- rep(((sum(irtpars$aaQ)) / (sum(irtpars$aaP)^2)) * (sum(irtpars$baQ)/ka), ka)
#       pBBpaAQ[(ka+1):(2*ka)] <- rep(-(1/sum(irtpars$aaP)) * (sum(irtpars$baQ)/ka), ka)
#       pBApaAP[(2*ka+1):(3*ka)] <- rep(-((sum(irtpars$aaQ)) / (sum(irtpars$aaP)^2)), ka)
#       pBApaAQ[(2*ka+1):(3*ka)] <- rep(1/sum(irtpars$aaP), ka)
#       pBBpaAP[(2*ka+1):(3*ka)] <- rep(1/ka, ka)
#       pBBpaAQ[(2*ka+1):(3*ka)] <- rep(-betaA/ka, ka)
#       return(list(pBApaAP=pBApaAP, pBBpaAP=pBBpaAP, pBApaAQ=pBApaAQ, pBBpaAQ=pBBpaAQ))
#     }
#   }
# }

# pdereqcoef <- function(irtpars, model, type, kx, ky, ka, betaA, betaB){
#   if(type="MM"){
#     if(model=="3pl"){
#       pBApaAP <- numeric(3*ka)
#       pBBpaAP <- numeric(3*ka)
#       pBApaAQ <- numeric(3*ka)
#       pBBpaAQ <- numeric(3*ka)
#       
#       pBBpaAP[(ka+1):(2*ka)] <- rep(((sum(irtpars$aaQ)) / (sum(irtpars$aaP)^2)) * (sum(irtpars$baQ)/ka), ka)
#       pBBpaAQ[(ka+1):(2*ka)] <- rep(-(1/sum(irtpars$aaP)) * (sum(irtpars$baQ)/ka), ka)
#       pBApaAP[(2*ka+1):(3*ka)] <- rep(-((sum(irtpars$aaQ)) / (sum(irtpars$aaP)^2)), ka)
#       pBApaAQ[(2*ka+1):(3*ka)] <- rep(1/sum(irtpars$aaP), ka)
#       pBBpaAP[(2*ka+1):(3*ka)] <- rep(1/ka, ka)
#       pBBpaAQ[(2*ka+1):(3*ka)] <- rep(-betaA/ka, ka)
#       return(list(pBApaAP=pBApaAP, pBBpaAP=pBBpaAP, pBApaAQ=pBApaAQ, pBBpaAQ=pBBpaAQ))
#     }
#   }
# }

pderrspse <- function(irtpars, x, y, a, irtrP, irtrQ, irtsP, irtsQ, qpoints, eqc, wS, model="2pl"){
  #irtpars - list of relevant irt pars
  #x, y, a - score value vectors tests X, Y, A
  #irtrP, irtrQ, irtsP, irtsQ - probabilities to answer each questions correctly for the quadrature points considered
  #qpoints - quadrature points considered
  #eqcoef - object of class "eqc" from pkg equateIRT with the equating coefficients and relevant partial derivatives
  #model - IRT model used: 1pl, 2pl, 3pl
  kxs <- length(x)-1
  kys <- length(y)-1
  ka <- length(a)-1
  
  #fix pderplpse for 1pl, 2pl
  
  if(model=="2pl"){
    ktot <- 2*(kxs+kys+2*ka)
    prSsS <- matrix(0, nrow=(kxs+kys+2), ncol=(2*(kxs+kys)+4))
    prPrQsPsQ <- matrix(0, nrow=(2*(kxs+kys)+4), ncol=(2*(kxs+kys)+2))
    paXaYbetaAB <- matrix(0, nrow=(2*(kxs+kys)+2), ncol=ktot)
    
    betaA <- eqc$A
    betaB <- eqc$B
    
    prPpaX <- pderpl(irtrP, qpoints, b = irtpars$bx, model="2pl", a = irtpars$ax)
    prQpaX <- pderplpse(irtrQ, qpoints, b = irtpars$bx, pder = "rQaX", model="2pl", a = irtpars$ax, betaA=betaA, betaB=betaB)
    psPpaY <- pderplpse(irtsP, qpoints, b = irtpars$by, pder = "sPaY", model="2pl", a = irtpars$ay, betaA=betaA, betaB=betaB)
    psQpaY <- pderplpse(irtsQ, qpoints, b = irtpars$by, pder = "sQaY", model="2pl", a = irtpars$ay, betaA=betaA, betaB=betaB)
    prQpeqcoef <- pderplpse(irtrQ, qpoints, b = irtpars$bx, pder = "rQeqc", model="2pl", a = irtpars$ax, betaA=betaA, betaB=betaB)
    psPpeqcoef <- pderplpse(irtsP, qpoints, b = irtpars$by, pder = "sPeqc", model="2pl", a = irtpars$ay, betaA=betaA, betaB=betaB)
    
    paXaYbetaAB[(2*kxs+2*kys+1):(2*kxs+2*kys+2), (kxs+1):(kxs+ka)] <- t(eqc$partial[(2*ka+1):(3*ka),])
    paXaYbetaAB[(2*kxs+2*kys+1):(2*kxs+2*kys+2), (2*kxs+ka+1):(2*kxs+2*ka)] <- t(eqc$partial[(3*ka+1):(4*ka),])
    
    paXaYbetaAB[(2*kxs+2*kys+1):(2*kxs+2*kys+2), (2*kxs+kys+2*ka+1):(2*kxs+kys+3*ka)] <- t(eqc$partial[1:ka,])
    paXaYbetaAB[(2*kxs+2*kys+1):(2*kxs+2*kys+2), (2*kxs+3*ka+2*kys+1):(2*kxs+4*ka+2*kys)] <- t(eqc$partial[(ka+1):(2*ka),])
    
    prSsS[1:(kxs+1),1:(kxs+1)] <- wS*diag(1, kxs+1)
    prSsS[1:(kxs+1), (kxs+2):(2*(kxs+1))] <- (1-wS)*diag(1, kxs+1)
    prSsS[(kxs+2):(kxs+kys+2), (2*(kxs+1)+1):(2*(kxs+1)+kys+1)] <- wS*diag(1, kys+1)
    prSsS[(kxs+2):(kxs+kys+2), (2*(kxs+1)+kys+2):(2*(kxs+1)+2*(kys+1))] <- (1-wS)*diag(1, kys+1)
    
    prPrQsPsQ[1:(kxs+1), 1:(2*kxs)] <- t(prPpaX)
    prPrQsPsQ[(kxs+2):(2*(kxs+1)), 1:(2*kxs)] <- t(prQpaX)
    prPrQsPsQ[(2*(kxs+1)+1):(2*(kxs+1)+kys+1), (2*kxs+1):(2*kxs+2*kys)] <- t(psPpaY)
    prPrQsPsQ[(2*(kxs+1)+kys+2):(2*(kxs+1)+2*(kys+1)), (2*kxs+1):(2*kxs+2*kys)] <- t(psQpaY)
    
    prPrQsPsQ[(kxs+2):(2*(kxs+1)), (2*kxs+2*kys+1):(2*kxs+2*kys+2)] <- t(prQpeqcoef)
    prPrQsPsQ[(2*(kxs+1)+1):(2*(kxs+1)+kys+1), (2*kxs+2*kys+1):(2*kxs+2*kys+2)] <- t(psPpeqcoef)
    
    paXaYbetaAB[1:kxs, 1:kxs] <- diag(1, kxs)
    paXaYbetaAB[(kxs+1):(2*kxs), (kxs+ka+1):(2*kxs+ka)] <- diag(1, kxs)
    
    paXaYbetaAB[(2*kxs+1):(2*kxs+kys), (2*kxs+2*ka+1):(2*kxs+2*ka+kys)] <- diag(1, kys)
    paXaYbetaAB[(2*kxs+kys+1):(2*kxs+2*kys), (2*kxs+3*ka+kys+1):(2*kxs+3*ka+2*kys)] <- diag(1, kys)
    
    return(list(prSsS=prSsS, prPrQsPsQ=prPrQsPsQ, paXaYbetaAB=paXaYbetaAB))
  }
  
  if(model=="3pl"){
    ktot <- 3*(kxs+kys+2*ka)
    prSsS <- matrix(0, nrow=(kxs+kys+2), ncol=(2*(kxs+kys)+4))
    prPrQsPsQ <- matrix(0, nrow=(2*(kxs+kys)+4), ncol=(3*(kxs+kys)+2))
    paXaYbetaAB <- matrix(0, nrow=(3*(kxs+kys)+2), ncol=ktot)
    
    betaA <- eqc$A
    betaB <- eqc$B
    
    prPpaX <- pderpl(irtrP, qpoints, b = irtpars$bx, model, a = irtpars$ax, c = irtpars$cx)
    prQpaX <- pderplpse(irtrQ, qpoints, b = irtpars$bx, pder = "rQaX", model, a = irtpars$ax, c = irtpars$cx, betaA, betaB)
    psPpaY <- pderplpse(irtsP, qpoints, b = irtpars$by, pder = "sPaY", model, a = irtpars$ay, c = irtpars$cy, betaA, betaB)
    psQpaY <- pderplpse(irtsQ, qpoints, b = irtpars$by, pder = "sQaY", model, a = irtpars$ay, c = irtpars$cy, betaA, betaB)
    prQpeqcoef <- pderplpse(irtrQ, qpoints, b = irtpars$bx, pder = "rQeqc", model, a = irtpars$ax, c = irtpars$cx, betaA, betaB)
    psPpeqcoef <- pderplpse(irtsP, qpoints, b = irtpars$by, pder = "sPeqc", model, a = irtpars$ay, c = irtpars$cy, betaA, betaB)
    
    paXaYbetaAB[(3*kxs+3*kys+1):(3*kxs+3*kys+2), (kxs+1):(kxs+ka)] <- t(eqc$partial[(5*ka+1):(6*ka),])
    paXaYbetaAB[(3*kxs+3*kys+1):(3*kxs+3*kys+2), (2*kxs+ka+1):(2*kxs+2*ka)] <- t(eqc$partial[(3*ka+1):(4*ka),])
    paXaYbetaAB[(3*kxs+3*kys+1):(3*kxs+3*kys+2), (3*kxs+2*ka+1):(3*kxs+3*ka)] <- t(eqc$partial[(4*ka+1):(5*ka),])
    
    paXaYbetaAB[(3*kxs+3*kys+1):(3*kxs+3*kys+2), (3*kxs+kys+3*ka+1):(3*kxs+kys+4*ka)] <- t(eqc$partial[(2*ka+1):(3*ka),])
    paXaYbetaAB[(3*kxs+3*kys+1):(3*kxs+3*kys+2), (3*kxs+4*ka+2*kys+1):(3*kxs+5*ka+2*kys)] <- t(eqc$partial[1:ka,])
    paXaYbetaAB[(3*kxs+3*kys+1):(3*kxs+3*kys+2), (3*kxs+5*ka+3*kys+1):ktot] <- t(eqc$partial[(ka+1):(2*ka),])
    
    prSsS[1:(kxs+1),1:(kxs+1)] <- wS*diag(1, kxs+1)
    prSsS[1:(kxs+1), (kxs+2):(2*(kxs+1))] <- (1-wS)*diag(1, kxs+1)
    prSsS[(kxs+2):(kxs+kys+2), (2*(kxs+1)+1):(2*(kxs+1)+kys+1)] <- wS*diag(1, kys+1)
    prSsS[(kxs+2):(kxs+kys+2), (2*(kxs+1)+kys+2):(2*(kxs+1)+2*(kys+1))] <- (1-wS)*diag(1, kys+1)
    
    prPrQsPsQ[1:(kxs+1), 1:(3*kxs)] <- t(prPpaX)
    prPrQsPsQ[(kxs+2):(2*(kxs+1)), 1:(3*kxs)] <- t(prQpaX)
    prPrQsPsQ[(2*(kxs+1)+1):(2*(kxs+1)+kys+1), (3*kxs+1):(3*kxs+3*kys)] <- t(psPpaY)
    prPrQsPsQ[(2*(kxs+1)+kys+2):(2*(kxs+1)+2*(kys+1)), (3*kxs+1):(3*kxs+3*kys)] <- t(psQpaY)
    
    prPrQsPsQ[(kxs+2):(2*(kxs+1)), (3*kxs+3*kys+1):(3*kxs+3*kys+2)] <- t(prQpeqcoef)
    prPrQsPsQ[(2*(kxs+1)+1):(2*(kxs+1)+kys+1), (3*kxs+3*kys+1):(3*kxs+3*kys+2)] <- t(psPpeqcoef)
    
    paXaYbetaAB[1:kxs, 1:kxs] <- diag(1, kxs)
    paXaYbetaAB[(kxs+1):(2*kxs), (kxs+ka+1):(2*kxs+ka)] <- diag(1, kxs)
    paXaYbetaAB[(2*kxs+1):(3*kxs), (2*kxs+2*ka+1):(3*kxs+2*ka)] <- diag(1, kxs)
    
    paXaYbetaAB[(3*kxs+1):(3*kxs+kys), (3*kxs+3*ka+1):(3*kxs+3*ka+kys)] <- diag(1, kys)
    paXaYbetaAB[(3*kxs+kys+1):(3*kxs+2*kys), (3*kxs+4*ka+kys+1):(3*kxs+4*ka+2*kys)] <- diag(1, kys)
    paXaYbetaAB[(3*kxs+2*kys+1):(3*kxs+3*kys), (3*kxs+5*ka+2*kys+1):(3*kxs+5*ka+3*kys)] <- diag(1, kys)
    
    return(list(prSsS=prSsS, prPrQsPsQ=prPrQsPsQ, paXaYbetaAB=paXaYbetaAB))
  }
}

PEN<-function(h, r, x, var, mean, wpen, K, kernel, slog, bunif){
  
  res<-numeric(length(x))
  a<-sqrt(var/(var+h^2))
  al<-sqrt(var/(var+((pi^2*slog^2)/3)*h^2))
  au<-sqrt(var/(var+((bunif^2)/3)*h^2))
  for(i in 1:length(x)){      
    ff<-0
    if(kernel=="gaussian")
      ff <- sum(r*dnorm((x[i]-a*x-(1-a)*mean)/(a*h))/(a*h) )
    if(kernel=="stdgaussian")
      ff <- sum(r*dnorm((x[i]-x)/(h))/(h) )
    if(kernel=="logistic")
      ff <- sum(r*(exp(-((x[i]-al*x-(1-al)*mean)/(al*h))/slog) / (slog*(1+exp(-((x[i]-al*x-(1-al)*mean)/(al*h))/slog))^2) /(al*h)))
    if(kernel=="uniform")
      ff <- sum(r*dunif((x[i]-au*x-(1-au)*mean)/(au*h), min=-bunif, max=bunif)/(au*h))
    res[i]<-ff
  }
  pen1<-sum((r-res)^2)
  pen2<-0
  for(i in 1:length(r)){
    dfleft<-0
    dfright<-0
    if(kernel=="gaussian"){
      dfleft<-sum(-r*dnorm(((x[i]-(wpen))-a*x-(1-a)*mean)/(a*h))*((x[i]-(wpen))-a*x-(1-a)*mean)/((a*h)^2))
      dfright<-sum(-r*dnorm(((x[i]+(wpen))-a*x-(1-a)*mean)/(a*h))*((x[i]+(wpen))-a*x-(1-a)*mean)/((a*h)^2))
    }
    if(kernel=="logistic"){
      Rxl<-(x[i]-wpen-al*x-(1-al)*mean)/(al*h)
      Rxr<-(x[i]+wpen-al*x-(1-al)*mean)/(al*h)
      dfleft<-sum((1/(slog*(al*h)^2))*r*(exp(-(Rxl)/slog) / (slog*(1+exp(-(Rxl)/slog))^2) /(al*h) )*(1-2*(1/(1+exp(-(Rxl)/slog)))))
      dfright<-sum((1/(slog*(al*h)^2))*r*(exp(-(Rxr)/slog) / (slog*(1+exp(-(Rxr)/slog))^2) /(al*h) )*(1-2*(1/(1+exp(-(Rxr)/slog)))))
    }
    if(kernel=="uniform"){
      dfleft<-0
      dfright<-0
    }
    
    if(dfleft<0&&dfright>0 || dfleft>0&&dfright<0)
      A<-1
    else
      A<-0
    pen2<-A+pen2
  }					
  return(pen1+K*pen2)
}

PREp<-function(eq, obs, r, s){
  pre<-numeric(10)
	for(i in 1:10)
		pre[i]<-100*(eq^i%*%r-obs^i%*%s)/obs^i%*%s
	return(pre)
}

probpl <- function(qpoints, b, model="3pl", a=0, c=0){
  out <- matrix(0, ncol = length(qpoints), nrow = length(b))
  if(model=="1pl"){
    tmat <- matrix(rep(b, length(qpoints)), nrow = length(b), ncol = length(qpoints))
    tmat <- t(t(tmat) - qpoints)
    out <- 1 / (1 + exp(tmat))
  }
  if(model=="2pl"){
    tmat <- matrix(rep(a, length(qpoints)), nrow = length(a), ncol = length(qpoints))
    tmat <- - t(t(tmat) * qpoints) + tmat * b
    out <- 1 / (1 + exp(tmat))
  }
  if(model=="3pl"){
    tmat <- matrix(rep(a, length(qpoints)), nrow = length(a), ncol = length(qpoints))
    tmat <- - t(t(tmat) * qpoints) + tmat * b
    out <- c + (1-c) / (1+exp(tmat))
  }
  return(out)
}

SEE_EX<-function(fprim, dFdr, dGds, Ur, Vr, Us, Vs){
  SEE<-numeric(length(fprim))
	for(i in 1:length(fprim))
		SEE[i]<-(1/fprim[i])*sqrt((sum((-(dFdr[i,])%*%Ur+(dGds[i,])%*%Vr)^2)+sum((-(dFdr[i,])%*%Us+(dGds[i,])%*%Vs)^2)))
	return(SEE)
}

SEE_EY<-function(gprim, dFdr, dGds, Ur, Vr, Us, Vs){
  SEE<-numeric(length(gprim))
	for(i in 1:length(gprim))
		SEE[i]<-(1/gprim[i])*sqrt((sum(((dFdr[i,])%*%Ur-(dGds[i,])%*%Vr)^2)+sum(((dFdr[i,])%*%Us-(dGds[i,])%*%Vs)^2)))
	return(SEE)
}

SEED<-function(fprimeY, gprimeY, dFdreY, dGdseY, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin, slog, bunif){
  if(hxlin==0)
    hxlin<-as.numeric(1000*sqrt(varx))
  if(hylin==0)
  	hylin<-as.numeric(1000*sqrt(vary))

	cdfxlin<-cdf(r, hxlin, meanx, varx, kernel="gaussian", slog, bunif)
	cdfylin<-cdf(s, hylin, meany, vary, kernel="gaussian", slog, bunif)

	eqYxLIN<-eqinvCE(cdfxlin, s, x, y, vary, meany, hylin, kernel="gaussian", slog, bunif)

	gprimeYlin<-densityeq(s, hylin, vary, meany, eqYxLIN, y, kernel="gaussian", slog, bunif)
	fprimeYlin<-densityeq(r, hxlin, varx, meanx, x, x, kernel="gaussian", slog, bunif)

	dFdreYlin<-dFdr(r, hxlin, varx, meanx, fprimeYlin, x, x, kernel="gaussian", slog, bunif)
	dGdseYlin<-dFdr(s, hylin, vary, meany, gprimeYlin, eqYxLIN, y, kernel="gaussian", slog, bunif)

	SEE_EYlin<-matrix(nrow=length(x), ncol=(length(t(dFdreYlin[,1])%*%Ur)+length(t(dFdreY[,1])%*%Us)))
	SEE_EYpct<-matrix(nrow=length(x), ncol=(length(t(dFdreYlin[,1])%*%Ur)+length(t(dFdreY[,1])%*%Us)))
	for(i in 1:length(x)){
		SEE_EYlin[i,]<-(1/gprimeYlin[i])*c(dFdreYlin[i,]%*%Ur-dGdseYlin[i,]%*%Vr, dFdreYlin[i,]%*%Us-dGdseYlin[i,]%*%Vs)
		SEE_EYpct[i,]<-(1/gprimeY[i])*c(dFdreY[i,]%*%Ur-dGdseY[i,]%*%Vr, dFdreY[i,]%*%Us-dGdseY[i,]%*%Vs)
	}
	SEEDYx<-numeric(length(x))

	for(i in 1:length(x)){
		SEEDYx[i]<-sqrt(sum((SEE_EYpct[i,]-SEE_EYlin[i,])^2))
	}
  SEEvectors<-new("SEEvect", SEEYx=SEE_EYpct,  SEEYxLIN=SEE_EYlin)
	SEEYxLIN<-sqrt(rowSums(SEE_EYlin^2))
  out1<-data.frame(SEEDYx, eqYxLIN, SEEYxLIN)
  return(new("SEEDout", SEED=out1, SEEvect=SEEvectors, hlin=data.frame(hxlin, hylin)))
}

tradlinear <- function(x, y, r, s, N, M){
  meanx <- x %*% r
  varx <- (x^2) %*% r - meanx^2
  meany <- y %*% s  
  vary <- (y^2) %*% s - meany^2
  eqres <- meany + (sqrt(vary)/sqrt(varx)) * (x - meanx)
  dFdr<-matrix(0, length(x), ncol=length(x))
  for(i in 1:length(x)){
    dFdr[i, ] <- (sqrt(vary)/sqrt(varx))*((((x-meanx)^2)/(2*varx))*(meanx-x[i])-x)
  }
  dGds<-matrix(0, length(x), ncol=length(y))
  for(i in 1:length(x)){
    dGds[i, ] <- y + ((x[i]-meanx)/(sqrt(varx)))*((y-meany)^2/(2*sqrt(vary)) )
  }
  covrs <- matrix(0, nrow=(length(x)+length(y)), ncol=(length(x)+length(y)))
  covrs[1:length(x),1:length(x)] <- (1/N)*(diag(r)-r%*%t(r))
  covrs[(length(x)+1):(length(x)+length(y)),(length(x)+1):(length(x)+length(y))] <- (1/M)*(diag(s)-s%*%t(s))
  dFdrDGds <- matrix(0, nrow=(length(x)), ncol=(length(x)+length(y)))
  dFdrDGds[1:length(x), 1:length(x)] <- dFdr
  dFdrDGds[1:length(x),(length(x)+1):(length(x)+length(y))] <- dGds
  seeres <- numeric(length(x))
  for(i in 1:length(x))
    seeres[i] <- as.numeric(sqrt(dFdrDGds[i,] %*% covrs %*% dFdrDGds[i,]))
  return(data.frame(eqYx=eqres, SEEYx=seeres))
}