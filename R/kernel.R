setClass("cdist", representation(est1="matrix", est2="matrix", obs1="matrix", obs2="matrix"))
setClass("SEEvect", representation(SEEYx="matrix", SEEXy="matrix", SEEYxLIN="matrix", SEEXyLIN="matrix"))
setClass("SEEDout", representation(SEED="data.frame", SEEvect="SEEvect", hlin="data.frame"))
setClass("keout", representation(Cr="matrix", Cs="matrix", Cp="matrix", Cq="matrix", SEEvect="SEEvect", Pest="matrix", Pobs="matrix", Qest="matrix", Qobs="matrix", scores="list", linear="logical", PRE="data.frame", h="data.frame", type="character", equating="data.frame"))
setClass("genseed", representation(out="data.frame"))

setMethod("plot", signature(x="keout"), function(x){
    par(mfrow=c(2,1))
    plot(0:(length(x@equating$eqYx)-1), x@equating$eqYx, pch=16, ylim=c(min(x@equating$eqYx, x@equating$eqXy), max(x@equating$eqYx, x@equating$eqXy)), ylab="Equated values", xlab="Score values")
    plot(0:(length(x@equating$eqYx)-1), x@equating$SEEYx, pch=16, ylim=c(0, max(c(x@equating$SEEYx, x@equating$SEEXy))), ylab="SEE from X to Y", xlab="Score values")
}
)

setMethod("plot", signature(x="genseed"), function(x){
    ylimvect1<-c(min(x@out$eqYxD, -2*x@out$SEEDYx),max(x@out$eqYxD, 2*x@out$SEEDYx))
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
    ylab1<-"Mean of A|X or Y|X"
    xlab2<-"Score values test A/Y"
    ylab2<-"Variance of A|X or Y|X"
    xlab3<-"Score values test X"
    ylab3<-"Mean of X|A or X|Y"
    xlab4<-"Score values test X"
    ylab4<-"Variance of X|A or X|Y"
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
  cat(" Score range: \n")
  cat("   Tests X and Y: \n       Min =", min(object@scores[[1]]), "Max =", max(object@scores[[1]]))
  if(length(object@scores)>1)
    cat("\n   Test A: \n       Min =", min(object@scores[[2]]), "Max =", max(object@scores[[2]]))
  cat("\n\n","Bandwidths used: \n")
  print(object@h)
  cat("\n Equating function and standard errors: \n")
  print(data.frame(Score=0:(length(object@equating$eqYx)-1), eqYx=object@equating$eqYx, SEEYx=object@equating$SEEYx))
  cat("\n Comparing the moments: \n")
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

#continuization function
cdf<-function(r, h, mean, var){  	#r=estimated score probabilities, h=optimal h, mean of test, var of test
	a<-sqrt(var/(var+h^2))
	x<-c(0:(length(r)-1))
	cdf<-numeric(length(r))
	for(i in 1:length(r)){
		cdftemp<-0
		for(j in 1:length(r))
			cdftemp<-cdftemp+r[j]*pnorm((x[i]-a*x[j]-(1-a)*mean)/(a*h))
		cdf[i]<-cdftemp
	}
	return(cdf)
}

#continuization function for chain equating
cdfce<-function(r, ha, mean, var, eqa, a){  	#r=estimated score probabilities, h=optimal h, mean of test, var of test
	aa<-sqrt(var/(var+ha^2))
	cdf<-numeric(length(eqa))
	for(i in 1:length(eqa)){
		cdftemp<-0
		for(j in 1:length(a))
			cdftemp<-cdftemp+r[j]*pnorm((eqa[i]-aa*a[j]-(1-aa)*mean)/(aa*ha))
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
densityeq<-function(r, h, var, mean, eqx, x){
  res<-numeric(length(eqx))
	a<-sqrt(var/(var+h^2))
	for(i in 1:length(eqx)){
		ff<-0
		for(j in 1:length(r))
			ff<-ff+r[j]*dnorm((eqx[i]-a*x[j]-(1-a)*mean)/(a*h))/(a*h)
		res[i]<-ff
	}
	return(res)
}

#calculates the derivatives of F w/ respect to r for the score values in eqx
dFdr<-function(r, h, var, mean, fprim, eqx, x){  				
	a<-sqrt(var/(var+h^2))
	dFdrest<-matrix(0, length(eqx), ncol=length(x))
	for(i in 1:length(eqx)){
		for(j in 1:length(x))
			dFdrest[i, j]<-pnorm((eqx[i]-a*x[j]-(1-a)*mean)/a*h)-((1/2)*(eqx[i]-mean)*(1-a^2)*(((x[j]-mean)/sqrt(var))^2)+(1-a)*x[j])*fprim[i]
	}
	return(dFdrest)									#We obtain a J x J matrix of derivatives evaluated at each value of eqx.
}

#Let's say we want to equate X to Y. cdf=estimated
#cdf for test X, vector w/ J entries. r=estimated
#score probabilities for test Y. We specify mean
#var and optimal h for Y. We go through all entries in
#the estimated cdf for X, and calculate the corresponding
#score for test Y for that particular value. A vector w/
#J entries is returned.
eqinv<-function(cdf, r, var, mean, h){
	eqf<-numeric(length(cdf))
	for(i in 1:length(cdf)){
		u<-cdf[i]
		ggg<-function(x1, r, mean, var, h, u){
			a<-sqrt(var/(var+h^2))
			x<-c(0:(length(r)-1))
			cdftemp<-0
			for(j in 1:length(r))
				cdftemp<-cdftemp+r[j]*pnorm((x1-a*x[j]-(1-a)*mean)/a/h)
			cdftemp-u
		}
		eqf[i]<-uniroot(ggg, c(-10, 100), tol=0.000001, r, mean, var, h, u)$root
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

eqinvCE<-function(cdf, t, a, var, mean, h){  		
	eqf<-0
	for(i in 1:length(cdf)){
		u<-cdf[i]
		ggg<-function(a1, t, mean, var, h, u, a){
			aa<-sqrt(var/(var+h^2))
			cdftemp<-0
			for(j in 1:length(t))
				cdftemp<-cdftemp+t[j]*pnorm((a1-aa*a[j]-(1-aa)*mean)/(aa*h))
			cdftemp-u
		}
		eqf[i]<-uniroot(ggg, c(-10, 100), t, mean, var, h, u, a)$root
	}
	eqf
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
keirt<-function(irtr, irts, N, M, x, y, wpen, KPEN, linear){
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
        hxirt<-optimize(PEN, interval=c(0, 20), r=rirt, x, var=varxirt, mean=meanxirt, wpen=wpen, K=KPEN)$minimum
        hyirt<-optimize(PEN, interval=c(0, 20), r=sirt, y, var=varyirt, mean=meanyirt, wpen=wpen, K=KPEN)$minimum
        hxirtlin<-as.numeric(1000*sqrt(varxirt))
        hyirtlin<-as.numeric(1000*sqrt(varyirt))
      }
      
      cdfxirt<-cdf(rirt, hxirt, meanxirt, varxirt)
      cdfyirt<-cdf(sirt, hyirt, meanyirt, varyirt)
      eqYxIRT<-eqinv(cdfxirt, sirt, varyirt, meanyirt, hyirt)
      
      gprimeYirt<-densityeq(sirt, hyirt, varyirt, meanyirt, eqYxIRT, y)    					#G' for eY
		  fprimeYirt<-densityeq(rirt, hxirt, varxirt, meanxirt, x, x)							#F' for eY

      dFdreYEGirt<-dFdr(rirt, hxirt, varxirt, meanxirt, fprimeYirt, x, x)						#Matrices of derivatives. JxJ.
	  	dGdseYEGirt<-dFdr(sirt, hyirt, varyirt, meanyirt, gprimeYirt, eqYxIRT, y)						#KxK

  		Urirt<-1/sqrt(N)*(diag(sqrt(rirt))-rirt%*%t(sqrt(rirt)))										#Ur, Vs are C-matrices from the factorization of the covariance matrix
	  	Vrirt<-matrix(0, ncol=length(y), nrow=length(y))								#Us- and Vr-matrices are zero matrices for EG.
		  Usirt<-matrix(0, ncol=length(x), nrow=length(x))			
	  	Vsirt<-1/sqrt(M)*(diag(sqrt(sirt))-sirt%*%t(sqrt(sirt)))

      SEEYxirt<-SEE_EY(gprimeYirt, dFdreYEGirt, dGdseYEGirt, Urirt, Vrirt, Usirt, Vsirt)
      PREYxIRT<-PREp(eqYxIRT, y, rirt, sirt)
      
      if(!linear){
        SEED_EG<-SEED(fprimeYirt, gprimeYirt, dFdreYEGirt, dGdseYEGirt, Urirt, Vrirt, Usirt, Vsirt, rirt, sirt, meanxirt, varxirt, meanyirt, varyirt, x, y, hxirtlin, hyirtlin)
        output<-data.frame(eqYxIRT=eqYxIRT, SEEYxIRT=SEEYxirt, eqYxIRTLIN=SEED_EG@SEED$eqYxLIN, SEEYxIRTLIN=SEED_EG@SEED$SEEYxLIN, SEEDYxIRT=SEED_EG@SEED$SEEDYx, cdfxirt=cdfxirt, cdfyirt=cdfyirt)
        return(new("keout", Cr=Urirt, Cs=Vsirt, SEEvect=SEED_EG@SEEvect, scores=list(x=x), linear=linear, PRE=data.frame(PREYxIRT), h=data.frame(hyirt=hyirt, hxirt=hxirt, SEED_EG@hlin), type="IRT equipercentile", equating=output))
      }
      else{
        output<-data.frame(eqYxIRT=eqYxIRT, SEEYxIRT=SEEYxirt, cdfxirt=cdfxirt, cdfyirt=cdfyirt)
        return(new("keout", Cr=Urirt, Cs=Vsirt, scores=list(x=x), linear=linear, PRE=data.frame(PREYxIRT), h=data.frame(hyirt=hyirt, hxirt=hxirt), type="IRT linear", equating=output))
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

kequateCB<-function(x, y, P12, P21, DM12, DM21, N, M, hx=0, hy=0, hxlin=0, hylin=0, wcb=1/2, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE){
stopifnot(is(smoothed, "logical"))
if(smoothed==FALSE){
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
	      hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
	    if(hy==0)
        hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
      hxlin<-as.numeric(1000*sqrt(varx))
      hylin<-as.numeric(1000*sqrt(vary))
    }
    
    cdfx<-cdf(r, hx, meanx, varx)
		cdfy<-cdf(s, hy, meany, vary)
    eqYx<-eqinvCE(cdfx, s, y, vary, meany, hy)

    PREYx<-PREp(eqYx, y, r, s)
  	PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    if(irtx!=0 && irty!=0)
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    
    
		if(!linear){
      output<-data.frame(eqYx, cdfx, r)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, cdfx, r, SEEYxIRT=irtout@equating$SEEYxIRT)
      out<-new("keout", scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h), type="Unsmoothed CB equipercentile", equating=output)
      return(out)
		}

		output<-data.frame(eqYx, cdfx, r)
    if(irtx!=0 && irty!=0)
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, cdfx, r, SEEYxIRT=irtout@equating$SEEYxIRT)
		out<-new("keout", scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), type="Unsmoothed CB linear", equating=output)
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
	      hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
	    if(hy==0)
        hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
      hxlin<-as.numeric(1000*sqrt(varx))
      hylin<-as.numeric(1000*sqrt(vary))
    }
    
    cdfx<-cdf(r, hx, meanx, varx)
		cdfy<-cdf(s, hy, meany, vary)
    
    Cp12<-cmatrixSG(as.vector(P12), DM12, N)
    Cp21<-cmatrixSG(as.vector(P21), DM21, M)

    eqYx<-eqinvCE(cdfx, s, y, vary, meany, hy)

    gprimeY<-densityeq(s, hy, vary, meany, eqYx, y)
		fprimeY<-densityeq(r, hx, varx, meanx, x, x)
    
    dFdreYSG<-dFdr(r, hx, varx, meanx, fprimeY, x, x)
		dGdseYSG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y)

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
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    }
    
		if(!linear){
      CBlin<-kequate("CB", x=x, y=y, P12=P12, P21=P21, N=N, M=M, DM12, DM21, N, M, hxlin=hxlin, hylin=hxlin, wcb=wcb, KPEN=KPEN, wpen=wpen, linear=TRUE, irtx=irtx, irty=irty)
  	  SEEvectors<-new("SEEvect", SEEYx=SEEYxmat, SEEYxLIN=CBlin@SEEvect@SEEYxLIN)
      SEEDYx<-sqrt(rowSums((SEEYxmat-SEEvectors@SEEYxLIN)^2))
      output<-data.frame(eqYx, cdfx, r, SEEYx, SEEDYx, eqYxLIN=CBlin@equating$eqYx, SEEYxLIN=CBlin@equating$SEEYx)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, cdfx, r, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEEDYx, eqYxLIN=CBlin@equating$eqYx, SEEYxLIN=CBlin@equating$SEEYx)
      out<-new("keout", Cp=Cp12, Cq=Cp21, SEEvect=SEEvectors, Pest=P12, Pobs=P12obs, Qest=P21, Qobs=P21obs, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h), type="CB equipercentile", equating=output)
      return(out)
		}

    SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEEYxmat)
		output<-data.frame(eqYx, cdfx, SEEYx, r)
		out<-new("keout", Cp=Cp12, Cq=Cp21, SEEvect=SEEvectors, Pest=P12, Pobs=P12obs, Qest=P21, Qobs=P21obs, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), type="CB linear", equating=output)
		return(out)
}

kequateEG<-function(x, r, s, DMP, DMQ, N, M, hx=0, hy=0, hxlin=0, hylin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE){
    stopifnot(is(smoothed, "logical"))
    if(smoothed==FALSE){
      y<-x
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
          hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
        if(hy==0)
          hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
      }

		  cdfx<-cdf(r, hx, meanx, varx)
		  cdfy<-cdf(s, hy, meany, vary)
      eqYx<-eqinv(cdfx, s, vary, meany, hy)
      
      gprimeY<-densityeq(s, hy, vary, meany, eqYx, y)							#G' for eY
		  fprimeY<-densityeq(r, hx, varx, meanx, x, x)							#F' for eY

      dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x)						#Matrices of derivatives. JxJ.
	  	dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y)						#KxK

  		Ur<-1/sqrt(N)*(diag(sqrt(r))-r%*%t(sqrt(r)))										#Ur, Vs are C-matrices from pre-smooting model for EG.
	  	Vr<-matrix(0, ncol=length(y), nrow=length(y))								#Us- and Vr-matrices are zero matrices for EG.
		  Us<-matrix(0, ncol=length(x), nrow=length(x))			
	  	Vs<-1/sqrt(M)*(diag(sqrt(s))-s%*%t(sqrt(s)))

		  SEEYx<-SEE_EY(gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs)
      PREYx<-PREp(eqYx, y, r, s)
  		PRE<-data.frame(PREYx)
      h<-data.frame(hx, hy)
      output<-data.frame(eqYx, SEEYx, cdfx, cdfy, r, s)

      if(irtx!=0 && irty!=0){
        irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
      }
      if(!linear){
			  SEED_EG<-SEED(fprimeY, gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin)
  	    output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s, SEED_EG@SEED)
        if(irtx!=0 && irty!=0){
          output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s, SEED_EG@SEED)
          out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEED_EG@SEEvect, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin, irtout@h), type="Equipercentile EG without pre-smoothing", equating=output)
          return(out)
        }
        out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEED_EG@SEEvect, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin), type="Equipercentile EG without pre-smoothing", equating=output)
        return(out)
		  }
      SEE_EYlin<-matrix(nrow=length(x),ncol=(length(t(dFdreYEG[,1])%*%Ur)+length(t(dFdreYEG[,1])%*%Us)))
      for(i in 1:length(x)){
        SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYEG[i,]%*%Ur-dGdseYEG[i,]%*%Vr, dFdreYEG[i,]%*%Us-dGdseYEG[i,]%*%Vs)
      }
      SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEXy=matrix(0), SEEYxLIN=SEE_EYlin, SEEXyLIN=matrix(0))
      output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s)
      out<-new("keout", Cr=Ur, Cs=Vs, SEEvect=SEEvectors, scores=list(x=x), linear=linear, PRE=PRE, h=h, type="Linear EG without pre-smoothing", equating=output)
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
    
		y<-x
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
        hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
      if(hy==0)
        hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
    }
		cdfx<-cdf(r, hx, meanx, varx) 									#Continuize the estimated cdf:s for X and Y.
		cdfy<-cdf(s, hy, meany, vary)
		Cr<-cmatrixSG(r, DMP, N)										#Calculare c-matrices
		Cs<-cmatrixSG(s, DMQ, M)

		eqYx<-eqinv(cdfx, s, vary, meany, hy)								#Calculate the equated score values.

  	PREYx<-PREp(eqYx, y, r, s)
		PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    
    if(irtx!=0 && irty!=0){
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    }
    
		gprimeY<-densityeq(s, hy, vary, meany, eqYx, y)							#G' for eY
		fprimeY<-densityeq(r, hx, varx, meanx, x, x)							#F' for eY
		#gprimeX<-densityeq(s, hy, vary, meany, y, y)
		#fprimeX<-densityeq(r, hx, varx, meanx, eqXy, x)

    dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x)						#Matrices of derivatives. JxJ.
		dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y)						#KxK

		Ur<-Cr													#Ur, Vs are C-matrices from pre-smooting model for EG.
		Vr<-matrix(0, ncol=dim(DMP)[2], nrow=length(y))								#Us- and Vr-matrices are zero matrices for EG.
		Us<-matrix(0, ncol=dim(DMQ)[2], nrow=length(x))			
		Vs<-Cs

		SEEYx<-SEE_EY(gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs)					#Std errors for each score value. Vector w/ length(r) elements.

    
	  if(!linear){
			SEED_EG<-SEED(fprimeY, gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin)
  	  output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s, SEED_EG@SEED)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, r, s, SEED_EG@SEED)
      out<-new("keout", Cr=Cr, Cs=Cs, SEEvect=SEED_EG@SEEvect, Pest=matrix(r), Pobs=matrix(glmP$y/N), Qest=matrix(s), Qobs=matrix(glmQ$y/M), scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h, SEED_EG@hlin), type="EG equipercentile", equating=output)
      return(out)
		}
    SEE_EYlin<-matrix(nrow=length(x),ncol=(length(t(dFdreYEG[,1])%*%Ur)+length(t(dFdreYEG[,1])%*%Us)))
    for(i in 1:length(x)){
      SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYEG[i,]%*%Ur-dGdseYEG[i,]%*%Vr, dFdreYEG[i,]%*%Us-dGdseYEG[i,]%*%Vs)
    }
    SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEXy=matrix(0), SEEYxLIN=SEE_EYlin, SEEXyLIN=matrix(0))
		output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s)
    if(irtx!=0 && irty!=0)
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, r, s)
		out<-new("keout", Cr=Cr, Cs=Cs, SEEvect=SEEvectors, Pest=matrix(r), Pobs=matrix(glmP$y/N), Qest=matrix(s), Qobs=matrix(glmQ$y/M), scores=list(x=x), linear=linear, PRE=PRE, h=h, type="EG linear", equating=output)
		return(out)
}

kequateNEAT_CE<-function(x, a, P, Q, DMP, DMQ, N, M, hxP=0, hyQ=0, haP=0, haQ=0, hxPlin=0, hyQlin=0, haPlin=0, haQlin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE){
    stopifnot(is(smoothed, "logical"))
    if(smoothed==FALSE){
      rP<-rowSums(P)
  	  tP<-colSums(P)
		  sQ<-rowSums(Q)
		  tQ<-colSums(Q)
		  y<-x
		  meanx<-x%*%rP
		  meany<-y%*%sQ
		  meanaP<-a%*%tP
		  meanaQ<-a%*%tQ
		  varx<-(N/(N-1))*(y^2%*%rP-meanx^2)
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
          hxP<-optimize(PEN, interval=c(0, 20), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
        if(hyQ==0)
          hyQ<-optimize(PEN, interval=c(0, 20), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
        if(haP==0)
          haP<-optimize(PEN, interval=c(0, 20), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN)$minimum
        if(haQ==0)
          haQ<-optimize(PEN, interval=c(0, 20), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN)$minimum
        hxPlin<-as.numeric(1000*sqrt(varx))
        hyQlin<-as.numeric(1000*sqrt(vary))
        haPlin<-as.numeric(1000*sqrt(varaP))
        haQlin<-as.numeric(1000*sqrt(varaQ))
      }
    
      cdfxP<-cdf(rP, hxP, meanx, varx)
		  cdfyQ<-cdf(sQ, hyQ, meany, vary)
		  cdfaP<-cdf(tP, haP, meanaP, varaP)
		  cdfaQ<-cdf(tQ, haQ, meanaQ, varaQ)
    
      eAx<-eqinvCE(cdfxP, tP, a, varaP, meanaP, haP)		  #Linked scores from X to A on P
		  cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a)		  #cdf of the linked values on Q
		  eYCEeAx<-eqinvCE(cdfeAxQ, sQ, y, vary, meany, hyQ)	#from eAx on Q to Y            

      eYa<-eqinvCE(cdfaQ, sQ, y, vary, meany, hyQ)
      PREAx<-PREp(eAx, a, rP, tP)
      PREYa<-PREp(eYa, y, tQ, sQ)
      PRE<-data.frame(PREAx, PREYa)
      h<-data.frame(hxP, hyQ, haP, haQ, hxPlin, hyQlin, haPlin, haQlin)
    
      if(irtx!=0 && irty!=0)
        irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    
      if(!linear){
        output<-data.frame(eqYx=eYCEeAx)      
        if(irtx!=0 && irty!=0)
          output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
        return(new("keout", scores=list(x=x, a=a), linear=linear, PRE=PRE, h=h, type="Unsmoothed NEAT CE equipercentile", equating=output))
      }
      h<-data.frame(hxP, hyQ, haP, haQ)
      output<-data.frame(eqYx=eYCEeAx)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT)
      return(new("keout", scores=list(x=x, a=a), linear=linear, PRE=PRE, h=h, type="Unsmoothed NEAT CE linear", equating=output))
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
      Q<-matrix(glmQ$fitted.values/M, nrow=length(x))
      DMQ<-glmQ$x[,-1]
      Qobs<-matrix(glmQ$y/M, nrow=length(x))
    }
    else{
      Qobs=matrix(0)
    }
    if(is(P, "numeric"))
      P<-matrix(P, nrow=length(x))
    if(is(Q, "numeric"))
      Q<-matrix(Q, nrow=length(x))
    stopifnot(is(x, "numeric"), is(a, "numeric"), is(P,"matrix"), is(Q,"matrix"), is(DMP, "matrix"), is(DMQ,"matrix"), 
    is(N,"numeric"), is(M,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
    rP<-rowSums(P)
		tP<-colSums(P)
		sQ<-rowSums(Q)
		tQ<-colSums(Q)
		y<-x
		meanx<-x%*%rP
		meany<-y%*%sQ
		meanaP<-a%*%tP
		meanaQ<-a%*%tQ
		varx<-(N/(N-1))*(y^2%*%rP-meanx^2)
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
        hxP<-optimize(PEN, interval=c(0, 20), r=rP, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
      if(hyQ==0)
        hyQ<-optimize(PEN, interval=c(0, 20), r=sQ, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
      if(haP==0)
        haP<-optimize(PEN, interval=c(0, 20), r=tP, a, var=varaP, mean=meanaP, wpen=wpen, K=KPEN)$minimum
      if(haQ==0)
        haQ<-optimize(PEN, interval=c(0, 20), r=tQ, a, var=varaQ, mean=meanaQ, wpen=wpen, K=KPEN)$minimum
      hxPlin<-as.numeric(1000*sqrt(varx))
      hyQlin<-as.numeric(1000*sqrt(vary))
      haPlin<-as.numeric(1000*sqrt(varaP))
      haQlin<-as.numeric(1000*sqrt(varaQ))
    }

		cdfxP<-cdf(rP, hxP, meanx, varx)
		cdfyQ<-cdf(sQ, hyQ, meany, vary)
		cdfaP<-cdf(tP, haP, meanaP, varaP)
		cdfaQ<-cdf(tQ, haQ, meanaQ, varaQ)

		Cp<-cmatrixSG(as.vector(P), DMP, N)
		Cq<-cmatrixSG(as.vector(Q), DMQ, M)
		#Want to link tests X and A (not equal length)
		#Want to find the values of the continuized cdf for A on Q for the "linked" values from X to A on P.

		eAx<-eqinvCE(cdfxP, tP, a, varaP, meanaP, haP)		  #Linked scores from X to A on P
		cdfeAxQ<-cdfce(tQ, haQ, meanaQ, varaQ, eAx, a)		  #cdf of the linked values on Q
		eYCEeAx<-eqinvCE(cdfeAxQ, sQ, y, vary, meany, hyQ)	#from eAx on Q to Y

		eAy<-eqinvCE(cdfyQ, tQ, a, varaQ, meanaQ, haQ)		#from Y to A on Q
		cdfeAyP<-cdfce(tP, haP, meanaP, varaP, eAy, a)		#cdf of the linked values on P
		eXCEeAy<-eqinvCE(cdfeAyP, rP, x, varx, meanx, hxP)	#from eAy on P to X

		FprimeA<-densityeq(rP, hxP, varx, meanx, x, x)
		HpprimeA<-densityeq(tP, haP, varaP, meanaP, eAx, a)		 

		dFdrPeA<-dFdr(rP, hxP, varx, meanx, FprimeA, x, x)
		dHpdtPeA<-dFdr(tP, haP, varaP, meanaP, HpprimeA, eAx, a)
		HqprimeY<-densityeq(tQ, haQ, varaQ, meanaQ, eAx, a)
		GprimeY<-densityeq(sQ, hyQ, vary, meany, eYCEeAx, y)

		dHqdteY<-dFdr(tQ, haQ, varaQ, meanaQ, HqprimeY, eAx, a) #check
		dGdseY<-dFdr(sQ, hyQ, vary, meany, GprimeY, eYCEeAx, y)
    
    GprimeA<-densityeq(sQ, hyQ, vary, meany, y, y)
    HqprimeA<-densityeq(tQ, haQ, varaQ, meanaQ, eAy, a)
    
    dGdsQeA<-dFdr(sQ, hyQ, vary, meany, GprimeA, y, y)
    dHqdtPeA<-dFdr(tQ, haQ, varaQ, meanaQ, HqprimeA, eAy, a)
    
    eYa<-eqinvCE(cdfaQ, sQ, y, vary, meany, hyQ)
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
		Uq<-matrix(0, length(x), ncol=dim(DMQ)[2])
		i<-1
		while(i<dim(DMQ)[1]){
			Uq<-Cq[i:(i+length(x)-1),]+Uq
			i<-i+length(x)
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
		I<-t(matrix(1, length(x), ncol=1))
		i<-1
		j<-1
		while(i<dim(DMQ)[1]){
			Vq[j,1:dim(DMQ)[2]]<-I%*%Cq[i:(length(x)+i-1),]
			i<-i+length(x)
			j<-j+1
		}

    SEEYx<-numeric(length(x))
    SEEYxmat<-matrix(0, length(x), dim(Up)[2]+dim(Uq)[2])                                                         #Prepare matrix containing the SEE-vectors
    SEEYxmat[,1:dim(Up)[2]]<-(1/HpprimeA)*(dFdrPeA%*%Up-dHpdtPeA%*%Vp)                                            #Fill matrix
    SEEYxmat[,(dim(Up)[2]+1):(dim(Up)[2]+dim(Uq)[2])]<-(HqprimeY/GprimeY)*(1/GprimeY)*(dHqdteY%*%Vq-dGdseY%*%Uq)    
    SEEYx<-(sqrt(rowSums(SEEYxmat^2)))                                                                            #SEE for equating x to Y

    if(irtx!=0 && irty!=0){
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    }
    
    if(!linear){
      CElin<-kequate("NEAT_CE", a=a, x=x, P=P, Q=Q, N=N, M=M, DMP=DMP, DMQ=DMQ, hxPlin=hxPlin, hyQlin=hxPlin, haPlin=hxPlin, haQlin=hxPlin, KPEN=KPEN, linear=TRUE)
      SEEvectors<-new("SEEvect", SEEYx=SEEYxmat, SEEYxLIN=CElin@SEEvect@SEEYxLIN)
      SEEDYx<-sqrt(rowSums((SEEYxmat-SEEvectors@SEEYxLIN)^2))
  	  output<-data.frame(eqYx=eYCEeAx, SEEYx=SEEYx, SEEDYx, eqYxLIN=CElin@equating$eqYx, SEEYxLIN=CElin@equating$SEEYx)      
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx=SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, SEEDYx, eqYxLIN=CElin@equating$eqYx,  SEEYxLIN=CElin@equating$SEEYx)
      return(new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(x=x, a=a), linear=linear, PRE=PRE, h=h, type="NEAT CE equipercentile", equating=output))
    }

    h<-data.frame(hxP, hyQ, haP, haQ)
    SEEvectlin<-new("SEEvect", SEEYx=matrix(0), SEEXy=matrix(0), SEEYxLIN=SEEYxmat)
    output<-data.frame(eqYx=eYCEeAx, SEEYx=SEEYx)
    if(irtx!=0 && irty!=0)
      output<-data.frame(eqYx=eYCEeAx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx=SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT)
    return(new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectlin, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(x=x, a=a), linear=linear, PRE=PRE, h=h, type="NEAT CE linear", equating=output))
}

kequateNEAT_PSE<-function(x, P, Q, DMP, DMQ, N, M, w=0.5, hx=0, hy=0, hxlin=0, hylin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE){
#    stopifnot((is(smoothed, "logical"))
    if(smoothed==FALSE){
      y<-x
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
          hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
        if(hy==0)
          hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
      }
		  cdfx<-cdf(r, hx, meanx, varx)
		  cdfy<-cdf(s, hy, meany, vary)
      eqYx<-eqinv(cdfx, s, vary, meany, hy)
      PREYx<-PREp(eqYx, y, r, s)
		  PRE<-data.frame(PREYx)
      h<-data.frame(hx, hy)
    
      if(irtx!=0 && irty!=0)
        irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    
      if(!linear){
        output<-data.frame(eqYx, cdfx, cdfy, r, s)
        if(irtx!=0 && irty!=0)
          output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s)
        out<-new("keout", scores=list(x=x), linear=linear, PRE=PRE, h=h, type="Equipercentile NEAT PSE without pre-smoothing", equating=output)
        return(out)
		  }
      output<-data.frame(eqYx, cdfx, cdfy, r, s)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s)
      out<-new("keout", scores=list(x=x), linear=linear, PRE=PRE, h=h, type="Linear NEAT PSE without pre-smoothing", equating=output)
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
      Q<-matrix(glmQ$fitted.values/M, nrow=length(x))
      DMQ<-glmQ$x[,-1]
      Qobs<-matrix(glmQ$y/M, nrow=length(x))
    }
    else{
      Qobs<-matrix(0)
    }
    if(is(P, "numeric"))
      P<-matrix(P, nrow=length(x))
    if(is(Q, "numeric"))
      Q<-matrix(Q, nrow=length(x))
    stopifnot(is(x, "numeric"), is(P,"matrix"), is(Q,"matrix"), is(DMP, "matrix"), is(DMQ,"matrix"), is(N,"numeric"), 
    is(w, "numeric"), is(M,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
    y<-x
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
        hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
      if(hy==0)
        hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
    }
		cdfx<-cdf(r, hx, meanx, varx)
		cdfy<-cdf(s, hy, meany, vary)

		Cp<-cmatris(as.vector(P), DMP, N)
		Cq<-cmatris(as.vector(Q), DMQ, M)

		eqYx<-eqinv(cdfx, s, vary, meany, hy)								#Calculate the equated score values.
		eqXy<-eqinv(cdfy, r, varx, meanx, hx)
		
		gprimeY<-densityeq(s, hy, vary, meany, eqYx, y)							#G' for eY
		fprimeY<-densityeq(r, hx, varx, meanx, x, x)							#F' for eY
    
		dFdreYEG<-dFdr(r, hx, varx, meanx, fprimeY, x, x)						#Matrices of derivatives. JxJ.
		dGdseYEG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y)						#KxK

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
		while(i<=(dim(Cp)[1])){
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
		while(i<=dim(Cp)[1]/length(y)){
			Vq[i,]<-t(matrix(1,length(y), ncol=1))%*%Cq[(j+1):(j+length(y)),]
			i<-i+1
			j<-j+length(y)
		}

		Ur<-matrix(0, length(x), ncol=dim(Cp)[2])
		Vr<-matrix(0, length(x), ncol=dim(Cp)[2])
		Us<-matrix(0, length(y), ncol=dim(Cq)[2])
		Vs<-matrix(0, length(y), ncol=dim(Cq)[2])

		sumUr<-matrix(0, length(x), ncol=dim(Cp)[2])
		for(i in 1:(dim(Cp)[1]/length(x)))
			sumUr<-sumUr+(tQ[i]/tP[i])*(1/tP[i])*P[,i]%*%t(Vp[i,])
		sumVr<-matrix(0, length(x), ncol=dim(Cp)[2])
		for(i in 1:(dim(Cp)[1]/length(x)))
			sumVr<-sumVr+(1/tQ[i])*Q[,i]%*%t(Vp[i,])
		sumUs<-matrix(0, length(y), ncol=dim(Cq)[2])
		for(i in 1:(dim(Cp)[1]/length(x)))
			sumUs<-sumUs+(1/tP[i])*P[,i]%*%t(Vq[i,])
		sumVs<-matrix(0, length(y), ncol=dim(Cq)[2])
		for(i in 1:(dim(Cp)[1]/length(x)))
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
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    }
    
		if(!linear){
			SEED_NEAT<-SEED(fprimeY, gprimeY, dFdreYEG, dGdseYEG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin)
  	  output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s, SEED_NEAT@SEED)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfxirt, r, s, SEED_NEAT@SEED)
      out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEED_NEAT@SEEvect,Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h, SEED_NEAT@hlin), type="NEAT/NEC PSE equipercentile", equating=output)
      return(out)
		}
    SEE_EYlin<-matrix(nrow=length(x),ncol=(length(t(dFdreYEG[,1])%*%Ur)+length(t(dFdreYEG[,1])%*%Us)))
    for(i in 1:length(x)){
      SEE_EYlin[i,]<-(1/gprimeY[i])*c(dFdreYEG[i,]%*%Ur-dGdseYEG[i,]%*%Vr, dFdreYEG[i,]%*%Us-dGdseYEG[i,]%*%Vs)
    }
    SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEXy=matrix(0), SEEYxLIN=SEE_EYlin)
		output<-data.frame(eqYx, SEEYx, cdfx, cdfy, r, s)
    if(irtx!=0 && irty!=0)
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfxirt, r, s)
		out<-new("keout", Cp=Cp, Cq=Cq, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, Qest=Q, Qobs=Qobs, scores=list(x=x), linear=linear, PRE=PRE, h=h, type="NEAT/NEC PSE linear", equating=output)
    summary(out)
		return(out)
}

kequateSG<-function(x, P, DM, N, hx=0, hy=0, hxlin=0, hylin=0, KPEN=0, wpen=1/4, linear=FALSE, irtx=0, irty=0, smoothed=TRUE){
#stopifnot((is(smoothed, "logical"))
if(smoothed==FALSE){
    y<-x
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
        hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
      if(hy==0)
        hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
    }
  	r<-rowSums(P)
		s<-colSums(P)
    cdfx<-cdf(r, hx, meanx, varx)
		cdfy<-cdf(s, hy, meany, vary)
    eqYx<-eqinv(cdfx, s, vary, meany, hy)
    PREYx<-PREp(eqYx, y, r, s)
    PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    output<-data.frame(eqYx, cdfx, cdfy, r, s)

    if(irtx!=0 && irty!=0)
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)

    if(!linear){
  	  output<-data.frame(eqYx, cdfx, cdfy, r, s)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s)
      out<-new("keout", scores=list(x=x), linear=linear, PRE=PRE, h=h, type="Equipercentile SG without pre-smoothing", equating=output)
      return(out)
		}
    output<-data.frame(eqYx, cdfx, cdfy, r, s)
    if(irtx!=0 && irty!=0)
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s)
    out<-new("keout", scores=list(x=x), linear=linear, PRE=PRE, h=h, type="Linear SG without pre-smoothing", equating=output)
    return(out)
}
    
    if(is(P, "glm")){
      if(is(P$x, "list"))
        stop("The design matrix must be included in the glm object.")
      glmP<-P
      N<-sum(glmP$y)
      P<-matrix(glmP$fitted.values/N, ncol=length(x))
      DM<-glmP$x[,-1]
      Pobs<-matrix(glmP$y/N, nrow=length(x))
    }
    else
      Pobs<-matrix(0)
    stopifnot(is(x, "numeric"), is(P,"matrix"), is(DM, "matrix"), is(N,"numeric"), is(KPEN, "numeric"), is(linear, "logical"))
		y<-x
		J<-dim(P)[1]
		K<-dim(P)[2]
		r<-rowSums(P)
		s<-colSums(P)
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
	      hx<-optimize(PEN, interval=c(0, 20), r=r, x, var=varx, mean=meanx, wpen=wpen, K=KPEN)$minimum
	    if(hy==0)
        hy<-optimize(PEN, interval=c(0, 20), r=s, y, var=vary, mean=meany, wpen=wpen, K=KPEN)$minimum
    }

		cdfx<-cdf(r, hx, meanx, varx)
		cdfy<-cdf(s, hy, meany, vary)
		Cp<-cmatrixSG(as.vector(P), DM, N)

		eqYx<-eqinv(cdfx, s, vary, meany, hy)

		gprimeY<-densityeq(s, hy, vary, meany, eqYx, y)
		fprimeY<-densityeq(r, hx, varx, meanx, x, x)
		dFdreYSG<-dFdr(r, hx, varx, meanx, fprimeY, x, x)
		dGdseYSG<-dFdr(s, hy, vary, meany, gprimeY, eqYx, y)
		Ur<-matrix(0, length(x), ncol=dim(DM)[2])
		i<-1
		while(i<length(x)*length(y)){
			Ur<-Cp[i:(i+length(x)-1),]+Ur
			i<-i+length(x)
		}
		
		Vr<-matrix(0, length(x), ncol=dim(DM)[2])
		I<-t(matrix(1, length(x), ncol=1))
		i<-1
		j<-1
		while(i<(length(x)*length(x))){
			Vr[j,1:dim(DM)[2]]<-I%*%Cp[i:(length(x)+i-1),]
			i<-i+length(x)
			j<-j+1
		}
			
		Us<-matrix(0, ncol=dim(DM)[2], nrow=length(y))
		Vs<-matrix(0, ncol=dim(DM)[2], nrow=length(x))

		SEEYx<-SEE_EY(gprimeY, dFdreYSG, dGdseYSG, Ur, Vr, Us, Vs)
		PREYx<-PREp(eqYx, y, r, s)
		PRE<-data.frame(PREYx)
    h<-data.frame(hx, hy)
    M<-N
        
    if(irtx!=0 && irty!=0){
      irtout<-keirt(irtx, irty, N, M, x, y, wpen, KPEN, linear)
    }

		if(!linear){
			SEED_SG<-SEED(fprimeY, gprimeY, dFdreYSG, dGdseYSG, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin)
  	  output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s, SEED_SG@SEED)
      if(irtx!=0 && irty!=0)
        output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s, SEED_SG@SEED)
      out<-new("keout", Cp=Cp, SEEvect=SEED_SG@SEEvect, Pest=P, Pobs=Pobs, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(h, SEED_SG@hlin), type="SG equipercentile", equating=output)
      return(out)
		}
    SEE_EYlin<-matrix(nrow=length(x),ncol=(length(t(dFdreYSG[,1])%*%Ur)+length(t(dFdreYSG[,1])%*%Us)))
    for(i in 1:length(x)){
      SEE_EYlin[i,]<-(1/gprimeY[i])*c(t(dFdreYSG[,i])%*%Ur-t(dGdseYSG[,i])%*%Vr, t(dFdreYSG[,i])%*%Us-t(dGdseYSG[,i])%*%Vs)
    }
    SEEvectors<-new("SEEvect", SEEYx=matrix(0), SEEYxLIN=SEE_EYlin)
		output<-data.frame(eqYx, cdfx, cdfy, SEEYx, r, s)
    if(irtx!=0 && irty!=0)
      output<-data.frame(eqYx, eqYxIRT=irtout@equating$eqYxIRT, SEEYx, SEEYxIRT=irtout@equating$SEEYxIRT, cdfx, cdfy, cdfxirt=irtout@equating$cdfxirt, cdfyirt=irtout@equating$cdfyirt, r, s)
		out<-new("keout", Cp=Cp, SEEvect=SEEvectors, Pest=P, Pobs=Pobs, scores=list(x=x), linear=linear, PRE=PRE, h=data.frame(hxlin, hylin), type="SG linear", equating=output)
		return(out)
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

PEN<-function(h, r, x, var, mean, wpen, K){

  a<-sqrt(var/(var+h^2))
  pen1<-0
	for(i in 1:length(r)){
		ff<-0
		for(j in 1:length(r))
			ff<-ff+r[j]*dnorm((x[i]-a*x[j]-(1-a)*mean)/(a*h))/(a*h)
		pen1<-pen1+(r[i]-ff)^2
	}

	pen2<-0
	for(i in 1:length(r)){
		dfleft<-0
		dfright<-0
		for(j in 1:length(r)){
			dflefttemp<--r[j]*dnorm(((x[i]-(wpen))-a*x[j]-(1-a)*mean)/(a*h))*((x[i]-(wpen))-a*x[j]-(1-a)*mean)/((a*h)^2)
			dfleft<-dfleft+dflefttemp			
			dfrighttemp<--r[j]*dnorm(((x[i]+(wpen))-a*x[j]-(1-a)*mean)/(a*h))*((x[i]+(wpen))-a*x[j]-(1-a)*mean)/((a*h)^2)
			dfright<-dfright+dfrighttemp
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

SEE_EX<-function(fprim, dFdr, dGds, Ur, Vr, Us, Vs){
  SEE<-numeric(length(fprim))
	for(i in 1:length(fprim))
		SEE[i]<-(1/fprim[i])*sqrt((sum((-(dFdr[i,])%*%Ur+(dGds[i,])%*%Vr)^2)+sum((-(dFdr[i,])%*%Us+(dGds[i,])%*%Vs)^2)))
	return(SEE)
}

SEE_EXCE<-function(HQprim, dGQds, dHQdt, eXprimeAy, FPprim, dHPdt, dFPdr, Up, Vp, Vq, Uq){
  SEE<-numeric(length(HQprim))
  for(i in 1:length(SEE))
		SEE[i]<-sqrt((1/HQprim[i])^2*sum((-dGQds[,i]%*%Up+dHQdt[,i]%*%Vp)^2)+eXprimeAy[i]^2*(1/FPprim[i])^2*sum((-dHPdt[,i]%*%Vq+dFPdr[,i]%*%Uq)^2))
	SEE
}

SEE_EY<-function(gprim, dFdr, dGds, Ur, Vr, Us, Vs){
  SEE<-numeric(length(gprim))
	for(i in 1:length(gprim))
		SEE[i]<-(1/gprim[i])*sqrt((sum(((dFdr[i,])%*%Ur-(dGds[i,])%*%Vr)^2)+sum(((dFdr[i,])%*%Us-(dGds[i,])%*%Vs)^2)))
	return(SEE)
}

SEE_EYCE<-function(HPprim, dFPdr, dHPdt, eYprimeAx, GQprim, dHQdt, dGQds, Up, Vp, Vq, Uq){
  SEE<-numeric(length(HPprim))
	for(i in 1:length(SEE))
		SEE[i]<-sqrt((1/HPprim[i])^2*sum((dFPdr[,i]%*%Up-dHPdt[,i]%*%Vp)^2)+eYprimeAx[i]^2*(1/GQprim[i])^2*sum((dHQdt[,i]%*%Vq-dGQds[,i]%*%Uq)^2))
	SEE
}

SEED<-function(fprimeY, gprimeY, dFdreY, dGdseY, Ur, Vr, Us, Vs, r, s, meanx, varx, meany, vary, x, y, hxlin, hylin){
  if(hxlin==0)
    hxlin<-as.numeric(1000*sqrt(varx))
  if(hylin==0)
  	hylin<-as.numeric(1000*sqrt(vary))

	cdfxlin<-cdf(r, hxlin, meanx, varx)
	cdfylin<-cdf(s, hylin, meany, vary)

	eqYxLIN<-eqinv(cdfxlin, s, vary, meany, hylin)

	gprimeYlin<-densityeq(s, hylin, vary, meany, eqYxLIN, y)
	fprimeYlin<-densityeq(r, hxlin, varx, meanx, x, x)

	dFdreYlin<-dFdr(r, hxlin, varx, meanx, fprimeYlin, x, x)
	dGdseYlin<-dFdr(s, hylin, vary, meany, gprimeYlin, eqYxLIN, y)

	SEE_EYlin<-matrix(nrow=length(y), ncol=(length(t(dFdreYlin[,1])%*%Ur)+length(t(dFdreY[,1])%*%Us)))
	SEE_EYpct<-matrix(nrow=length(y), ncol=(length(t(dFdreYlin[,1])%*%Ur)+length(t(dFdreY[,1])%*%Us)))
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