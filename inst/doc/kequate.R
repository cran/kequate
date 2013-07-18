### R code from vignette source 'kequate.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: kequate.Rnw:78-79
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: kequate.Rnw:414-417
###################################################
load("eqguide.RData")
load("CBsim.RData")
library(kequate)


###################################################
### code chunk number 3: kequate.Rnw:419-420
###################################################
freq <- kefreq(simeq$bivar1$X, 0:20)


###################################################
### code chunk number 4: kequate.Rnw:430-431
###################################################
SGfreq <- kefreq(simeq$bivar1$X, 0:20, simeq$bivar1$A, 0:10)


###################################################
### code chunk number 5: kequate.Rnw:433-436
###################################################
SGfreq <- kefreq(simeq$bivar1$X, 0:20, simeq$bivar1$A, 0:10)
PNEAT <- kefreq(simeq$bivar1$X, 0:20, simeq$bivar1$A, 0:10)
QNEAT <- kefreq(simeq$bivar2$Y, 0:20, simeq$bivar2$A, 0:10)


###################################################
### code chunk number 6: kequate.Rnw:447-449
###################################################
EGX <- glm(freq~I(X) + I(X^2) + I(X^3) + I(X^4) + I(X^5), family = 
"poisson", data = FXEG, x = TRUE)


###################################################
### code chunk number 7: kequate.Rnw:451-453
###################################################
EGY <- glm(freq~I(Y) + I(Y^2) + I(Y^3) + I(Y^4) + I(Y^5), family = 
"poisson", data = FYEG, x = TRUE)


###################################################
### code chunk number 8: kequate.Rnw:459-461
###################################################
SGglm <- glm(frequency~I(X) + I(X^2) + I(A) + I(A^2) + I(A^3) + I(X):I(A) 
+ I(X^2):I(A^2), data = SGfreq, family = "poisson", x = TRUE)


###################################################
### code chunk number 9: kequate.Rnw:465-469
###################################################
freqCB12 <- kefreq(CBeq12[,1], 0:40, CBeq12[,2])
freqCB21 <- kefreq(CBeq21[,1], 0:40, CBeq21[,2])
glmCB12 <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^3)+I(Y)+I(Y^2)+I(Y^3)+I(Y^4)+I(X):I(Y)+I(X^2):I(Y)+I(X):I(Y^2)+I(X^2):I(Y^2), data=freqCB12, family=poisson, x=TRUE)
glmCB21 <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^3)+I(Y)+I(Y^2)+I(Y^3)+I(Y^4)+I(X):I(Y)+I(X^2):I(Y)+I(X):I(Y^2)+I(X^2):I(Y^2), data=freqCB21, family=poisson, x=TRUE)


###################################################
### code chunk number 10: kequate.Rnw:489-490
###################################################
PNEATordered <- PNEAT[order(PNEAT$A, PNEAT$X),]


###################################################
### code chunk number 11: kequate.Rnw:493-496
###################################################
PNEAT$indx0 <- numeric(length(PNEAT$X))
PNEAT$ind1x <- numeric(length(PNEAT$X))
PNEAT$ind2x <- numeric(length(PNEAT$X))


###################################################
### code chunk number 12: kequate.Rnw:500-506
###################################################
PNEAT$indx0[PNEAT$X==0] <- 1
PNEAT$ind1x[PNEAT$X %in% c(5, 10, 15, 20)] <- 1
PNEAT$ind2x[PNEAT$X==5] <- 5
PNEAT$ind2x[PNEAT$X==10] <- 10
PNEAT$ind2x[PNEAT$X==15] <- 15
PNEAT$ind2x[PNEAT$X==20] <- 20


###################################################
### code chunk number 13: kequate.Rnw:508-518
###################################################
QNEAT$indy0 <- numeric(length(QNEAT$X))
QNEAT$ind1y <- numeric(length(QNEAT$X))
QNEAT$ind2y <- numeric(length(QNEAT$X))

QNEAT$indy0[QNEAT$X==0] <- 1
QNEAT$ind1y[QNEAT$X %in% c(5, 10, 15, 20)] <- 1
QNEAT$ind2y[QNEAT$X==5] <- 5
QNEAT$ind2y[QNEAT$X==10] <- 10
QNEAT$ind2y[QNEAT$X==15] <- 15
QNEAT$ind2y[QNEAT$X==20] <- 20


###################################################
### code chunk number 14: kequate.Rnw:522-525
###################################################
PNEATglm <- glm(frequency~I(X) + I(X^2) + I(X^3) + I(A) + I(A^2) + 
I(X):I(A) + I(X):I(A^2) + I(indx0) + I(ind1x) + I(ind2x) + I(ind2x^2), 
data = PNEAT, family = "poisson", x = TRUE)


###################################################
### code chunk number 15: kequate.Rnw:527-530
###################################################
QNEATglm <- glm(frequency~I(X) + I(X^2) + I(X^3) + I(A) + I(A^2) + 
I(X):I(A) + I(X):I(A^2) + I(indy0) + I(ind1y) + I(ind2y) + I(ind2y^2), 
data = QNEAT, family = "poisson", x = TRUE)


###################################################
### code chunk number 16: kequate.Rnw:542-546
###################################################
testfreq <- as.data.frame(table(factor(data11$S11, levels = 0:40, ordered 
= TRUE), factor(data11$edu, levels = 1:2, ordered = TRUE), 
factor(data11$math, levels = 1:3, ordered = TRUE), dnn = c("S11", "edu", 
"math")))


###################################################
### code chunk number 17: kequate.Rnw:553-555
###################################################
testdata11 <- data.frame(frequency = testfreq$Freq, S11 = rep(0:40, 6),
edu = rep(1:2, each=41*3), math = rep(1:3, each = 41*2))


###################################################
### code chunk number 18: kequate.Rnw:559-562
###################################################
glm11 <- glm(frequency~I(S11) +  I(S11^2) + I(S11^3) + I(S11^4) + 
I(math) + I(math^2) + factor(edu) + I(S11):I(math) + I(S11):factor(edu) + 
I(math):factor(edu), data = data11, family = "poisson", x = TRUE)


###################################################
### code chunk number 19: kequate.Rnw:564-565
###################################################
glm12 <- glm(frequency~I(S12) +  I(S12^2) + I(S12^3) + I(S12^4) + I(math) + I(math^2) + factor(edu) + I(S12):I(math) + I(S12):factor(edu) + I(math):factor(edu), data = data12, family = "poisson", x = TRUE)


###################################################
### code chunk number 20: kequate.Rnw:574-575
###################################################
FTglm <- FTres(EGX$y, EGX$fitted.values)


###################################################
### code chunk number 21: kequate.Rnw:579-581
###################################################
Pest <- matrix(PNEATglm$fitted.values, nrow=21)
Pobs <- matrix(PNEATglm$y, nrow=21)


###################################################
### code chunk number 22: kequate.Rnw:583-584
###################################################
NEATPcdist <- cdist(Pest, Pobs)


###################################################
### code chunk number 23: kequate.Rnw:752-753
###################################################
keEG <- kequate("EG", 0:20, 0:20, EGX, EGY)


###################################################
### code chunk number 24: kequate.Rnw:757-759
###################################################
keEGobs <- kequate("EG", 0:20, 0:20, EGX$y/1453, EGY$y/1455, N = 1453, 
M = 1455, smoothed = FALSE)


###################################################
### code chunk number 25: kequate.Rnw:762-763
###################################################
summary(keEG)


###################################################
### code chunk number 26: kequate.Rnw:769-770
###################################################
keSG <- kequate("SG", 0:20, 0:10, SGglm)


###################################################
### code chunk number 27: kequate.Rnw:774-775
###################################################
summary(keSG)


###################################################
### code chunk number 28: kequate.Rnw:779-781
###################################################
DMSG <- SGglm$x[,-1]
PSG <- matrix(SGglm$fitted.values/sum(SGglm$fitted.values), nrow=21)


###################################################
### code chunk number 29: kequate.Rnw:783-784
###################################################
keSGDM <- kequate("SG", 0:20, 0:10, P = PSG, DM = DMSG, N = 1000)


###################################################
### code chunk number 30: kequate.Rnw:818-819
###################################################
keCB <- kequate("CB", 0:40, 0:40, glmCB12, glmCB21)


###################################################
### code chunk number 31: kequate.Rnw:826-827
###################################################
keNEATCE <- kequate("NEAT_CE", 0:20, 0:20, 0:10, PNEATglm, QNEATglm)


###################################################
### code chunk number 32: neatceplot
###################################################
plot(keNEATCE)


###################################################
### code chunk number 33: neatceplot1
###################################################
plot(keNEATCE)


###################################################
### code chunk number 34: kequate.Rnw:846-847
###################################################
keNEATPSE <- kequate("NEAT_PSE", 0:20, 0:20, PNEATglm, QNEATglm)


###################################################
### code chunk number 35: neatpseplot
###################################################
plot(keNEATPSE)


###################################################
### code chunk number 36: neatpseplot1
###################################################
plot(keNEATPSE)


###################################################
### code chunk number 37: kequate.Rnw:874-876
###################################################
keNEATPSEnew <- kequate("NEAT_PSE", 0:20, 0:20, PNEATglm, QNEATglm, hx = 
0.5, hy = 0.5, hxlin = 1000, hylin = 1000)


###################################################
### code chunk number 38: kequate.Rnw:880-881
###################################################
NECtest2012 <- kequate("NEC", 0:40, 0:40, glm12, glm11)


###################################################
### code chunk number 39: kequate.Rnw:884-885
###################################################
summary(NECtest2012)


###################################################
### code chunk number 40: necplot1
###################################################
plot(NECtest2012)


###################################################
### code chunk number 41: necplot
###################################################
plot(NECtest2012)


###################################################
### code chunk number 42: kequate.Rnw:901-903
###################################################
NECtestL <- kequate("NEC", 0:40, 0:40, glm12, glm11, kernel = "logistic")
NECtestU <- kequate("NEC", 0:40, 0:40, glm12, glm11, kernel = "uniform")


###################################################
### code chunk number 43: neccomp
###################################################
plot(0:40, getSee(NECtest2012), ylim=c(0, 0.8), pch=1, xlab="", ylab="")
par(new=TRUE)
plot(0:40, getSee(NECtestL), ylim=c(0, 0.8), pch=2, xlab="", ylab="")
par(new=TRUE)
plot(0:40, getSee(NECtestU), ylim=c(0, 0.8), pch=3, xlab="Score value", ylab="SEE")
legend("topright", inset=.1, title="Kernel utilized", c("Gaussian", "Logistic", "Uniform"), pch=c(1, 2, 3))


###################################################
### code chunk number 44: neckernelcomp
###################################################
plot(0:40, getSee(NECtest2012), ylim=c(0, 0.8), pch=1, xlab="", ylab="")
par(new=TRUE)
plot(0:40, getSee(NECtestL), ylim=c(0, 0.8), pch=2, xlab="", ylab="")
par(new=TRUE)
plot(0:40, getSee(NECtestU), ylim=c(0, 0.8), pch=3, xlab="Score value", ylab="SEE")
legend("topright", inset=.1, title="Kernel utilized", c("Gaussian", "Logistic", "Uniform"), pch=c(1, 2, 3))


###################################################
### code chunk number 45: kequate.Rnw:926-928
###################################################
keEGirt <- kequate("EG", 0:20, 0:20, EGX, EGY, irtx = simeq$irt2, irty = 
simeq$irt1)


###################################################
### code chunk number 46: seedplot
###################################################
SEEDPSECE <- genseed(keNEATPSE, keNEATCE)
plot(SEEDPSECE)


###################################################
### code chunk number 47: seedplot1
###################################################
SEEDPSECE <- genseed(keNEATPSE, keNEATCE)
plot(SEEDPSECE)


