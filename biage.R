#age correction for bivariate twin ACE model

# Select Variables for Analysis
Vars      <- c('IMT','avePD')
nv        <- 2       # number of variables
ntv       <- nv*2    # number of total variables
sel_Vars  <- paste(Vars,c(rep(1,nv),rep(2,nv)),sep="") 
selVars   <- c("age",sel_Vars)

# Select Data for Analysis
mzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==1, selVars)))
dzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==2, selVars)))

# Generate Descriptive Statistics
colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
cov(mzData,use="complete")
cov(dzData,use="complete")

# Set Starting Values
svMe      <- rep(0,ntv+1)      # start value for means
svPa      <- diag(0.6,nv)          # start values for path parameters

# ACE Model
# Matrices declared to store a, c, and e Path Coefficients
pathA     <- mxMatrix( type="Lower", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, labels=labLower("a",nv), name="a" )
pathC     <- mxMatrix( type="Lower", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, labels=labLower("c",nv), name="c" )
pathE     <- mxMatrix( type="Lower", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, labels=labLower("e",nv), name="e" )
pathG     <- mxMatrix( type="Diag", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, label=c("ageP1","ageP2"), name="s" ) # effect of age on phenotype
varage    <- mxMatrix( type="Full", nrow=1, ncol=1,
                       free=TRUE, values=1, label="varage", name="vage" ) #variance of age
var_age   <- mxAlgebra(expression = cbind(vage,vage), name = "vagebind") #variance of age for matrix calculation


# Matrices generated to hold A, C, E and G(age) computed Variance Components
covA      <- mxAlgebra( expression=a %*% t(a), name="A" )
covC      <- mxAlgebra( expression=c %*% t(c), name="C" )
covE      <- mxAlgebra( expression=e %*% t(e), name="E" )
covG      <- mxAlgebra( expression=s %*% t(s), name="G" )

# Algebra to compute total variances
covP      <- mxAlgebra( expression=A+C+E, name="V" )

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins
meanG     <- mxMatrix( type="Full", nrow=1, ncol=ntv+1, free=TRUE,
                       values=svMe, labels=labFull("mean",1,ntv+1), name="expMean" )
covMZ     <- mxAlgebra( expression=rbind( cbind(vage, vagebind%*%s, vagebind%*%s),
                                          cbind(s%*%t(vagebind), V+G, A+C+G),
                                          cbind(s%*%t(vagebind), A+C+G, V+G)), name="expCovMZ" )
covDZ     <- mxAlgebra( expression=rbind( cbind(vage, vagebind%*%s, vagebind%*%s),
                                          cbind(s%*%t(vagebind), V+G, 0.5%x%A+C+G),
                                          cbind(s%*%t(vagebind), 0.5%x%A+C+G , V+G)), name="expCovDZ" )

# Data objects for Multiple Groups
dataMZ    <- mxData( observed=mzData, type="raw" )
dataDZ    <- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="expCovMZ", means="expMean",
                                  dimnames=selVars )
expDZ     <- mxExpectationNormal( covariance="expCovDZ", means="expMean",
                                  dimnames=selVars )
funML     <- mxFitFunctionML()

#Get Confidential intervals
matI = mxMatrix(type="Iden",nrow=nv,ncol=nv,name="I")
CorA = mxAlgebra(solve(sqrt(I*A)) %&% A, name = "CorA")
CorC = mxAlgebra(solve(sqrt(I*C)) %&% C, name = "CorC")
CorE = mxAlgebra(solve(sqrt(I*E)) %&% E, name = "CorE")
rowVC     <- rep('VC',nv)
colVC     <- rep(c('CorA','CorC','CorE'),each=nv)
estVC     <- mxAlgebra(expression = cbind(A,C,E),name="VC",dimnames=list(rowVC,colVC))
ciACE     <- mxCI("VC[1:2,]")


# Combine Groups
pars      <- list( pathA, pathC, pathE, pathG, varage, var_age, covA, covC, covE, covG, covP, matI, CorA, CorC, CorE )
modelMZ   <- mxModel( pars, meanG, covMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ   <- mxModel( pars, meanG, covDZ, dataDZ, expDZ, funML, name="DZ" )
fitML     <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction") )
AcegModel  <- mxModel( "ACEG", pars, modelMZ, modelDZ, fitML, estVC, ciACE )

# Run ACEG model
AcegFit    <- mxRun(AcegModel, intervals=T)
AcegSumm   <- summary(AcegFit)
AcegSumm

#Run CEG model to test significant of A
CegModel = omxSetParameters(AcegModel,labels=labLower("a",nv),free=F,values=0)
CegFit = mxRun(CegModel,intervals=T)
mxCompare(AcegFit,CegFit)

#chi-square test
mxCompare(SatFit,AcegFit)



#Calculate distance between observed corvariance matrix and model-based corvariance matrix
estcovMZ = rbind(cbind(AcegFit$V$result+AcegFit$G$result,AcegFit$A$result+AcegFit$C$result+AcegFit$G$result),
                 cbind(AcegFit$A$result+AcegFit$C$result+AcegFit$G$result,AcegFit$V$result+AcegFit$G$result))
estcovDZ = rbind(cbind(AcegFit$V$result+AcegFit$G$result,0.5%x%AcegFit$A$result+AcegFit$C$result+AcegFit$G$result),
                 cbind(0.5%x%AcegFit$A$result+AcegFit$C$result+AcegFit$G$result,AcegFit$V$result+AcegFit$G$result))
obscovMZ = as.matrix(cov(mzData,use="complete"))[c(2:5),c(2:5)]
obscovDZ = as.matrix(cov(dzData,use="complete"))[c(2:5),c(2:5)]


covMZ.dist.aceg = log(det(estcovMZ)) + sum(diag(obscovMZ%*%solve(estcovMZ))) - log(det(obscovMZ)) - 2
covDZ.dist.aceg = log(det(estcovDZ)) + sum(diag(obscovDZ%*%solve(estcovDZ))) - log(det(obscovDZ)) - 2
covMZ.dist.aceg
covDZ.dist.aceg