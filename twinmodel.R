#双变量twin模型
require(OpenMx)

labFull   <- function(lab,nr,nc) { paste(lab,1:nr,rep(1:nc,each=nr),sep="") }
labLower  <- function(lab,nv) { paste(lab,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="") }

# Select Variables for Analysis
Vars      <- c('IMT','avePD')
nv        <- 2       # number of variables
ntv       <- nv*2    # number of total variables
selVars   <- paste(Vars,c(rep(1,nv),rep(2,nv)),sep="") 

# Select Data for Analysis
mzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==1, selVars)))
dzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==2, selVars)))

# Generate Descriptive Statistics
colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
cov(mzData,use="complete")
cov(dzData,use="complete")

# Set Starting Values
svMe      <- rep(0,nv)      # start value for means
svPa      <- diag(0.6,nv)          # start values for path parameters

# ACE Model
# Matrices declared to store a, c, and e Path Coefficients
pathA     <- mxMatrix( type="Lower", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, labels=labLower("a",nv), name="a" )
pathC     <- mxMatrix( type="Lower", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, labels=labLower("c",nv), name="c" )
pathE     <- mxMatrix( type="Lower", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, labels=labLower("e",nv), name="e" )

# Matrices generated to hold A, C, and E computed Variance Components
covA      <- mxAlgebra( expression=a %*% t(a), name="A" )
covC      <- mxAlgebra( expression=c %*% t(c), name="C" )
covE      <- mxAlgebra( expression=e %*% t(e), name="E" )

# Algebra to compute total variances
covP      <- mxAlgebra( expression=A+C+E, name="V" )

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins
meanG     <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE,
                       values=svMe, labels=labFull("me",1,nv), name="expMean" )
covMZ     <- mxAlgebra( expression=rbind( cbind(V, A+C),
                                          cbind(A+C, V)), name="expCovMZ" )
covDZ     <- mxAlgebra( expression=rbind( cbind(V, 0.5%x%A+ C),
                                          cbind(0.5%x%A+C , V)), name="expCovDZ" )

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
rowVC     <- rep('VC',nv)
colVC     <- rep(c('A','C','E'),each=nv)
estVC     <- mxAlgebra(expression = cbind(A,C,E),name="VC",dimnames=list(rowVC,colVC))
ciACE     <- mxCI("VC[1:2,]")


# Combine Groups
pars      <- list( pathA, pathC, pathE, covA, covC, covE, covP )
modelMZ   <- mxModel( pars, meanG, covMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ   <- mxModel( pars, meanG, covDZ, dataDZ, expDZ, funML, name="DZ" )
fitML     <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction") )
AceModel  <- mxModel( "ACE", pars, modelMZ, modelDZ, fitML, estVC, ciACE )

# Run ACE model
AceFit    <- mxRun(AceModel, intervals=T)
AceSumm   <- summary(AceFit)
AceSumm


#Calculate distance between observed corvariance matrix and model-based corvariance matrix
estcovMZ = rbind(cbind(AceFit$V$result,AceFit$A$result+AceFit$C$result),
                 cbind(AceFit$A$result+AceFit$C$result,AceFit$V$result))
estcovDZ = rbind(cbind(AceFit$V$result,0.5%x%AceFit$A$result+AceFit$C$result),
                 cbind(0.5%x%AceFit$A$result+AceFit$C$result,AceFit$V$result))
obscovMZ = as.matrix(cov(mzData,use="complete"))
obscovDZ = as.matrix(cov(dzData,use="complete"))

covMZ.dist.ace = log(det(estcovMZ)) + sum(diag(obscovMZ%*%solve(estcovMZ))) - log(det(obscovMZ)) - 2
covDZ.dist.ace = log(det(estcovDZ)) + sum(diag(obscovDZ%*%solve(estcovDZ))) - log(det(obscovDZ)) - 2
covMZ.dist.ace
covDZ.dist.ace

#https://github.com/jelman/TwinModels/blob/master/Bivariate.R