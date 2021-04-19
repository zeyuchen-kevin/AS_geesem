require(OpenMx)

# Select Variables for Analysis
vars <- 'avePD'
nv        <- 1       # number of variables
ntv       <- nv*2    # number of total variables
sel_Vars  <- paste(vars,c(rep(1,nv),rep(2,nv)),sep="")
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
svMe      <- 0    # start value for means
svPa      <- 0.6      # start value for path coefficients (sqrt(variance/#ofpaths))
# ACE Model
# Matrices declared to store a, c, and e Path Coefficients
pathA     <- mxMatrix( type="Full", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, label="a11", name="a" )
pathC     <- mxMatrix( type="Full", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, label="c11", name="c" )
pathE     <- mxMatrix( type="Full", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, label="e11", name="e" )
pathG     <- mxMatrix( type="Full", nrow=nv, ncol=nv,
                       free=TRUE, values=svPa, label="g11", name="s" )
varage    <- mxMatrix( type="Full", nrow=nv, ncol=nv,
                       free=TRUE, values=1, label="varage", name="vage" )

# Matrices generated to hold A, C, and E computed Variance Components
covA      <- mxAlgebra( expression=a %*% t(a), name="A" )
covC      <- mxAlgebra( expression=c %*% t(c), name="C" )
covE      <- mxAlgebra( expression=e %*% t(e), name="E" )
covG      <- mxAlgebra( expression=s %*% t(s), name="G" )

# Algebra to compute total variances
covP      <- mxAlgebra( expression=A+C+E, name="V" )

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins
meanG     <- mxMatrix( type="Full", nrow=1, ncol=3,
                       free=TRUE, values=svMe, label="mean", name="expMean" )
covMZ     <- mxAlgebra( expression=rbind( cbind(vage*vage, s*vage, s*vage),
                                          cbind(s*vage, V+G, A+C+G),
                                          cbind(s*vage, A+C+G, V+G)), name="expCovMZ" )
covDZ     <- mxAlgebra( expression=rbind( cbind(vage*vage, s*vage, s*vage),
                                          cbind(s*vage, V+G, 0.5%x%A+C+G),
                                          cbind(s*vage, 0.5%x%A+C+G , V+G)), name="expCovDZ" )

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
colVC     <- rep(c('A','C','E','G'),each=nv)
estVC     <- mxAlgebra(expression = cbind(A,C,E,G),name="VC",dimnames=list(rowVC,colVC))
ciACE     <- mxCI("VC[1,1:4]")


# Combine Groups
pars      <- list( pathA, pathC, pathE, pathG, varage, covA, covC, covE, covG, covP )
modelMZ   <- mxModel( pars, meanG, covMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ   <- mxModel( pars, meanG, covDZ, dataDZ, expDZ, funML, name="DZ" )
fitML     <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction") )
AcegModel  <- mxModel( "ACEG", pars, modelMZ, modelDZ, fitML, estVC, ciACE )

# Run ACEG model
AcegFit    <- mxRun(AcegModel, intervals=T)
AcegSumm   <- summary(AcegFit)
AcegSumm

#Test significance of A
CegModel = omxSetParameters(AcegModel, labels="a11",free=F,values=0)
CegFit = mxRun(CegModel,intervals=T)
mxCompare(AcegFit,CegFit)

#chi-squared Test
mxCompare(SatFit,AcegFit)


#Calculate distance between observed corvariance matrix and model-based corvariance matrix
estcovMZ = rbind(cbind(AcegFit$V$result+AcegFit$G$result,AcegFit$A$result+AcegFit$C$result+AcegFit$G$result),
                 cbind(AcegFit$A$result+AcegFit$C$result+AcegFit$G$result,AcegFit$V$result+AcegFit$G$result))
estcovDZ = rbind(cbind(AcegFit$V$result+AcegFit$G$result,0.5%x%AcegFit$A$result+AcegFit$C$result+AcegFit$G$result),
                 cbind(0.5%x%AcegFit$A$result+AcegFit$C$result+AcegFit$G$result,AcegFit$V$result+AcegFit$G$result))

obscovMZ = as.matrix(cov(mzData,use="complete"))[c(2,3),c(2,3)]
obscovDZ = as.matrix(cov(dzData,use="complete"))[c(2,3),c(2,3)]


covMZ.dist.aceg = log(det(estcovMZ)) + sum(diag(obscovMZ%*%solve(estcovMZ))) - log(det(obscovMZ)) - 2
covDZ.dist.aceg = log(det(estcovDZ)) + sum(diag(obscovDZ%*%solve(estcovDZ))) - log(det(obscovDZ)) - 2
covMZ.dist.aceg
covDZ.dist.aceg