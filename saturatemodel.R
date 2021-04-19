# Twin Univariate Saturated model to estimate means and covariances

library(OpenMx)


#Select Variables for Analysis
vars <- 'avePD'
nv = 1
ntv = nv*2
selVars = paste(vars,c(rep(1,nv),rep(2,nv)),sep="")

#Select Data for Analysis
mzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==1, selVars)))
dzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==2, selVars)))

# Set Starting Values
svMe      <- 0   # start value for means
svVa      <- 1      # start value for variance
svVas     <- diag(svVa,ntv,ntv)

#Saturated Model
#Create Algebra for expected Mean Matrices
meanMZ = mxMatrix(type = "Full",nrow=1, ncol=ntv, free=T, values=svMe, labels=c("mMZ1","mMZ2"), name="meanMZ")
meanDZ = mxMatrix(type = "Full",nrow=1, ncol=ntv, free=T, values=svMe, labels=c("mDZ1","mDZ2"), name="meanDZ")

#Create Algebra for expected Variance/Covariance Matrices
covMZ = mxMatrix(type="Symm", nrow=ntv, ncol=ntv, free=T, values=svVas, labels=c("vMZ1","cMZ21","vMZ2"), name="covMZ")
covDZ = mxMatrix(type="Symm", nrow=ntv, ncol=ntv, free=T, values=svVas, labels=c("vDZ1","cDZ21","vDZ2"), name="covDZ")

#Create Algebra for Maximum Likelihood Estimates of Twin Correlations
#matI = mxMatrix(type="Iden", nrow=ntv, ncol=ntv, name="I")
#corMZ = mxAlgebra(solve(sqrt(I*covMZ)) %&% covMZ, name="corMZ")
#corDZ = mxAlgebra(solve(sqrt(I*covDZ)) %&% covDZ, name="corDZ")

#Create Data Objects 
dataMZ = mxData(observed=mzData, type="raw")
dataDZ = mxData(observed=dzData, type="raw")

#Create Expectation Objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="covMZ", means="meanMZ",
                                  dimnames=selVars )
expDZ     <- mxExpectationNormal( covariance="covDZ", means="meanDZ",
                                  dimnames=selVars )
funML     <- mxFitFunctionML()

# Combine Groups
modelMZ   <- mxModel( meanMZ, covMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ   <- mxModel( meanDZ, covDZ, dataDZ, expDZ, funML, name="DZ" )
fitML     <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction") )
SaturatedModel  <- mxModel( "oneSAT", modelMZ, modelDZ, fitML )

# Run Saturated model
SatFit    <- mxRun(SaturatedModel, intervals=T)
SatSumm   <- summary(SatFit)
SatSumm


#==============================================================================================
#Twin Bivariate Saturated model to estimate means and covariances

#Define some Functions
labFull   <- function(lab,nr,nc) { paste(lab,1:nr,rep(1:nc,each=nr),sep="") }
labSymm   <- function(lab,nv) { paste(lab,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="") }
labLower  <- function(lab,nv) { paste(lab,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="") }


# Select Variables for Analysis
Vars      <- c('IMT','avePD')
nv        <- 2       # number of variables
ntv       <- nv*2    # number of total variables
selVars   <- paste(Vars,c(rep(1,nv),rep(2,nv)),sep="")

# Select Data for Analysis
mzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==1, selVars)))
dzData    <- as.data.frame(scale(subset(nosmokePD.F, zyg==2, selVars)))
cov(mzData,use="complete")
cov(dzData,use="complete")

# Saturated Model
# Set Starting Values
svMe      <- rep(0,nv)            # start value for means
svVa      <- diag(0.5,ntv)          # start values for variances

# Algebra for expected Mean Matrices in MZ & DZ twins
meanMZ    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE,
                       values=svMe, labels=labFull("meMZ",1,ntv), name="expMeanMZ" )
meanDZ    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE,
                       values=svMe, labels=labFull("meDZ",1,ntv), name="expMeanDZ" )

# Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covMZ     <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE,
                       values=svVa, labels=labSymm("vaMZ",ntv), name="expCovMZ" )
covDZ     <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE,
                       values=svVa, labels=labSymm("vaDZ",ntv), name="expCovDZ" )

# Data objects for Multiple Groups
dataMZ    <- mxData( observed=mzData, type="raw" )
dataDZ    <- mxData( observed=dzData, type="raw" )


#Create Expectation Objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="expCovMZ", means="expMeanMZ",
                                  dimnames=selVars )
expDZ     <- mxExpectationNormal( covariance="expCovDZ", means="expMeanDZ",
                                  dimnames=selVars )
funML     <- mxFitFunctionML()

# Combine Groups
modelMZ   <- mxModel( meanMZ, covMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ   <- mxModel( meanDZ, covDZ, dataDZ, expDZ, funML, name="DZ" )
fitML     <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction") )
SaturatedModel  <- mxModel( "oneSAT", modelMZ, modelDZ, fitML )

# Run Saturated model
SatFit    <- mxRun(SaturatedModel, intervals=T)
SatSumm   <- summary(SatFit)
SatSumm
