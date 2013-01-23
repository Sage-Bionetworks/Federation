source('http://depot.sagebase.org/CRAN.R')
pkgInstall(c("BCC","predictiveModeling"))


###################################################
### step 1: loadLibraries
###################################################
library(predictiveModeling)
library(BCC)
library(survival)
library(survcomp)
library(MASS)

###### these are the utility functions to write #############

source("~/Federation/loadFederationMetabricTrainingData.R")
source("~/Federation/loadFederationMetabricTestData1.R")
source("~/Federation/loadFederationMetabricTestData2.R")
source("~/Federation/loadFederationMicmaData.R")

metabricTrainingData <- loadFederationMetabricTrainingData()
metabricTestData1 <- loadFederationMetabricTestData1()
metabricTestData2 <- loadFederationMetabricTestData2()
micmaData <- loadFederationMicmaData()


# only difference between previous Federation model submission was "Clinical Variates"
# Coxph model is used for this unit test with new validation dataset
source("~/Federation/CoxphModel.R")

# Metabric
coxTest<-CoxphModel$new()
coxTest$customTrain(metabricTrainingData$exprData, metabricTrainingData$copyData,
                    metabricTrainingData$clinicalFeaturesData, metabricTrainingData$clinicalSurvData)

metabricPredictions1 <- coxTest$customPredict(metabricTestData1$exprData, metabricTestData1$copyData,
                            metabricTestData1$clinicalFeaturesData)

metabricPredictions2 <- coxTest$customPredict(metabricTestData2$exprData, metabricTestData2$copyData,
                                              metabricTestData2$clinicalFeaturesData)

# covariate in common
metabric <- metabricTrainingData$clinicalFeaturesData
micma <- micmaData$clinicalFeaturesData
inCommon <- intersect(names(metabric),names(micma))
micmaData$clinicalFeaturesData <- micma[,inCommon]
metabricTrainingData$clinicalFeaturesData <- metabric[,inCommon]


# Metabric with MICMA
coxTest2<-CoxphModel$new()
coxTest2$customTrain(metabricTrainingData$exprData, metabricTrainingData$copyData,
                     metabricTrainingData$clinicalFeaturesData, metabricTrainingData$clinicalSurvData)
micmaPredictions <- coxTest2$customPredict(micmaData$exprData, micmaData$copyData,
                                          micmaData$clinicalFeaturesData)

