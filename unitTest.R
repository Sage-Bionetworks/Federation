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

###### these are the utility functions to write ##############
metabricTrainingData <- loadFederationMetabricTrainingData()
metabricTestData1 <- loadFederationMetabricTestData1()
metabricTestData2 <- loadFederationMetabricTestData2()
micmaData <- loadFederationMicmaData()


# only difference between previous Federation model submission was "Clinical Variates"
# Coxph model is used for this unit test with new validation dataset
source("~/Federation/CoxphModel.R")

#training
coxTest<-CoxphModel$new()
coxTest$customTrain(metabricTrainingData$exprData, metabricTrainingData$copyData,
                    metabricTrainingData$clinicalFeaturesData, metabricTrainingData$clinicalSurvData)

metabricPredictions1 <- coxTest$customPredict(metabricTestData1$exprData, metabricTestData1$copyData,
                            metabricTestData1$clinicalFeaturesData)

metabricPredictions1 <- coxTest$customPredict(micmaData$exprData, micmaData$copyData,
                                              micmaData$clinicalFeaturesData)
