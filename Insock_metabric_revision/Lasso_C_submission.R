### Lasso Molecular Feature only 

###################################################
### step 1: loadLibraries
###################################################
library(predictiveModeling)
library(BCC)
synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")


###################################################ac
### step 2: loadData
###################################################
# synapseLogin() ### not required if configured for automatic login
trainingData <- loadFederationMetabricTrainingData()
testingData <- loadFederationMetabricTestData1()

###################################################
### step 3: call predefined Models' classFile
###################################################

modelClassFile0 = ("~/Federation/Insock_metabric_revision/mapper.R")
modelClassFile = ("~/Federation/Insock_metabric_revision/myEnetCoxModel.R")
source(modelClassFile)
source(modelClassFile0)

modelClassFile1 = ("~/Federation/Insock_metabric_revision/C/Conly.R")
modelClassFile2 = ("~/Federation/Insock_metabric_revision/C/C_GII.R")

source(modelClassFile1)
source(modelClassFile2)

###################################################
### step 4: trainModel
###################################################
# Lasso Grid Setting
alphas = 1
lambdas <- exp(seq(-5, 2, length = 100))

ClinicalOnly <- Conly$new()
ClinicalOnly$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions1 <- ClinicalOnly$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions1 <- ClinicalOnly$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

ClinicalGII <- C_GII$new()
ClinicalGII$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions2 <- ClinicalGII$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions2 <- ClinicalGII$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)



###################################################
### step 5: computeTrainCIndex
###################################################
trainPerformance1 <- SurvivalModelPerformance$new(as.numeric(trainPredictions1), trainingData$clinicalSurvData)
trainPerformance2 <- SurvivalModelPerformance$new(as.numeric(trainPredictions2), trainingData$clinicalSurvData)

print(trainPerformance1$getExactConcordanceIndex())
print(trainPerformance2$getExactConcordanceIndex())

testPerformance1 <- SurvivalModelPerformance$new(as.numeric(testPredictions1), testingData$clinicalSurvData[rownames(testPredictions1),])
testPerformance2 <- SurvivalModelPerformance$new(as.numeric(testPredictions2), testingData$clinicalSurvData[rownames(testPredictions2),])

print(testPerformance1$getExactConcordanceIndex())
print(testPerformance2$getExactConcordanceIndex())


###################################################
### step 6: submitModel
###################################################
source("~/Federation/Insock_metabric_revision/submitCompetitionModel_MetabricTrained_InSock.R")
myGeneList1 = "Clinical" 
myGeneList2 = "Clinical + GII" 

submitCompetitionModel_MetabricTrained_InSock(modelName = "Lasso without penalty with clinical only", trainedModel=ClinicalOnly,rFiles=list(modelClassFile1,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList1)
submitCompetitionModel_MetabricTrained_InSock(modelName = "Lasso without penalty with clinical + GII", trainedModel=ClinicalGII,rFiles=list(modelClassFile2,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList2)
