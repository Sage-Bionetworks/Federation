### Elastic Net Molecular Feature only 
rm(list = ls())

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

survData<-trainingData$clinicalSurvData
set.seed(2)
a1<-sample(499)
randSurvTrainingData<-Surv(survData[a1,1],survData[a1,2])
rownames(randSurvTrainingData)<-rownames(survData)

survTestData<-testingData$clinicalSurvData
set.seed(5)
b1<-sample(480)
randSurvTestData<-Surv(survTestData[b1,1],survTestData[b1,2])
rownames(randSurvTestData)<-rownames(survTestData)

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
# Elastic Net Grid Setting
alphas = seq(0.05,0.95,by=0.05)
lambdas <- exp(seq(-5, 2, length = 100))

ClinicalOnly <- Conly$new()
ClinicalOnly$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions1 <- ClinicalOnly$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions1 <- ClinicalOnly$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

ClinicalGII <- C_GII$new()
ClinicalGII$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions2 <- ClinicalGII$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions2 <- ClinicalGII$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)



###################################################
### step 5: computeTrainCIndex
###################################################
trainPerformance1 <- SurvivalModelPerformance$new(as.numeric(trainPredictions1), randSurvTrainingData[rownames(trainPredictions1),])
trainPerformance2 <- SurvivalModelPerformance$new(as.numeric(trainPredictions2), randSurvTrainingData[rownames(trainPredictions2),])

print(trainPerformance1$getExactConcordanceIndex())
print(trainPerformance2$getExactConcordanceIndex())

testPerformance1 <- SurvivalModelPerformance$new(as.numeric(testPredictions1), randSurvTestData[rownames(testPredictions1),])
testPerformance2 <- SurvivalModelPerformance$new(as.numeric(testPredictions2), randSurvTestData[rownames(testPredictions2),])

print(testPerformance1$getExactConcordanceIndex())
print(testPerformance2$getExactConcordanceIndex())


###################################################
### step 6: submitModel
###################################################
source("~/Federation/Insock_metabric_revision_negativeControl/submitCompetitionModel_MetabricTrained_InSock_negativeControl.R")
myGeneList1 = "Clinical" 
myGeneList2 = "Clinical + GII" 

submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Elastic Net without penalty with clinical only", trainedModel=ClinicalOnly,rFiles=list(modelClassFile1,modelClassFile,modelClassFile0), algorithm = "enet", geneList= myGeneList1)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Elastic Net without penalty with clinical + GII", trainedModel=ClinicalGII,rFiles=list(modelClassFile2,modelClassFile,modelClassFile0), algorithm = "enet", geneList= myGeneList2)
