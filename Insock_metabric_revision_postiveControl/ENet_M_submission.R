### Elastic Net Molecular Feature only 
rm(list = ls())
###################################################
### step 1: loadLibraries
###################################################
library(predictiveModeling)
library(federationPLoSRevision)
library(BCC)
synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")


###################################################ac
### step 2: loadData
###################################################
# synapseLogin() ### not required if configured for automatic login
trainingData <- loadFederationMetabricTrainingData()
testingData <- loadFederationMetabricTestData1()

TrainingERdata <- trainingData$clinicalFeaturesData$ER_IHC_status
names(TrainingERdata)<-rownames(trainingData$clinicalFeaturesData)

TestingERdata <- testingData$clinicalFeaturesData$ER_IHC_status
names(TestingERdata)<-rownames(testingData$clinicalFeaturesData)

###################################################
### step 3: call predefined Models' classFile
###################################################

modelClassFile = ("~/DrugResponse/R5/myCatEnetModel.R")
source(modelClassFile)

modelClassFile1 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_cancerCensus.R")
modelClassFile2 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_marginalAssociation.R")
modelClassFile3 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_metabricClustering.R")
modelClassFile4 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_topvaryingHiggins.R")
modelClassFile5 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_topvarying.R")
modelClassFile6 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_masp.R")
modelClassFile7 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_OncomapDx.R")
modelClassFile8 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_Mamaprint.R")
modelClassFile9 = ("~/Federation/Insock_metabric_revision_postiveControl/M/M_ExpCNV_maspGII.R")

source(modelClassFile1)
source(modelClassFile2)
source(modelClassFile3)
source(modelClassFile4)
source(modelClassFile5)
source(modelClassFile6)
source(modelClassFile7)
source(modelClassFile8)
source(modelClassFile9)

###################################################
### step 4: trainModel
###################################################
# Elastic Net Grid Setting
alphas = seq(0.05,0.95,by=0.05)
lambdas <- exp(seq(-5, 2, length = 100))

CancerCensus <- M_ExpCNV_cancerCensus$new()
CancerCensus$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions1 <- CancerCensus$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions1 <- CancerCensus$customPredict(testingData$exprData, testingData$copyData)

MarginalAssociation <- M_ExpCNV_marginalAssociation$new()
MarginalAssociation$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions2 <- MarginalAssociation$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions2 <- MarginalAssociation$customPredict(testingData$exprData, testingData$copyData)

MetabricClustering <- M_ExpCNV_metabricClustering$new()
MetabricClustering$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions3 <- MetabricClustering$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions3 <- MetabricClustering$customPredict(testingData$exprData, testingData$copyData)

TopvaringHiggins <- M_ExpCNV_topvaryingHiggins$new()
TopvaringHiggins$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions4 <- TopvaringHiggins$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions4 <- TopvaringHiggins$customPredict(testingData$exprData, testingData$copyData)

Topvaring <- M_ExpCNV_topvarying$new()
Topvaring$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions5 <- Topvaring$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions5 <- Topvaring$customPredict(testingData$exprData, testingData$copyData)

Masp <- M_ExpCNV_masp$new()
Masp$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions6 <- Masp$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions6 <- Masp$customPredict(testingData$exprData, testingData$copyData)

OncomapDx <- M_ExpCNV_OncomapDx$new()
OncomapDx$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions7 <- OncomapDx$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions7 <- OncomapDx$customPredict(testingData$exprData, testingData$copyData)

Mammaprint <- M_ExpCNV_Mamaprint$new()
Mammaprint$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions8 <- Mammaprint$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions8 <- Mammaprint$customPredict(testingData$exprData, testingData$copyData)

MaspGII <- M_ExpCNV_maspGII$new()
MaspGII$customTrain(trainingData$exprData,trainingData$copyData,TrainingERdata, alpha = alphas,lambda = lambdas)
trainPredictions9 <- MaspGII$customPredict(trainingData$exprData, trainingData$copyData)
testPredictions9 <- MaspGII$customPredict(testingData$exprData, testingData$copyData)


###################################################
### step 5: computeTrainCIndex
###################################################
trainPerformance1 <- performance(prediction(trainPredictions1, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance2 <- performance(prediction(trainPredictions2, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance3 <- performance(prediction(trainPredictions3, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance4 <- performance(prediction(trainPredictions4, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance5 <- performance(prediction(trainPredictions5, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance6 <- performance(prediction(trainPredictions6, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance7 <- performance(prediction(trainPredictions7, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance8 <- performance(prediction(trainPredictions8, TrainingERdata), 'auc')@y.values[[1]]
trainPerformance9 <- performance(prediction(trainPredictions9, TrainingERdata), 'auc')@y.values[[1]]

print(trainPerformance1)
print(trainPerformance2)
print(trainPerformance3)
print(trainPerformance4)
print(trainPerformance5)
print(trainPerformance6)
print(trainPerformance7)
print(trainPerformance8)
print(trainPerformance9)

testPerformance1 <- performance(prediction(testPredictions1, TestingERdata), 'auc')@y.values[[1]]
testPerformance2 <- performance(prediction(testPredictions2, TestingERdata), 'auc')@y.values[[1]]
testPerformance3 <- performance(prediction(testPredictions3, TestingERdata), 'auc')@y.values[[1]]
testPerformance4 <- performance(prediction(testPredictions4, TestingERdata), 'auc')@y.values[[1]]
testPerformance5 <- performance(prediction(testPredictions5, TestingERdata), 'auc')@y.values[[1]]
testPerformance6 <- performance(prediction(testPredictions6, TestingERdata), 'auc')@y.values[[1]]
testPerformance7 <- performance(prediction(testPredictions7, TestingERdata), 'auc')@y.values[[1]]
testPerformance8 <- performance(prediction(testPredictions8, TestingERdata), 'auc')@y.values[[1]]
testPerformance9 <- performance(prediction(testPredictions9, TestingERdata), 'auc')@y.values[[1]]

print(testPerformance1)
print(testPerformance2)
print(testPerformance3)
print(testPerformance4)
print(testPerformance5)
print(testPerformance6)
print(testPerformance7)
print(testPerformance8)
print(testPerformance9)



###################################################
### step 6: submitModel
###################################################
source("~/Federation/Insock_metabric_revision_postiveControl/submitCompetitionModel_MetabricTrained_InSock_positiveControl.R")
myGeneList1 = "Cancer Census" 
myGeneList2 = "Marginal Association" 
myGeneList3 = "Metabric Clustering" 
myGeneList4 = "Higgins" 
myGeneList5 = "Top-varying" 
myGeneList6 = "MASP" 
myGeneList7 = "OncotypeDX" 
myGeneList8 = "Mammaprint" 
myGeneList9 = "MASP + GII" 


submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy CancerCensus", trainedModel=CancerCensus,rFiles=list(modelClassFile1,modelClassFile), algorithm = "enet", geneList= myGeneList1)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy Mariginal Association", trainedModel=MarginalAssociation,rFiles=list(modelClassFile2,modelClassFile), algorithm = "enet", geneList= myGeneList2)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy MetabricClustering", trainedModel=MetabricClustering,rFiles=list(modelClassFile3,modelClassFile), algorithm = "enet", geneList= myGeneList3)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy Higgins", trainedModel=TopvaringHiggins,rFiles=list(modelClassFile4,modelClassFile), algorithm = "enet", geneList= myGeneList4)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy Top-varying", trainedModel=Topvaring,rFiles=list(modelClassFile5,modelClassFile), algorithm = "enet", geneList= myGeneList5)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy Masp", trainedModel=Masp,rFiles=list(modelClassFile6,modelClassFile), algorithm = "enet", geneList= myGeneList6)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy OncotypeDX", trainedModel=OncomapDx,rFiles=list(modelClassFile7,modelClassFile), algorithm = "enet", geneList= myGeneList7)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy Mammaprint", trainedModel=Mammaprint,rFiles=list(modelClassFile8,modelClassFile), algorithm = "enet", geneList= myGeneList8)
submitCompetitionModel_MetabricTrained_InSock_positiveControl(modelName = "Elastic Net without penalty with expr + copy Masp + GII", trainedModel=MaspGII,rFiles=list(modelClassFile9,modelClassFile), algorithm = "enet", geneList= myGeneList9)

