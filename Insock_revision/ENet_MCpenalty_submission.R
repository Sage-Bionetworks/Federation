### ENet Molecular Feature only 

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
trainingData <- loadMetabricMicmaTrainingData()
testingData <- loadFederationMicmaData()

###################################################
### step 3: call predefined Models' classFile
###################################################

modelClassFile0 = ("~/Federation/Insock_revision/mapper.R")
modelClassFile = ("~/Federation/Insock_revision/myEnetCoxModel.R")
source(modelClassFile)

modelClassFile1 = ("~/DrugResponse/survival_analysis/Insock_revision/MC_penalty/MC_ExpCNV_cancerCensus.R")
modelClassFile2 = ("~/DrugResponse/survival_analysis/Insock_revision/MC_penalty/MC_ExpCNV_marginalAssociation.R")
modelClassFile3 = ("~/DrugResponse/survival_analysis/Insock_revision/MC_penalty/MC_ExpCNV_metabricClustering.R")
modelClassFile4 = ("~/DrugResponse/survival_analysis/Insock_revision/MC_penalty/MC_ExpCNV_topvaryingHiggins.R")
modelClassFile5 = ("~/DrugResponse/survival_analysis/Insock_revision/MC_penalty/MC_ExpCNV_topvarying.R")
modelClassFile6 = ("~/DrugResponse/survival_analysis/Insock_revision/MC_penalty/MC_ExpCNV_masp.R")

source(modelClassFile1)
source(modelClassFile2)
source(modelClassFile3)
source(modelClassFile4)
source(modelClassFile5)
source(modelClassFile6)

###################################################
### step 4: trainModel
###################################################
# ENet Grid Setting
alphas = seq(0.05,0.95,by=0.05)
lambdas <- exp(seq(-5, 2, length = 100))

CancerCensus <- MC_ExpCNV_cancerCensus$new()
CancerCensus$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions1 <- CancerCensus$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions1 <- CancerCensus$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

MarginalAssociation <- MC_ExpCNV_marginalAssociation$new()
MarginalAssociation$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions2 <- MarginalAssociation$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions2 <- MarginalAssociation$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

MetabricClustering <- MC_ExpCNV_metabricClustering$new()
MetabricClustering$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions3 <- MetabricClustering$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions3 <- MetabricClustering$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

TopvaringHiggins <- MC_ExpCNV_topvaryingHiggins$new()
TopvaringHiggins$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions4 <- TopvaringHiggins$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions4 <- TopvaringHiggins$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

Topvaring <- MC_ExpCNV_topvarying$new()
Topvaring$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions5 <- Topvaring$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions5 <- Topvaring$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

Masp <- MC_ExpCNV_topvarying$new()
Masp$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,trainingData$clinicalSurvData, alpha = alphas,lambda = lambdas)
trainPredictions6 <- Masp$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions6 <- Masp$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)


###################################################
### step 5: computeTrainCIndex
###################################################
trainPerformance1 <- SurvivalModelPerformance$new(as.numeric(trainPredictions1), trainingData$clinicalSurvData[rownames(trainPredictions1),])
trainPerformance2 <- SurvivalModelPerformance$new(as.numeric(trainPredictions2), trainingData$clinicalSurvData[rownames(trainPredictions2),])
trainPerformance3 <- SurvivalModelPerformance$new(as.numeric(trainPredictions3), trainingData$clinicalSurvData[rownames(trainPredictions3),])
trainPerformance4 <- SurvivalModelPerformance$new(as.numeric(trainPredictions4), trainingData$clinicalSurvData[rownames(trainPredictions4),])
trainPerformance5 <- SurvivalModelPerformance$new(as.numeric(trainPredictions5), trainingData$clinicalSurvData[rownames(trainPredictions5),])
trainPerformance6 <- SurvivalModelPerformance$new(as.numeric(trainPredictions6), trainingData$clinicalSurvData[rownames(trainPredictions6),])

print(trainPerformance1$getExactConcordanceIndex())
print(trainPerformance2$getExactConcordanceIndex())
print(trainPerformance3$getExactConcordanceIndex())
print(trainPerformance4$getExactConcordanceIndex())
print(trainPerformance5$getExactConcordanceIndex())
print(trainPerformance6$getExactConcordanceIndex())

testPerformance1 <- SurvivalModelPerformance$new(as.numeric(testPredictions1), testingData$clinicalSurvData[rownames(testPredictions1),])
testPerformance2 <- SurvivalModelPerformance$new(as.numeric(testPredictions2), testingData$clinicalSurvData[rownames(testPredictions2),])
testPerformance3 <- SurvivalModelPerformance$new(as.numeric(testPredictions3), testingData$clinicalSurvData[rownames(testPredictions3),])
testPerformance4 <- SurvivalModelPerformance$new(as.numeric(testPredictions4), testingData$clinicalSurvData[rownames(testPredictions4),])
testPerformance5 <- SurvivalModelPerformance$new(as.numeric(testPredictions5), testingData$clinicalSurvData[rownames(testPredictions5),])
testPerformance6 <- SurvivalModelPerformance$new(as.numeric(testPredictions6), testingData$clinicalSurvData[rownames(testPredictions6),])

print(testPerformance1$getExactConcordanceIndex())
print(testPerformance2$getExactConcordanceIndex())
print(testPerformance3$getExactConcordanceIndex())
print(testPerformance4$getExactConcordanceIndex())
print(testPerformance5$getExactConcordanceIndex())
print(testPerformance6$getExactConcordanceIndex())



###################################################
### step 6: submitModel
###################################################
source("~/Federation/Insock_revision/submitCompetitionModel_micmaTrained_InSock.R")
myGeneList1 = "CancerCensus + clinical" 
myGeneList2 = "MarginalAssociation + clinical" 
myGeneList3 = "MetabricClustering + clinical" 
myGeneList4 = "TopVaringHiggins + clinical" 
myGeneList5 = "TopVaring + clinical" 
myGeneList6 = "MASP + clinical" 

submitCompetitionModel_micmaTrained_InSock(modelName = "ENet without penalty with expr +copy + clinical CancerCensus", trainedModel=CancerCensus,rFiles=list(modelClassFile1,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList1)
submitCompetitionModel_micmaTrained_InSock(modelName = "ENet without penalty with expr +copy + clinical Mariginal Association", trainedModel=MarginalAssociation,rFiles=list(modelClassFile2,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList2)
submitCompetitionModel_micmaTrained_InSock(modelName = "ENet without penalty with expr +copy + clinical MetabricClustering", trainedModel=MetabricClustering,rFiles=list(modelClassFile3,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList3)
submitCompetitionModel_micmaTrained_InSock(modelName = "ENet without penalty with expr +copy + clinical TopvaringHiggins", trainedModel=TopvaringHiggins,rFiles=list(modelClassFile4,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList4)
submitCompetitionModel_micmaTrained_InSock(modelName = "ENet without penalty with expr +copy + clinical Topvaring", trainedModel=Topvaring,rFiles=list(modelClassFile5,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList5)
submitCompetitionModel_micmaTrained_InSock(modelName = "ENet without penalty with expr +copy + clinical Masp", trainedModel=Masp,rFiles=list(modelClassFile5,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList6)

