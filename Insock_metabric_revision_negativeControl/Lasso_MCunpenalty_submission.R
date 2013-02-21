### Lasso Molecular Feature only 
rm(list = ls())

###################################################
### step 1: loadLibraries
###################################################
library(predictiveModeling)
library(BCC)
library(federationPLoSRevision)
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

modelClassFile1 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_cancerCensus.R")
modelClassFile2 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_marginalAssociation.R")
modelClassFile3 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_metabricClustering.R")
modelClassFile4 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_topvaryingHiggins.R")
modelClassFile5 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_topvarying.R")
modelClassFile6 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_masp.R")
modelClassFile7 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_OncomapDx.R")
modelClassFile8 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_Mamaprint.R")
modelClassFile9 = ("~/Federation/Insock_metabric_revision/MC_unpenalty/MC_ExpCNV_maspGII.R")

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
# Lasso Grid Setting
alphas = 1
lambdas <- exp(seq(-5, 2, length = 100))

CancerCensus <- MC_ExpCNV_cancerCensus$new()
CancerCensus$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions1 <- CancerCensus$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions1 <- CancerCensus$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

MarginalAssociation <- MC_ExpCNV_marginalAssociation$new()
MarginalAssociation$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions2 <- MarginalAssociation$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions2 <- MarginalAssociation$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

MetabricClustering <- MC_ExpCNV_metabricClustering$new()
MetabricClustering$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions3 <- MetabricClustering$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions3 <- MetabricClustering$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

TopvaringHiggins <- MC_ExpCNV_topvaryingHiggins$new()
TopvaringHiggins$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions4 <- TopvaringHiggins$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions4 <- TopvaringHiggins$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

Topvaring <- MC_ExpCNV_topvarying$new()
Topvaring$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions5 <- Topvaring$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions5 <- Topvaring$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

Masp <- MC_ExpCNV_masp$new()
Masp$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions6 <- Masp$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions6 <- Masp$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

OncomapDx <- MC_ExpCNV_OncomapDx$new()
OncomapDx$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions7 <- OncomapDx$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions7 <- OncomapDx$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

Mammaprint <- MC_ExpCNV_Mamaprint$new()
Mammaprint$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions8 <- Mammaprint$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions8 <- Mammaprint$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)

MaspCGII <- MC_ExpCNV_maspGII$new()
MaspCGII$customTrain(trainingData$exprData,trainingData$copyData,trainingData$clinicalFeaturesData,randSurvTrainingData, alpha = alphas,lambda = lambdas)
trainPredictions9 <- MaspCGII$customPredict(trainingData$exprData, trainingData$copyData, trainingData$clinicalFeaturesData)
testPredictions9 <- MaspCGII$customPredict(testingData$exprData, testingData$copyData, testingData$clinicalFeaturesData)


###################################################
### step 5: computeTrainCIndex
###################################################
trainPerformance1 <- SurvivalModelPerformance$new(as.numeric(trainPredictions1), randSurvTrainingData[rownames(trainPredictions1),])
trainPerformance2 <- SurvivalModelPerformance$new(as.numeric(trainPredictions2), randSurvTrainingData[rownames(trainPredictions2),])
trainPerformance3 <- SurvivalModelPerformance$new(as.numeric(trainPredictions3), randSurvTrainingData[rownames(trainPredictions3),])
trainPerformance4 <- SurvivalModelPerformance$new(as.numeric(trainPredictions4), randSurvTrainingData[rownames(trainPredictions4),])
trainPerformance5 <- SurvivalModelPerformance$new(as.numeric(trainPredictions5), randSurvTrainingData[rownames(trainPredictions5),])
trainPerformance6 <- SurvivalModelPerformance$new(as.numeric(trainPredictions6), randSurvTrainingData[rownames(trainPredictions6),])
trainPerformance7 <- SurvivalModelPerformance$new(as.numeric(trainPredictions7), randSurvTrainingData[rownames(trainPredictions7),])
trainPerformance8 <- SurvivalModelPerformance$new(as.numeric(trainPredictions8), randSurvTrainingData[rownames(trainPredictions8),])
trainPerformance9 <- SurvivalModelPerformance$new(as.numeric(trainPredictions9), randSurvTrainingData[rownames(trainPredictions9),])

print(trainPerformance1$getExactConcordanceIndex())
print(trainPerformance2$getExactConcordanceIndex())
print(trainPerformance3$getExactConcordanceIndex())
print(trainPerformance4$getExactConcordanceIndex())
print(trainPerformance5$getExactConcordanceIndex())
print(trainPerformance6$getExactConcordanceIndex())
print(trainPerformance7$getExactConcordanceIndex())
print(trainPerformance8$getExactConcordanceIndex())
print(trainPerformance9$getExactConcordanceIndex())

testPerformance1 <- SurvivalModelPerformance$new(as.numeric(testPredictions1), randSurvTestData[rownames(testPredictions1),])
testPerformance2 <- SurvivalModelPerformance$new(as.numeric(testPredictions2), randSurvTestData[rownames(testPredictions2),])
testPerformance3 <- SurvivalModelPerformance$new(as.numeric(testPredictions3), randSurvTestData[rownames(testPredictions3),])
testPerformance4 <- SurvivalModelPerformance$new(as.numeric(testPredictions4), randSurvTestData[rownames(testPredictions4),])
testPerformance5 <- SurvivalModelPerformance$new(as.numeric(testPredictions5), randSurvTestData[rownames(testPredictions5),])
testPerformance6 <- SurvivalModelPerformance$new(as.numeric(testPredictions6), randSurvTestData[rownames(testPredictions6),])
testPerformance7 <- SurvivalModelPerformance$new(as.numeric(testPredictions7), randSurvTestData[rownames(testPredictions7),])
testPerformance8 <- SurvivalModelPerformance$new(as.numeric(testPredictions8), randSurvTestData[rownames(testPredictions8),])
testPerformance9 <- SurvivalModelPerformance$new(as.numeric(testPredictions9), randSurvTestData[rownames(testPredictions9),])

print(testPerformance1$getExactConcordanceIndex())
print(testPerformance2$getExactConcordanceIndex())
print(testPerformance3$getExactConcordanceIndex())
print(testPerformance4$getExactConcordanceIndex())
print(testPerformance5$getExactConcordanceIndex())
print(testPerformance6$getExactConcordanceIndex())
print(testPerformance7$getExactConcordanceIndex())
print(testPerformance8$getExactConcordanceIndex())
print(testPerformance9$getExactConcordanceIndex())



###################################################
### step 6: submitModel
###################################################
source("~/Federation/Insock_metabric_revision_negativeControl/submitCompetitionModel_MetabricTrained_InSock_negativeControl.R")
myGeneList1 = "Cancer Census + Clinical" 
myGeneList2 = "Marginal Association + Clinical" 
myGeneList3 = "Metabric Clustering + Clinical" 
myGeneList4 = "Higgins + Clinical" 
myGeneList5 = "Top-varying + Clinical" 
myGeneList6 = "MASP + Clinical" 
myGeneList7 = "OncotypeDX + Clinical" 
myGeneList8 = "Mammaprint + Clinical" 
myGeneList9 = "MASP + Clinical + GII" 



submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical CancerCensus", trainedModel=CancerCensus,rFiles=list(modelClassFile1,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList1)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical Mariginal Association", trainedModel=MarginalAssociation,rFiles=list(modelClassFile2,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList2)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical MetabricClustering", trainedModel=MetabricClustering,rFiles=list(modelClassFile3,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList3)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical Higgins", trainedModel=TopvaringHiggins,rFiles=list(modelClassFile4,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList4)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical Top-varying", trainedModel=Topvaring,rFiles=list(modelClassFile5,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList5)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical Masp", trainedModel=Masp,rFiles=list(modelClassFile6,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList6)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical OncotypeDX", trainedModel=OncomapDx,rFiles=list(modelClassFile7,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList7)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical Mammaprint", trainedModel=Mammaprint,rFiles=list(modelClassFile8,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList8)
submitCompetitionModel_MetabricTrained_InSock_negativeControl(modelName = "Lasso with penalty with expr + copy + clinical Masp Clinical GII", trainedModel=MaspCGII,rFiles=list(modelClassFile9,modelClassFile,modelClassFile0), algorithm = "lasso", geneList= myGeneList9)

