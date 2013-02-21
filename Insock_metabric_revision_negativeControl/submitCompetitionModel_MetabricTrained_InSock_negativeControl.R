require(utils)
require(sessionTools)
require(devtools)

submitCompetitionModel_MetabricTrained_InSock_negativeControl <- function(modelName = NULL, trainedModel=NULL,
                                                rFiles=NULL, algorithm=NULL, geneList=NULL, cvPerformance=NULL, parentDatasetId = "syn1682334"){
  
  submittedModelLayer <- Data(list(name = modelName, parentId = parentDatasetId))
  
  for (curRFile in rFiles){
    submittedModelLayer <- addFile(submittedModelLayer, curRFile)
  }
  
  submittedModelLayer <- addObject(submittedModelLayer, trainedModel, "trainedModel")
  submittedModelLayer <- addObject(submittedModelLayer, cvPerformance, "cvPerformance")
  submittedModelLayer <- addObject(submittedModelLayer, sessionSummary(), "sessionSummary")
  
  metabricTrainingData <- loadFederationMetabricTrainingData()
  metabricTestingData1 <- loadFederationMetabricTestData1()
  
  
  survData<-metabricTrainingData$clinicalSurvData
  set.seed(2)
  a1<-sample(499)
  randSurvTrainingData<-Surv(survData[a1,1],survData[a1,2])
  rownames(randSurvTrainingData)<-rownames(survData)
  
  survTestData<-metabricTestingData1$clinicalSurvData
  set.seed(5)
  b1<-sample(480)
  randSurvTestData<-Surv(survTestData[b1,1],survTestData[b1,2])
  rownames(randSurvTestData)<-rownames(survTestData)
  
  
  metabricPredictions_train <- trainedModel$customPredict(metabricTrainingData$exprData, metabricTrainingData$copyData,
                                                          metabricTrainingData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions_train), randSurvTrainingData)
  cIndex_train <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_train", cIndex_train))
  
  
  metabricPredictions1 <- trainedModel$customPredict(metabricTestingData1$exprData, metabricTestingData1$copyData,
                                                     metabricTestingData1$clinicalFeaturesData)
  testPerformance1 <- SurvivalModelPerformance$new(as.numeric(metabricPredictions1), randSurvTestData)
  cIndex_metabric1 <- testPerformance1$getExactConcordanceIndex()
  print(paste("cIndex_metabric1", cIndex_metabric1))
  
  
  submittedModelLayer <- addObject(submittedModelLayer, metabricPredictions1, "metabricPredictions1")
  
  submittedModelLayer$annotations$cIndex_train <- cIndex_train
  submittedModelLayer$annotations$cIndex_metabric2 <- cIndex_metabric1
  
  submittedModelLayer$annotations$geneList <- geneList
  submittedModelLayer$annotations$algorithm <- algorithm
  
  submittedModelLayer <- storeEntity(submittedModelLayer)
    
}
