require(utils)
require(sessionTools)
require(devtools)
require(ROCR)

submitCompetitionModel_MetabricTrained_InSock_positiveControl <- function(modelName = NULL, trainedModel=NULL,
                                                rFiles=NULL, algorithm=NULL, geneList=NULL, cvPerformance=NULL, parentDatasetId = "syn1687665"){
  
  submittedModelLayer <- Data(list(name = modelName, parentId = parentDatasetId))
  
  for (curRFile in rFiles){
    submittedModelLayer <- addFile(submittedModelLayer, curRFile)
  }
  
  submittedModelLayer <- addObject(submittedModelLayer, trainedModel, "trainedModel")
  submittedModelLayer <- addObject(submittedModelLayer, cvPerformance, "cvPerformance")
  submittedModelLayer <- addObject(submittedModelLayer, sessionSummary(), "sessionSummary")
  
  metabricTrainingData <- loadFederationMetabricTrainingData()
  metabricTestingData1 <- loadFederationMetabricTestData1()
  
  
  TrainingERdata <- metabricTrainingData$clinicalFeaturesData$ER_IHC_status
  names(TrainingERdata)<-rownames(metabricTrainingData$clinicalFeaturesData)
  
  TestingERdata <- metabricTestingData1$clinicalFeaturesData$ER_IHC_status
  names(TestingERdata)<-rownames(metabricTestingData1$clinicalFeaturesData)
  
  
  metabricPredictions_train <- trainedModel$customPredict(metabricTrainingData$exprData, metabricTrainingData$copyData)
  trainPerformance <- performance(prediction(metabricPredictions_train, TrainingERdata), 'auc')@y.values[[1]]
  
  cIndex_train <- trainPerformance
  print(paste("cIndex_train", cIndex_train))
  
  
  metabricPredictions1 <- trainedModel$customPredict(metabricTestingData1$exprData, metabricTestingData1$copyData)
  testPerformance <- performance(prediction(metabricPredictions1, TestingERdata), 'auc')@y.values[[1]]
  cIndex_metabric1 <- testPerformance
  print(paste("cIndex_metabric1", cIndex_metabric1))
  
  
  submittedModelLayer <- addObject(submittedModelLayer, metabricPredictions1, "metabricPredictions1")
  
  submittedModelLayer$annotations$cIndex_train <- cIndex_train
  submittedModelLayer$annotations$cIndex_metabric1 <- cIndex_metabric1
  
  submittedModelLayer$annotations$geneList <- geneList
  submittedModelLayer$annotations$algorithm <- algorithm
  
  submittedModelLayer <- storeEntity(submittedModelLayer)
    
}
