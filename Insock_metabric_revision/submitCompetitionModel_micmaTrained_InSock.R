require(utils)
require(sessionTools)
require(devtools)

submitCompetitionModel_micmaTrained_InSock <- function(modelName = NULL, trainedModel=NULL,
                                                rFiles=NULL, algorithm=NULL, geneList=NULL, cvPerformance=NULL, parentDatasetId = "syn1646909"){
  
  submittedModelLayer <- Data(list(name = modelName, parentId = parentDatasetId))
  
  for (curRFile in rFiles){
    submittedModelLayer <- addFile(submittedModelLayer, curRFile)
  }
  
  submittedModelLayer <- addObject(submittedModelLayer, trainedModel, "trainedModel")
  submittedModelLayer <- addObject(submittedModelLayer, cvPerformance, "cvPerformance")
  submittedModelLayer <- addObject(submittedModelLayer, sessionSummary(), "sessionSummary")
  
  metabricTrainingData <- loadFederationMetabricTrainingData()
  metabricTestingData <- loadFederationMetabricTestData1()
  
  metabricPredictions_train <- trainedModel$customPredict(metabricTrainingData$exprData, metabricTrainingData$copyData,
                                                          metabricTrainingData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions_train), metabricTrainingData$clinicalSurvData[rownames(metabricPredictions_train),])
  cIndex_train <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_train", cIndex_train))
  
  
  metabricPredictions1 <- trainedModel$customPredict(metabricTestingData$exprData, metabricTestingData$copyData,
                                                 metabricTestingData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions1), metabricTestingData$clinicalSurvData[rownames(metabricPredictions1),])
  cIndex_metabric1 <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_metabric1", cIndex_metabric1))
  
  submittedModelLayer <- addObject(submittedModelLayer, metabricPredictions1, "metabricPredictions1")
  
  submittedModelLayer$annotations$cIndex_train <- cIndex_train
  submittedModelLayer$annotations$cIndex_metabric1 <- cIndex_metabric1
  
  submittedModelLayer$annotations$geneList <- geneList
  submittedModelLayer$annotations$algorithm <- algorithm
  
  submittedModelLayer <- storeEntity(submittedModelLayer)
  
  #   source_url("https://raw.github.com/AAMargolin/AdamTestCode/master/synapseExecute/addTableDescriptionToFolderEntity.R")
  #   addTableDescriptionToFolderEntity(parentDatasetId)
}
