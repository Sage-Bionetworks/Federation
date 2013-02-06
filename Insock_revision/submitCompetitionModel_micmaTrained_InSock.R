require(utils)
require(sessionTools)
require(devtools)

submitCompetitionModel_micmaTrained_InSock <- function(modelName = NULL, trainedModel=NULL,
                                                rFiles=NULL, algorithm=NULL, geneList=NULL, cvPerformance=NULL, parentDatasetId = "syn1642232"){
  
  submittedModelLayer <- Data(list(name = modelName, parentId = parentDatasetId))
  
  for (curRFile in rFiles){
    submittedModelLayer <- addFile(submittedModelLayer, curRFile)
  }
  
  submittedModelLayer <- addObject(submittedModelLayer, trainedModel, "trainedModel")
  submittedModelLayer <- addObject(submittedModelLayer, cvPerformance, "cvPerformance")
  submittedModelLayer <- addObject(submittedModelLayer, sessionSummary(), "sessionSummary")
  
  metabricTrainingData <- loadMetabricMicmaTrainingData()
  micmaData <- loadFederationMicmaData()
  
  metabricPredictions_train <- trainedModel$customPredict(metabricTrainingData$exprData, metabricTrainingData$copyData,
                                                          metabricTrainingData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions_train), metabricTrainingData$clinicalSurvData[rownames(metabricPredictions_train),])
  cIndex_train <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_train", cIndex_train))
  
  
  micmaPredictions <- trainedModel$customPredict(micmaData$exprData, micmaData$copyData,
                                                 micmaData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(micmaPredictions), micmaData$clinicalSurvData[rownames(micmaPredictions),])
  cIndex_micma <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_micma", cIndex_micma))
  
  submittedModelLayer <- addObject(submittedModelLayer, micmaPredictions, "micmaPredictions")
  
  submittedModelLayer$annotations$cIndex_train <- cIndex_train
  submittedModelLayer$annotations$cIndex_micma <- cIndex_micma
  
  submittedModelLayer$annotations$geneList <- geneList
  submittedModelLayer$annotations$algorithm <- algorithm
  
  submittedModelLayer <- storeEntity(submittedModelLayer)
  
  #   source_url("https://raw.github.com/AAMargolin/AdamTestCode/master/synapseExecute/addTableDescriptionToFolderEntity.R")
  #   addTableDescriptionToFolderEntity(parentDatasetId)
}
