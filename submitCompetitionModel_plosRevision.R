require(utils)
require(sessionTools)

#' Submit a trained model for a breast cancer competition
#'
#' Uploads a trained model and model code to Synapse to be evaluated in the breast cancer competition.
#'
#' @param modelName the name of the model that will be stored in Synapse
#' @param trainedModel object that has been trained using the training data. The customPredict() method of this model
#' will be called with the validation data to evaluate model performance.
#' @param rFiles list of files required to train the model. One of the files must be the class file defining the class of
#' trained model and defining the customTrain() and customPredict() methods. Other files are dependencies required to run
#' customTrain() and customPredict() on the class file.
#' @param cvPerformance optional argument containing an object of class SurvivalModelPerformanceCV as returned by the function
#' crossValidatePredictiveSurvivalModel.
#' @param isPracticeModel binary argument specifying if the model is uploaded to the Synapse dataset containing practice models
#' or models to be scored in the competition. Set to FALSE if you do not want the model to be evaluated.
#'
#' @author Adam Margolin
#' @export

submitCompetitionModel_plosRevision <- function(modelName = NULL, trainedModel=NULL,
                                   rFiles=NULL, cvPerformance=NULL, parentDatasetId = "syn1642232"){
  
  submittedModelLayer <- Data(list(name = modelName, parentId = parentDatasetId))
  
  for (curRFile in rFiles){
    submittedModelLayer <- addFile(submittedModelLayer, curRFile)
  }
  
  submittedModelLayer <- addObject(submittedModelLayer, trainedModel, "trainedModel")
  submittedModelLayer <- addObject(submittedModelLayer, cvPerformance, "cvPerformance")
  submittedModelLayer <- addObject(submittedModelLayer, sessionSummary(), "sessionSummary")
  
  metabricTrainingData <- loadMetabricTrainingData_federation()
  metabricTestData1 <- loadFederationMetabricTestData1()
  metabricTestData2 <- loadFederationMetabricTestData2()
  micmaData <- loadFederationMicmaData()
  
  metabricPredictions_train <- trainedModel$customPredict(metabricTrainingData$exprData, metabricTrainingData$copyData,
                                                          metabricTrainingData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions_train), metabricTrainingData$clinicalSurvData)
  cIndex_train <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_train", cIndex_train))
  
  metabricPredictions1 <- trainedModel$customPredict(metabricTestData1$exprData, metabricTestData1$copyData,
                                                metabricTestData1$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions1), metabricTestData1$clinicalSurvData)
  cIndex_metabric1 <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_metabric1", cIndex_metabric1))
  
  metabricPredictions2 <- trainedModel$customPredict(metabricTestData2$exprData, metabricTestData2$copyData,
                                                metabricTestData2$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(metabricPredictions2), metabricTestData2$clinicalSurvData)
  cIndex_metabric2 <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_metabric2", cIndex_metabric2))
  
  micmaPredictions <- trainedModel$customPredict(micmaData$exprData, micmaData$copyData,
                                             micmaData$clinicalFeaturesData)
  trainPerformance <- SurvivalModelPerformance$new(as.numeric(micmaPredictions), micmaData$clinicalSurvData)
  cIndex_micma <- trainPerformance$getExactConcordanceIndex()
  print(paste("cIndex_micma", cIndex_micma))
  
  submittedModelLayer <- addObject(submittedModelLayer, metabricPredictions1, "metabricPredictions1")
  submittedModelLayer <- addObject(submittedModelLayer, metabricPredictions2, "metabricPredictions2")
  submittedModelLayer <- addObject(submittedModelLayer, micmaPredictions, "micmaPredictions")
  
  submittedModelLayer <- addObject(submittedModelLayer, cIndex_train)
  submittedModelLayer <- addObject(submittedModelLayer, cIndex_metabric1)
  submittedModelLayer <- addObject(submittedModelLayer, cIndex_metabric2)
  submittedModelLayer <- addObject(submittedModelLayer, cIndex_micma)
  
  submittedModelLayer <- storeEntity(submittedModelLayer)
  
  propertyValue(submittedModelLayer, "id")
}
