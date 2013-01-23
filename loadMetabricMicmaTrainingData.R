#' Load training data for the metabric reevaluation
#'
#' Loads data for gene expression, copy number, clinical covariates and clinical survival times for the training dataset
#' used to train predicitve models.
#'
#' @return a list with elements: exprData, containing an ExpressionSet of gene expression features;
#' copyData, containing an ExpressionSet of copy number features; clinicalFeaturesData, containing an
#' AnnotatedDataFrame of clinical covariates; and clinicalSurvData, containing a Surv object
#' of survival times and censor values.
#' @author Adam Margolin and In Sock
#' @export

loadMetabricMicmaTrainingData <- function(){
  metabricTrainingData <- list()
  
  idExpressionLayer <- "syn1588845"
  expressionLayer <- loadEntity(idExpressionLayer)
  metabricTrainingData$exprData <- expressionLayer$objects[[1]]
  
  idCopyLayer <- "syn1589116"
  copyLayer <- loadEntity(idCopyLayer)
  metabricTrainingData$copyData <- copyLayer$objects[[1]]
  
  idClinicalFeaturesLayer <- "syn1643466"
  clinicalFeaturesLayer <- loadEntity(idClinicalFeaturesLayer)
  metabricTrainingData$clinicalFeaturesData <- clinicalFeaturesLayer$objects[[1]]@data
  
  idClinicalSurvLayer <- "syn1643540"
  clinicalSurvLayer <- loadEntity(idClinicalSurvLayer)
  metabricTrainingData$clinicalSurvData <- clinicalSurvLayer$objects$clinicalSurv_train
  
  
  return(metabricTrainingData)
}

