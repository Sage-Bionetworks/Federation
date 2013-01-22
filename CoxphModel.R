require(survival)

CoxphModel <- setRefClass(Class = "CoxphModel",
                               fields=c("model","numFeatures","numRounds"),
                               
                               methods = list(
                                 initialize = function(...){
                                   return(.self)
                                 },
                                 
                                 rawModel = function(){
                                   return(.self$model)
                                 },
                                 
                                 customTrain = function(exprData, copyData, clinicalFeaturesData, clinicalSurvData,  ...){
                                   
                                   .self$model <- coxph(clinicalSurvData ~., data=clinicalFeaturesData)
                                   
                                 },
                                 
                                 customPredict = function(exprData, copyData, clinicalFeaturesData){
                                   
                                   
                                   predictedResponse <- predict(.self$model, clinicalFeaturesData)
                                   
                                   return(predictedResponse)
                                 }
                                 )
                               )
