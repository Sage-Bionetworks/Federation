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
                                   A<-clinicalFeaturesData                                   
                                   
                                   .self$model <- coxph(clinicalSurvData ~., data=A)
                                   
                                 },
                                 
                                 customPredict = function(exprData, copyData, clinicalFeaturesData){
                                   A<-clinicalFeaturesData
                                   
                                   predictedResponse <- predict(.self$model, data=A)
                                   
                                   return(predictedResponse)
                                 }
                                 )
                               )
