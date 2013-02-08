Conly <- setRefClass(Class = "Conly",                              
                     contains="PredictiveModel",
                     fields=c("model","childclass"),
                     methods = list(
                       initialize = function(...){
                         return(.self)
                       },
                       
                       rawModel = function(){
                         return(.self$model)
                       },
                       
                       customTrain = function(exprData,copyData,clinicalFeaturesData,clinicalSurvData, ...){
                         res<-mapper(clinicalFeaturesData)
                         FEA<-t(filterNasFromMatrix(dataMatrix=t(res), filterBy = "columns"))
                         FEA<-scale(FEA)
                         # Model training
                         .self$childclass <- myEnetCoxModel$new()
                         .self$model <- .self$childclass$customTrain(FEA,
                                                                     clinicalSurvData[rownames(FEA),],
                                                                     alpha = alphas, 
                                                                     lambda = lambdas,
                                                                     nfolds =5)                                                
                         
                       },
                       customPredict = function(exprData, copyData, clinicalFeaturesData, ...){
                         res<-mapper(clinicalFeaturesData)
                         beta <- rownames(.self$childclass$getCoefficients())
                         FEA<-res[,beta]
                         FEA<-scale(FEA)
                         
                         predictedResponse <- predict(.self$childclass$model,FEA)
                         names(predictedResponse)<-rownames(FEA)
                         return(predictedResponse)
                       }
                     )
)
