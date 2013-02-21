C_GII <- setRefClass(Class = "C_GII",                              
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
                         
                         instab = unlist(apply(t(exprs(copyData)), 1, function(x){return(sum(x>1,na.rm = TRUE)+sum(x< -1, na.rm = TRUE))})) 
                         
                         FEA <- cbind(res,instab[rownames(res)])                                     
                         colnames(FEA)<-c(colnames(res),"GII")
                         FEA<-scale(FEA)
                         FEA<-t(filterNasFromMatrix(dataMatrix=t(FEA), filterBy = "columns"))
                         
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
                         instab = unlist(apply(t(exprs(copyData)), 1, function(x){return(sum(x>1,na.rm = TRUE)+sum(x< -1, na.rm = TRUE))})) 
                         
                         FEA <- cbind(res,instab[rownames(res)])                                     
                         colnames(FEA)<-c(colnames(res),"GII")
                         FEA<-scale(FEA)
                         beta <- rownames(.self$childclass$getCoefficients())
                         FEA<-FEA[,beta]
                         
                         
                         predictedResponse <- predict(.self$childclass$model,FEA)
                         names(predictedResponse)<-rownames(FEA)
                         return(predictedResponse)
                       }
                     )
)
