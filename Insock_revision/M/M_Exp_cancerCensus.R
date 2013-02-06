source("~/DrugResponse/R5/myEnetCoxModel.R")

M_Exp_cancerCensus <- setRefClass(Class = "M_Exp_cancerCensus",                              
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
                         featureData <-createAggregateFeatureDataSet(list(expr=exprData))
                         featureData_filtered <- filterNasFromMatrix(dataMatrix=featureData, filterBy = "rows")
                         featureData <- unique(featureData_filtered)
                         
                         controlled<-loadEntity("syn1670947")
                         name<-controlled$objects$cancer_census_names
                         name<-paste(name,"_expr",sep="")                                                  
                         pos<-match(name,rownames(featureData))
                         POS<-pos[which(is.na(pos)==0)]
                         
                         FEA <-t(featureData[POS,])                                                
                         
                         # Model training
                         .self$childclass <- myEnetCoxModel$new()
                         .self$model <- .self$childclass$customTrain(FEA,
                                                                     clinicalSurvData,
                                                                     alpha = alphas, 
                                                                     lambda = lambdas,
                                                                     nfolds =5)                                                
                         
                       },
                       customPredict = function(exprData, copyData, clinicalFeaturesData, ...){
                         featureData <-createAggregateFeatureDataSet(list(expr=exprData))
                         
                         controlled<-loadEntity("syn1670947")
                         name<-controlled$objects$cancer_census_names
                         name<-paste(name,"_expr",sep="")                                                  
                         pos<-match(name,rownames(featureData))
                         POS<-pos[which(is.na(pos)==0)]
                         
                         FEA <-t(featureData[POS,])                                                                                  
                         
                         predictedResponse <- predict(.self$childclass$model,FEA)
                         return(predictedResponse)
                       }
                       )
                     )
