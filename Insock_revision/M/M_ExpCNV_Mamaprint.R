M_ExpCNV_Mamaprint <- setRefClass(Class = "M_ExpCNV_Mamaprint",                              
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
                            
                            featureData <-createAggregateFeatureDataSet(list(expr=exprData,copy = copyData))
                            
                            controlled<-loadEntity("syn1671415")
                            name<-controlled$objects$Mamaprint
                            name<-paste(name,"_expr",sep="")
                            pos<-match(name,rownames(featureData))
                            POS<-pos[which(is.na(pos)==0)]
                            
                            FEA <-t(featureData[POS,])                                                
                            FEA<-t(filterNasFromMatrix(dataMatrix=t(FEA), filterBy = "columns"))
                            
                            # Model training
                            .self$childclass <- myEnetCoxModel$new()
                            .self$model <- .self$childclass$customTrain(FEA,
                                                                        clinicalSurvData,
                                                                        alpha = alphas, 
                                                                        lambda = lambdas,
                                                                        nfolds =5)                                                
                            
                          },
                          customPredict = function(exprData, copyData, clinicalFeaturesData, ...){
                            featureData <-createAggregateFeatureDataSet(list(expr=exprData,copy = copyData))
                            
                            controlled<-loadEntity("syn1671415")
                            name<-controlled$objects$Mamaprint
                            name<-paste(name,"_expr",sep="")
                            pos<-match(name,rownames(featureData))
                            POS<-pos[which(is.na(pos)==0)]
                            
                            FEA <-t(featureData[POS,])               
                            beta <- rownames(.self$childclass$getCoefficients())
                            FEA<-FEA[,beta]
                            
                            
                            predictedResponse <- predict(.self$childclass$model,FEA)
                            names(predictedResponse)<-rownames(FEA)
                            return(predictedResponse)
                          }
                          )
                        )
