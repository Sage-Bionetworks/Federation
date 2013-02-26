M_ExpCNV_topvarying <- setRefClass(Class = "M_ExpCNV_topvarying",                              
                        contains="PredictiveModel",
                        fields=c("model","childclass"),
                        methods = list(
                          initialize = function(...){
                            return(.self)
                          },
                          
                          rawModel = function(){
                            return(.self$model)
                          },
                          
                          customTrain = function(exprData,copyData,clinicalFeaturesData, ...){
                            
                            featureData <-createAggregateFeatureDataSet(list(expr=exprData,copy = copyData))                            
                            
                            controlled<-loadEntity("syn1670947")
                            name<-controlled$objects$topvarying_names
                            name<-setdiff(union(paste(name,"_expr",sep=""),paste(name,"_copy",sep="")),"2099_eg_expr")
                            pos<-match(name,rownames(featureData))
                            POS<-pos[which(is.na(pos)==0)]
                            
                            FEA <-t(featureData[POS,])                                                
                            FEA<-t(filterNasFromMatrix(dataMatrix=t(FEA), filterBy = "columns"))
                            FEA<-scale(FEA)
                            
                            # Model training
                            .self$childclass <- myCatEnetModel$new()
                            .self$model <- .self$childclass$customTrain(FEA,
                                                                        clinicalFeaturesData,
                                                                        alpha = alphas, 
                                                                        lambda = lambdas,
                                                                        nfolds =5)                                                
                            
                          },
                          customPredict = function(exprData, copyData, ...){
                            featureData <-createAggregateFeatureDataSet(list(expr=exprData,copy = copyData))
                            
                            controlled<-loadEntity("syn1670947")
                            name<-controlled$objects$topvarying_names
                            name<-setdiff(union(paste(name,"_expr",sep=""),paste(name,"_copy",sep="")),"2099_eg_expr")
                            pos<-match(name,rownames(featureData))
                            POS<-pos[which(is.na(pos)==0)]
                            
                            FEA <-t(featureData[POS,])               
                            beta <- rownames(.self$childclass$getCoefficients())[-1]
                            FEA<-FEA[,beta]
                            FEA<-scale(FEA)
                            
                            
                            predictedResponse <- predict(.self$childclass$model,FEA)
                            names(predictedResponse)<-rownames(FEA)
                            return(predictedResponse)
                          }
                          )
                        )
