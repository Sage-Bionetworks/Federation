M_ExpCNV_marginalAssociation <- setRefClass(Class = "M_ExpCNV_marginalAssociation",                              
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
                            
                            controlled<-loadEntity("syn1670947")
                            name<-controlled$objects$marginal_association_names
                            name<-union(paste(name,"_expr",sep=""),paste(name,"_copy",sep=""))                            
                            pos<-match(name,rownames(featureData))
                            POS<-pos[which(is.na(pos)==0)]
                            
                            FEA <-t(featureData[POS,])                                                
                            FEA<-t(filterNasFromMatrix(dataMatrix=t(FEA), filterBy = "columns"))
                            FEA<-scale(FEA)
                            
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
                            
                            controlled<-loadEntity("syn1670947")
                            name<-controlled$objects$marginal_association_names
                            name<-union(paste(name,"_expr",sep=""),paste(name,"_copy",sep=""))                            
                            pos<-match(name,rownames(featureData))
                            POS<-pos[which(is.na(pos)==0)]
                            
                            FEA <-t(featureData[POS,])               
                            beta <- rownames(.self$childclass$getCoefficients())
                            FEA<-FEA[,beta]
                            FEA<-scale(FEA)
                            
                            
                            predictedResponse <- predict(.self$childclass$model,FEA)
                            names(predictedResponse)<-rownames(FEA)
                            return(predictedResponse)
                          }
                          )
                        )
