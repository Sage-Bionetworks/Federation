MC_ExpCNV_metabricClustering <- setRefClass(Class = "MC_ExpCNV_metabricClustering",                              
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
                                     
                                     featureData <-createAggregateFeatureDataSet(list(expr=exprData,copy = copyData))                                       
                                     
                                     controlled<-loadEntity("syn1670947")
                                     name<-controlled$objects$metabric_clustering_names
                                     name<-union(paste(name,"_expr",sep=""),paste(name,"_copy",sep=""))                            
                                     pos<-match(name,rownames(featureData))
                                     POS<-pos[which(is.na(pos)==0)]
                                     
                                     FEA1 <-t(featureData[POS,])                                        
                                     FEA <- cbind(FEA1,res[rownames(FEA1),])                                     
                                     FEA<-t(filterNasFromMatrix(dataMatrix=t(FEA), filterBy = "columns"))
                                     penalty <- rep(0,ncol(FEA))
                                     penalty[grep("_eg_",colnames(FEA))]<-1
                                     FEA<-scale(FEA)
                                     
                                     # Model training
                                     .self$childclass <- myEnetCoxModel$new()
                                     .self$model <- .self$childclass$customTrain(FEA,
                                                                                 clinicalSurvData[rownames(FEA),],
                                                                                 alpha = alphas, 
                                                                                 lambda = lambdas,
                                                                                 nfolds =5,
                                                                                 penalty.factor = penalty)
                                   },
                                   customPredict = function(exprData, copyData, clinicalFeaturesData, ...){
                                     res<-mapper(clinicalFeaturesData)
                                     
                                     featureData <-createAggregateFeatureDataSet(list(expr=exprData,copy = copyData))                                       
                                     
                                     controlled<-loadEntity("syn1670947")
                                     name<-controlled$objects$metabric_clustering_names
                                     name<-union(paste(name,"_expr",sep=""),paste(name,"_copy",sep=""))                            
                                     pos<-match(name,rownames(featureData))
                                     POS<-pos[which(is.na(pos)==0)]
                                     
                                     FEA1 <-t(featureData[POS,])                                        
                                     FEA <- cbind(FEA1,res[rownames(FEA1),])                                     
                                     beta <- rownames(.self$childclass$getCoefficients())
                                     FEA<-FEA[,beta]
                                     FEA<-scale(FEA)
                                     
                                     
                                     predictedResponse <- predict(.self$childclass$model,FEA)
                                     names(predictedResponse)<-rownames(FEA)
                                     return(predictedResponse)
                                   }
                                 )
)
