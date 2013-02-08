MC_ExpCNV_maspGII <- setRefClass(Class = "MC_ExpCNV_maspGII",                              
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
                                  
                                  controlled<-loadEntity("syn1670951")
                                  name<-controlled$objects$masp_names
                                  name<-paste(name,"_expr",sep="")
                                  pos<-match(name,rownames(featureData))
                                  POS<-pos[which(is.na(pos)==0)]
                                  
                                  instab = unlist(apply(t(exprs(copyData)), 1, function(x){return(sum(x>1,na.rm = TRUE)+sum(x< -1, na.rm = TRUE))})) 
                                  
                                  FEA1 <-t(featureData[POS,])                                        
                                  FEA <- cbind(FEA1,res[rownames(FEA1),],instab[rownames(FEA1)]) 
                                  colnames(FEA)<-c(colnames(FEA1),colnames(res),"GII")
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
                                  
                                  controlled<-loadEntity("syn1670951")
                                  name<-controlled$objects$masp_names
                                  name<-paste(name,"_expr",sep="")                            
                                  pos<-match(name,rownames(featureData))
                                  POS<-pos[which(is.na(pos)==0)]
                                  
                                  instab = unlist(apply(t(exprs(copyData)), 1, function(x){return(sum(x>1,na.rm = TRUE)+sum(x< -1, na.rm = TRUE))})) 
                                  
                                  FEA1 <-t(featureData[POS,])                                        
                                  FEA <- cbind(FEA1,res[rownames(FEA1),],instab[rownames(FEA1)])                                     
                                  colnames(FEA)<-c(colnames(FEA1),colnames(res),"GII")
                                  beta <- rownames(.self$childclass$getCoefficients())
                                  FEA<-FEA[,beta]
                                  FEA<-scale(FEA)
                                  
                                  
                                  predictedResponse <- predict(.self$childclass$model,FEA)
                                  names(predictedResponse)<-rownames(FEA)
                                  return(predictedResponse)
                                }
                              )
)
