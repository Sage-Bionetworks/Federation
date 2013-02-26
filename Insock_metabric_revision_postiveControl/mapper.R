mapper<-function(DATAFRAME){
  
  res<-matrix(NA,ncol = ncol(DATAFRAME),nrow = nrow(DATAFRAME))
  rownames(res)<-rownames(DATAFRAME)
  colnames(res)<-colnames(DATAFRAME)[1:ncol(DATAFRAME)]
  
  for(k in 1:ncol(DATAFRAME)){
    if(is.numeric(DATAFRAME[,k])){res[,k]<-DATAFRAME[,k]}
    if(is.factor(DATAFRAME[,k])){
      aa<-levels(DATAFRAME[,k])
      bb<-seq(1,length(aa))
      for(kk in 1:length(aa)){
        res[which(DATAFRAME[,k]==aa[kk]),k]<-bb[kk]
      }
    }
  }
  return(res)
}