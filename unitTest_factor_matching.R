require(synapseClient)

# only difference between previous Federation model submission was "Clinical Variates"
# Coxph model is used for this unit test with new validation dataset
source("~/Federation/CoxphModel.R")

library(predictiveModeling)
library(BCC)
library(survival)
library(survcomp)
library(MASS)


###################################################ac
### step 1: load trainingData : for Expression, Copy Number and Survival 
###################################################
exp <-loadEntity("syn1588845")
Exp<-exp$objects$exprData_train

copy <-loadEntity("syn1589116")
Copy <-copy$objects$copyData_train

survivalData<-loadEntity("syn1589146")
SurvivalData<-survivalData$objects$clinicalSurv_train




###################################################ac
### step 2: Micma testData for Expression, Copy Number
###################################################
exp <-loadEntity("syn1589708")
Exp1<-exp$objects$exprData_micma

copy <-loadEntity("syn1588686")
Copy1 <-copy$objects$copyData_micma


###################################################ac
### step 3: Metabric testData 1 for Expression, Copy Number
###################################################
exp <-loadEntity("syn1589199")
Exp2<-exp$objects$exprData_validation1

copy <-loadEntity("syn1589221")
Copy2 <-copy$objects$copyData_validation1


###################################################ac
### step 4: Metabric testData2 for Expression, Copy Number
###################################################
exp <-loadEntity("syn1589260")
Exp3<-exp$objects$exprData_validation2

copy <-loadEntity("syn1589454")
Copy3 <-copy$objects$copyData_validation2



###################################################
### step 5: Model Runs with Metabric training and MICMA validation
###################################################

# Clinical feature data for training (factor matching with MICMA clinical feature dataset)
cv<-loadEntity("syn1627060")
AA<-cv$objects$AA
# you can choose any clinical features: here I choose 5 covariates from 8(I think one(p53_mutation_detail) is useless)
TRAIN<-AA[,c(1,2,3,5,7)]
# NA filtering process
a<-c()
for(k in 1:ncol(TRAIN)){
  a<-union(a,which(is.na(TRAIN[,k]==1)))
}
# Model Running
coxTest<-CoxphModel$new()
coxTest$customTrain(exprs(Exp),exprs(Copy),TRAIN[-a,],SurvivalData[-a,])

# Clinical feature data for testing (factor matching with Metabric training clinical feature dataset)
cvtest<-loadEntity("syn1627047")
AAtest<-cvtest$objects$AAtest
# the same clinical covariates which are selected in training
TEST<-AAtest[,c(1,2,3,5,7)]
# NA filtering
b<-c()
for(k in 1:ncol(TEST)){
  b<-union(b,which(is.na(TEST[,k]==1)))
}

# Runing validation with MICMA
P1<-coxTest$customPredict(exprs(Exp1),exprs(Copy1),TEST[-b,])



#training and validating METABRIC and METABRIC 1
cv<-loadEntity("syn1627170")
AA<-cv$objects$AA
TRAIN<-AA[,c(1:11)]
a<-c()
for(k in 1:ncol(TRAIN)){
  a<-union(a,which(is.na(TRAIN[,k]==1)))
}
# Model running 
coxTest<-CoxphModel$new()
coxTest$customTrain(exprs(Exp),exprs(Copy),TRAIN[-a,],SurvivalData[-a,])

# validation step
cvtest<-loadEntity("syn1627175")
AAtest<-cvtest$objects$AAtest
TEST<-AAtest[,c(1:11)]
b<-c()
for(k in 1:ncol(TEST)){
  b<-union(b,which(is.na(TEST[,k]==1)))
}

# validation with METABRIC 1
P2<-coxTest$customPredict(exprs(Exp2),exprs(Copy2),TEST[-b,])



#training and validating METABRIC and METABRIC 2
cv<-loadEntity("syn1627185")
AA<-cv$objects$AA
TRAIN<-AA[,c(1,12:14,18,20:21,23:25)]
a<-c()
for(k in 1:ncol(TRAIN)){
  a<-union(a,which(is.na(TRAIN[,k]==1)))
}
#training
coxTest<-CoxphModel$new()
coxTest$customTrain(exprs(Exp),exprs(Copy),TRAIN[-a,],SurvivalData[-a,])

cvtest<-loadEntity("syn1627192")
AAtest<-cvtest$objects$AAtest
TEST<-AAtest[,c(1,12:14,18,20:21,23:25)]

b<-c()
for(k in 1:ncol(TEST)){
  b<-union(b,which(is.na(TEST[,k]==1)))
}

P3<-coxTest$customPredict(exprs(Exp3),exprs(Copy3),TEST[-b,])
