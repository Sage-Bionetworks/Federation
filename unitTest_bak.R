source('http://depot.sagebase.org/CRAN.R')
pkgInstall(c("BCC","predictiveModeling"))


###################################################
### step 1: loadLibraries
###################################################
library(predictiveModeling)
library(BCC)
library(survival)
library(survcomp)
library(MASS)


###################################################ac
### step 2: load trainingData
###################################################
exp <-loadEntity("syn1588845")
Exp<-exp$objects$exprData_train

copy <-loadEntity("syn1589116")
Copy <-copy$objects$copyData_train

clinic <-loadEntity("syn1589898")
Clinic <-clinic$objects$clinicalFeatures_train
covariates<-pData(Clinic)

survivalData<-loadEntity("syn1589146")
SurvivalData<-survivalData$objects$clinicalSurv_train




###################################################ac
### step 3: Micma testData
###################################################
exp <-loadEntity("syn1589708")
Exp1<-exp$objects$exprData_micma

copy <-loadEntity("syn1588686")
Copy1 <-copy$objects$copyData_micma

clinic <-loadEntity("syn1589906")
Clinic1 <-clinic$objects$clinicalFeatures_micma
covariates1<-pData(Clinic1)


###################################################ac
### step 4: Metabric testData1
###################################################
exp <-loadEntity("syn1589199")
Exp2<-exp$objects$exprData_validation1

copy <-loadEntity("syn1589221")
Copy2 <-copy$objects$copyData_validation1

clinic <-loadEntity("syn1589902")
Clinic2 <-clinic$objects$clinicalFeatures_validation1
covariates2<-pData(Clinic2)


###################################################ac
### step 5: Metabric testData2
###################################################
exp <-loadEntity("syn1589260")
Exp3<-exp$objects$exprData_validation2

copy <-loadEntity("syn1589454")
Copy3 <-copy$objects$copyData_validation2

clinic <-loadEntity("syn1589904")
Clinic3 <-clinic$objects$clinicalFeatures_validation2
covariates3<-pData(Clinic3)


# Clinical Covariates intersection between Training and Testing(MICMA)
A<-covariates[,intersect(names(covariates),names(covariates1))]
Atest<-covariates1[,intersect(names(covariates),names(covariates1))]
AA<-A[,c(7,8)]
AAtest<-Atest[,c(7,8)]
# Clinical Covariates intersection between Training and Testing(Metabric1)

B<-covariates[,intersect(names(covariates),names(covariates2))]
Btest<-covariates2[,intersect(names(covariates),names(covariates2))]

BB<-B[,c(2,4,5)]
BBtest<-Btest[,c(2,4,5)]

# Clinical Covariates intersection between Training and Testing(Metabric2)
C<-covariates[,intersect(names(covariates),names(covariates3))]
Ctest<-covariates3[,intersect(names(covariates),names(covariates3))]

CC<-C[,c(2,4,5)]
CCtest<-Ctest[,c(2,4,5)]


# only difference between previous Federation model submission was "Clinical Variates"
# Coxph model is used for this unit test with new validation dataset
source("~/Federation/CoxphModel.R")

#training
coxTest<-CoxphModel$new()
coxTest$customTrain(exprs(Exp),exprs(Copy),AA,SurvivalData)

# validation with MICMA
P1<-coxTest$customPredict(exprs(Exp1),exprs(Copy1),AAtest)


#training
coxTest<-CoxphModel$new()
coxTest$customTrain(exprs(Exp),exprs(Copy),BB,SurvivalData)

# validation with Metabric1
P2<-coxTest$customPredict(exprs(Exp2),exprs(Copy2),BBtest)


#training
coxTest<-CoxphModel$new()
coxTest$customTrain(exprs(Exp),exprs(Copy),CC,SurvivalData)

# validation with Metabric2
P3<-coxTest$customPredict(exprs(Exp3),exprs(Copy3),CCtest)


