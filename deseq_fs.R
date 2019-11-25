##?????????????????????   source("http://bioconductor.org/biocLite.R")

setwd("C:/Users/Administrator/Desktop/rfe")
dat1 <- read.table("HiSeqV2",header=TRUE,row.names="sample",sep="\t")

## batch effect
source("http://www.bioconductor.org/biocLite.R")

biocLite("sva")
library(sva)
colname1 <- colnames(dat1)

pheno = pData(dat1)

edata = exprs(bladderEset)

batch = pheno$batch

modcombat = model.matrix(~1, data=pheno)

combat_edata = ComBat(dat=edata, batch=batch, mod=modcombat, par.prior=TRUE, prior.plot=FALSE)





dat2 <- read.table("DEG.csv",header=TRUE,sep=",")
dat3 <- dat2[,1]
install.packages('stringr')
library(stringr)
dat4 <- str_replace_all(dat3, "[|]", ",")
dat5 <-str_split_fixed(dat4,",",2)
dat5 <- dat5[,1]
dat6 <- dat1[c(dat5),]
dat61<-na.omit(dat6)

dat7 <- read.table("bq.txt",header=TRUE,sep="\t") ##????????????
dat8 <- t(dat61)
dat9 <- cbind(dat7,dat8)  ##???????????????,?????????????????????(??????)
rm(dat3)
rm(dat4)
rm(dat5)
rm(dat6)
rm(dat61)
rm(dat8)


v1<- sample(2:291,28)
nwdat<-dat9[,v1]
nwdat<-cbind(dat7,nwdat)
write.csv(nwdat,file= "nwdat.csv")




set.seed(7) 
library(mlbench)
library(caret)
install.packages("ggplot2")
install.packages("caret")
###source("http://bioconductor.org/biocLite.R")
dat11<-dat9[,2:ncol(dat9)]
sample3 <- rownames(coldata_UCEC)

sample12<- str_sub(sample3, 1, 15)
#sample4<- as.factor(sample12) #?????????
#label1<-as.numeric(as.character(sample4))
dataset1 <- dat9[sample12,]

write.csv(dataset1,file= "dataset.csv")

dataset2 <- dataset1[,-1]

correlationMatrix <- cor(dataset2)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.6)
dataset3<- dataset2[,-c(highlyCorrelated)]  ##去冗余（自变量）




library("caret")
fac <- factor(dataset1[,1],levels=c(0,1))

######randomforest-rfe

control <- rfeControl(functions=rfFuncs, method="cv", verbose = FALSE, returnResamp = "final")
Profile = rfe(dataset3, fac, sizes = c(1:1062), rfeControl = control)

feature_sele <- Profile$optVariables

g <- ggplot(Profile,type = c("o","g"))

p1 <- ggplot(Profile,type = c("o","g"))+ coord_cartesian(xlim = c(0,100))

p1 <- p1+geom_point(aes(20,0.99,col="red", size=2) )
g1<-p1+theme_bw()
write.csv(feature_sele,file= "rf-rfe.csv")

#####NB-rfe
control1 <- rfeControl(functions=nbFuncs, method="cv", verbose = FALSE, returnResamp = "final")
Profile1 = rfe(dataset3, fac, sizes = c(1:1062), rfeControl = control1)

feature_sele <- Profile1$optVariables

p1 <- ggplot(Profile1,type = c("o","g"))+ coord_cartesian(xlim = c(0,100))

p1 <- p1+geom_point(aes(9,0.98,col="red", size=2) )
g1<-p1+theme_bw()
write.csv(feature_sele,file= "new-nbrfe.csv")
write.csv(genesname,file= "genesname.csv")

write.csv(dataset3,file= "dataset3.csv")


#####nnet-rfe
cran <- getOption("repos")
cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
options(repos = cran)
install.packages("mxnet") 

dataset3 <- read.table("dataset3.csv",header=TRUE,sep=",")
dataset3 <- dataset3[,-1]
rank1 <- sample(1:100,100)
newdataset3 <- dataset3[rank1,]
factor1<- str_sub(dataset3[,1], 14, 14)
fac <- factor(factor1,levels=c(0,1))

trControl <- trainControl(method = "cv", number = 10)

# parameters for the RFE function
Control <- rfeControl(functions = caretFuncs, method = "cv",
                      verbose = FALSE , returnResamp = "final")

rf1 <- rfe(newdataset3,fac1 ,sizes = c( 1:1062), rfeControl = Control, trControl = trControl, method = "mxnet")


#####svm-rfe

dat7<- str_replace_all(fac, "[0]", "Tumor")
dat7<- str_replace_all(dat7, "[1]", "Normal")
fac1 <- factor(dat7,levels=c('Tumor','Normal'))


library(kernlab)
trControl <- trainControl(method = "cv", number = 10)

# parameters for the RFE function
Control <- rfeControl(functions = caretFuncs, method = "cv",
                         verbose = FALSE , returnResamp = "final")

rf1 <- rfe(dataset3,fac1 ,sizes = c( 1:1062), rfeControl = Control, trControl = trControl, method = "svmLinear")


feature_sele <-rf1$optVariables

write.csv(feature_sele,file= "new-svmrfe.csv")

p1 <- ggplot(rf1,type = c("o","g"))+ coord_cartesian(xlim = c(0,200))

p1 <- p1+geom_point(aes(114,0.99,col="red", size=2) )
g1<-p1+theme_bw()



###########  elm-rfe
source("https://bioconductor.org/biocLite.R")

trControl<- trainControl(method = "cv", number = 10)

# parameters for the RFE function
Control <- rfeControl(functions = caretFuncs, method = "cv",
                      verbose = FALSE , returnResamp = "final")

#############  nnet-rfe
CVfolds <- 10
tuneLength <-10
seedNum <- CVfolds + 1
seedLen <- CVfolds + tuneLength

set.seed(123)
seeds <- vector(mode = "list", length = seedNum)
for(i in 1:(seedNum-1)) seeds[[i]] <- sample.int(1000, seedLen)  

seeds[[seedNum]] <- sample.int(1000, 1)
trControl1 <- trainControl(
  method = "cv",
  number = CVfolds,
  classProbs=TRUE,
  summaryFunction = twoClassSummary,
  seeds = seeds
)


rf1 <- rfe(newdataset3,fac1 ,sizes = c( 1:1062), rfeControl = Control, trControl = trControl1, method = "nnet", MaxNWts = 10000)


feature_sele <-rf1$optVariables

write.csv(feature_sele,file= "new-svmrfe.csv")

p1 <- ggplot(rf1,type = c("o","g"))+ coord_cartesian(xlim = c(0,200))

p1 <- p1+geom_point(aes(114,0.99,col="red", size=2) )
g1<-p1+theme_bw()


####### DNN-rfe

Control <- rfeControl(functions = caretFuncs, method = "cv",
                      verbose = FALSE , returnResamp = "final")

trControl2<- trainControl(method = "cv", number = 10)
rf1 <- rfe(newdataset3,fac1 ,sizes = c( 1:1062), rfeControl = Control, trControl = trControl2, method = "dnn")











































































































