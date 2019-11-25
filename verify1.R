###
setwd("C:/Users/Administrator/Desktop/rfe/verify")
rf <- read.table("rf-rfe1.csv",header=TRUE,sep=",",row.names = 1)
dat2 <- read.table("caret_data.csv",header=TRUE,sep=",",row.names = 1)
svm_imp <- read.table("svm-imp.csv",header=TRUE,sep=",",row.names = 1)
rf_feature <- read.table("rf-variables.csv",header=TRUE,sep=",",row.names = 1)
rf_feature1<- rf_feature$Variables==100
rf_feature2<- rf_feature$var[rf_feature1]

rf_feature2 <- as.matrix(rf_feature2)
rf_feature2 <- rf_feature2[!duplicated(rf_feature2)]
rf_feature2 <- as.matrix(rf_feature2)


####AIC
dat_rf <- dat2[,c(as.matrix(svm_imp)) ]

fac <- dat2[,1]

svm_aic <- matrix(data = NA,nrow = 110,ncol = 2)
svm_aic[,1]<- c(1:110)
for (k in 110:1)
{
rf_glm <- glm(fac~.,dat_rf[,1:k],family = binomial(link ="logit"))
svm_aic[k,2]<- AIC(rf_glm)
}

gg1 <- plot(rf_aic[,1],rf_aic[,2],type="o")

write.csv(svm_aic,file= "svm_aic.csv")
##
nnet_imp <- read.table("nnet-imp.csv",header=TRUE,sep=",",row.names = 1)
nnet_feature <- read.table("nnet-rfe1.csv",header=TRUE,sep=",",row.names = 1)


nnet_imp1 <- nnet_imp[-c(unlist(nnet_feature)),1]
nnet_imp1 <- as.matrix(nnet_imp1)
nnet_imp2 <- rbind(as.matrix(unlist(nnet_feature)) ,nnet_imp1)
dat_rf <- dat2[,c(as.matrix(nnet_imp2)) ]

fac <- dat2[,1]

nnet_aic <- matrix(data = NA,nrow = 110,ncol = 2)
nnet_aic[,1]<- c(1:110)
for (k in 110:1)
{
  rf_glm <- glm(fac~.,dat_rf[,1:k],family = binomial(link ="logit"))
  nnet_aic[k,2]<- AIC(rf_glm)
}

gg1 <- plot(rf_aic[,1],rf_aic[,2],type="o")

write.csv(nnet_aic,file= "nnet_aic.csv")
write.csv(nnet_imp2,file= "nnet_imp.csv")



###random select
dat1 <- read.table("deseq_nor.csv",header=TRUE,sep=",")
row.names(dat1)<- dat1[,1]
dat1 <- dat1[,-1]
dat4 <- read.table("DEG_treat_vs_control1.csv",header=TRUE,sep=",")
dat3 <- dat4[,1]
randomdata <- dat1[-which(row.names(dat1)%in%c(as.matrix(dat3) )),]
randomdata <- t(randomdata)

label1<- matrix(data=1,nrow = 100,ncol = 1)
label1[51:100,1]<- 0
randomdata1 <- cbind(label1,randomdata)
seeds <- sample(1:100,100)
randomdata1 <- randomdata1[seeds,]
seeds2<- matrix(data = NA,nrow = 105,ncol = 30)
seeds2[,1:10]<-seeds1 
fac<- randomdata1[,1]
randomAIC1<-matrix(data=NA,nrow = 105,ncol = 30)
for (s in 11:30){
seeds <- sample(2:18409,105)
seeds2[,s]<-seeds
}


k<-30
randomdata <- data.frame(randomdata1[,c(seeds2[,k])]) 


  for (i in 105:1){
  random_glm <- glm(fac~.,randomdata[,1:i],family = binomial(link ="logit"))
  randomAIC1[i,k] <-AIC(random_glm)}

write.csv(randomAIC1,file= "randomAIC1.csv") 
  
  
  
  
  
#################  
gg1 <- plot(random_aic[,1],random_aic[,2],type="o")

write.csv(random_aic,file= "random_aic.csv")

seeds2<- matrix(data = NA,nrow = 105,ncol = 30)
seeds1[,10] <- seeds

seeds <- seeds1[1:50,8]

seeds <- c(seeds1[1:50,9],seeds1[1:55,8])

###############
library(MASS)
dat_rf <- dat2[,c(as.matrix(rf_imp)) ]

fac <- dat2[,1]


  rf_glm <- glm(fac~.,dat_rf[,1:100],family = binomial(link ="logit"))
  stpaic <- stepAIC(rf_glm,direction = "backward")

  stepaic1 <- stpaic$anova$AIC
  write.csv(stepaic1,file= "stepaic1.csv")
  aaaaa<-stpaic$coefficients
  
  
##???????????????
  aaa=phyper(4-1, 57, 829, 4, lower.tail=F)
  
####randomaic30_mean
  xl <- matrix(data=1:105,nrow = 105,ncol = 1)
  plot(xl,randomAIC1[,2],xlim=c(0,105),ylim=c(-10,700)
       ,ylab="AIC Values",xlab = "The Number of Variables" ,type = "l" )
  
  for(s in 2:30){
      lines(randomAIC1[,s],lty=1)
  }

  polygon(xl,randomAIC1[,2],randomAIC1[,3],randomAIC1[,4],
          randomAIC1[,5],randomAIC1[,6],randomAIC1[,7],col = 'yellow')
  
  
  
  
  
  
  
  
  
  