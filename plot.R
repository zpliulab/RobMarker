setwd("C:/Users/Administrator/Desktop/rfe/aicplot")



rf <- read.table("rf_aic.csv",header=TRUE,sep=",",row.names = 1)
nnet <- read.table("nnet_aic.csv",header=TRUE,sep=",",row.names = 1)
svm <- read.table("svm_aic.csv",header=TRUE,sep=",",row.names = 1)
random<- read.table("random_aic.csv",header=TRUE,sep=",",row.names = 1)








g2 <- plot(random[,1],pch=16,random[,2],xlim=c(0,105),ylim=c(0,500),col="palegreen3"
           ,ylab="AIC Values",xlab = "The Number of Variables" ,cex=1)
lines(random[,2],col="palegreen3",lty=2)

arrows(x0 = 40, y0 = 144, x1 = 75, y1 = 214, code = 1,col="gray3")
legend(79,480,c("Random-AIC","Changes in variables"),col=c("palegreen3","gray3"),
            text.col=c("palegreen3","gray3"),
            pch=c(16,4),lty=c(2,1),cex=0.9)





g1<- plot(svm[,1],svm[,2],pch=15,xlim=c(0,105),ylim=c(-10,200),col="red"
          ,ylab="AIC Values",xlab = "The Number of Variables" ,cex=1 )##adaboost


#text(8,18,paste("AIC min"),pos=1,cex=0.8)
points(svm[,1],svm[,2],pch=15,col="steelblue",cex=1)##knn
points(svm[,1],svm[,2],pch=15,col="orange2",cex=1)##nb
points(nnet[,1],nnet[,2],pch=17,col="green4",cex=1.2)##nnet
points(rf[,1],rf[,2],pch=18,col="deeppink" ,cex=1.3 )##rf
points(svm[,1],svm[,2],pch=15,col="tomato2",cex=1)##svm


lines(svm[,2],col="red",lty=6)##adaboost
lines(svm[,2],col="steelblue",lty=6)##knn
lines(svm[,2],col="orange2",lty=6)##nb
lines(nnet[,2],col="green4",lty=6)#nnet
lines(rf[,2],col="DeepPink",lty=6)#rf
lines(svm[,2],col="tomato2",lty=6)#svm


points(21,44,pch=15,col="red" ,cex=1.6)##adb
 points(62,126,pch=15,col="steelblue"  ,cex=1.6)##knn
  points(12,26,pch=15,col="orange2",cex=1.6)##nb
  points(63,128,pch=17,col="green4",cex=2)##nnet
points(13,28,pch=18,col="DeepPink",cex=2)##rf
points(57,116,pch=15,col="tomato2",cex=1.6)##svm


#points(8,18,pch=15,col="orange1",cex=1.3)
#points(5,12,pch=18,col="orange2",cex=1.5)
#points(7,16,pch=17,col="orange3",cex=1.4)
 
text(5,12,paste("AIC min"),pos=1,cex=0.8)

text(63,128,paste("NN"),pos=4,cex=0.9)
 text(13,28,paste("RF"),pos=4,cex=0.9)
# text(7,16,paste("AIC min"),pos=3,cex=0.8)
 text(57,116,paste("SVM"),pos=1,cex=0.9)
 text(12,26,paste("NB"),pos=1,cex=0.9)
 text(62,126,paste("KNN"),pos=1,cex=0.9)
  text(21,44,paste("Adaboost"),pos=4,cex=0.9)
 
legend(88,52,c("Adaboost-AIC","KNN-AIC","NB-AIC","NN-AIC","RF-AIC","SVM-AIC"),
       col=c("red","steelblue","orange2","green4","DeepPink","tomato2"),
       text.col=  c("red","steelblue","orange2","green4","DeepPink","tomato2"), 
       pch=c(15,15,15,17,18,15),
       lty=c(6,6,6,6,6,6),
       cex = 1,ncol=1,x.intersp=0.5)

legend(2,190,c("Model of Adaboost","Model of KNN","Model of NB","Model of NN","Model of RF","Model ofSVM"),
           col=c("red","steelblue","orange2","green4","DeepPink","tomato2"),
text.col=  c("red","steelblue","orange2","green4","DeepPink","tomato2"),  
 pch=c(15,15,15,17,18,15),
lty=c(0,0,0,0,0,0),
cex = 1,x.intersp=0.5, ncol=2)

points(c(40,50,60),c(114,134,154), type = "n", main = "code = 1",col="gray3")

arrows(x0 = 40, y0 = 34, x1 = 70, y1 = 94, code = 1,col="gray3")
