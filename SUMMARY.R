setwd("C:/Users/Administrator/Desktop/rfe/JUNE")

AIC_sele <- read.table("AIC_sele.csv",header=TRUE,sep=",",row.names = 1)


BIC_sele <- read.table("BIC_sele.csv",header=TRUE,sep=",",row.names = 1)


CP_sele <- read.table("CP_sele.csv",header=TRUE,sep=",",row.names = 1)


adaboost <- read.table("adaboost-rfe1.csv",header=TRUE,sep=",",row.names = 1)
knn <- read.table("knn-rfe1.csv",header=TRUE,sep=",",row.names = 1)
nb <- read.table("nb-rfe1.csv",header=TRUE,sep=",",row.names = 1)
nnet <- read.table("nnet-rfe1.csv",header=TRUE,sep=",",row.names = 1)
rf <- read.table("rf-rfe1.csv",header=TRUE,sep=",",row.names = 1)
svm <- read.table("svm-rfe1.csv",header=TRUE,sep=",",row.names = 1)

a_k <- merge(adaboost, knn)
a_nb <- merge(adaboost, nb)
a_nnet <- merge(adaboost, nnet)
a_rf <- merge(adaboost, rf)
a_svm <- merge(adaboost, svm)


k_nb <- merge(knn, nb)
k_nnet <- merge(knn, nnet)
k_rf <- merge(knn, rf)
k_svm <- merge(knn, svm)

nb_nnet <-merge(nb, nnet) 
nb_rf <- merge(nb, rf)
nb_svm <- merge(nb, svm)

nnet_rf <- merge(nnet, rf)
nnet_svm <- merge(svm, nnet)

rf_svm <- merge(rf, svm)

aic_nb <-  merge(AIC_sele, nb)
aic_svm <-  merge(AIC_sele, svm)
bic_nb <-  merge(BIC_sele, nb)
bic_svm <-  merge(BIC_sele, svm)
cp_nb <-  merge(CP_sele, nb)
cp_bic <-  merge(CP_sele, BIC_sele)

ad_nb_kn_svm <- merge(merge(a_nb,knn),svm)
allin <- merge(merge(ad_nb_kn_svm,rf),nnet)
ad_nb_kn_svm_rf <- merge(merge(merge(a_nb,knn),svm),rf)

bj <- union(union(union(unlist(adaboost),unlist(knn)),unlist(svm)),union(union(unlist(nb),unlist(nnet)),unlist(rf)))

bj1<-as.matrix(bj)
write.csv(bj1,file= "all_selected.csv")


###go
all_selected <- read.table("all_selected.csv",header=TRUE,sep=",",row.names = 1)
source("http://www.bioconductor.org/biocLite.R") 
biocLite("clusterProfiler")

library(clusterProfiler)


setwd("C:/Users/Administrator/Desktop/go")
d1= read.table("1.txt")
d11=t(d1)
eg <- bitr(all_selected[,1], fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
go1 <- enrichGO(eg$ENTREZID, 
                OrgDb = org.Hs.eg.db,
                keyType = 'ENTREZID',
                ont="BP",
                pAdjustMethod = 'BH',
                pvalueCutoff = 0.05, 
                qvalueCutoff = 0.05,
                readable= TRUE)
write.table(go1@result,"go_result.csv",sep=',')

d12= read.table("12.txt")
d102=t(d12)
eg12 <- bitr(d102, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db");
go12<- enrichGO(eg12$ENTREZID, 
                OrgDb = org.Hs.eg.db,
                keyType = 'ENTREZID',
                ont="BP",
                pAdjustMethod = 'BH',
                pvalueCutoff = 0.05, 
                qvalueCutoff = 0.05,
                readable= TRUE)
write.table(go12@result,"12.csv",sep=',')
