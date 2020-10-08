feature<-c("a",paste0("h",7:1),paste0("v",7:1),paste0("d",7:1))

r<-readRDS("old_mse/glm_HC_MSON_rounds_1_test_modeldeviance.rds")

r[[1]]


T<-matrix(0,length(r),22)
for(i in 1:length(r)){
  print(length(which(as.vector(coef(r[[i]],s="lambda.min")[-1])!=0)))
  T[i,]<-as.vector(coef(r[[i]],s="lambda.min")[-1])
}
coef(r[[i]],s="lambda.min")[-1]

colnames(T)<-feature
colMeans(abs(T))
write.csv(T,file="glm_coef_HC_MSON.csv")
glm_coef<-colMeans(abs(T))
glm_coef[order(glm_coef)[1:7]]<-0

r<-read.csv("logit_HC_MSON_rounds_1_test_coefficients.csv",row.names = 1)
r
r$x
logit_coef<-as.vector(as.numeric(abs(r[1,])))


Rank_coef<-matrix(0,4,ncol(X))
rownames(Rank_coef)<-c("glm_coef_rank","logit_coef_rank","glm_coef_absValue","logit_coef_absValue")
colnames(Rank_coef)<-feature
Rank_coef[1,]<-rank(-abs(glm_coef))
Rank_coef[2,]<-rank(-abs(logit_coef))
Rank_coef[3,]<-round(glm_coef,2)
Rank_coef[4,]<-round(logit_coef,2)
# Rank_coef[3,]<-rank(-abs(colMeans(svm_coef)))
Rank_coef


write.csv(Rank_coef,"coefficent_rank_HC_MSON.csv")




r<-readRDS("old_mse/glm_HC_MSFE_rounds_1_test_modeldeviance.rds")
as.vector(coef(r[[1]]))
r[[1]]
glm_coef<-as.vector(coef(r[[1]],s="lambda.min"))[-1]
T<-matrix(0,length(r),22)
for(i in 1:length(r)){
  print(paste0("sample:",i))
  print(length(which(as.vector(coef(r[[i]],s="lambda.min")[-1])!=0)))
  T[i,]<-as.vector(coef(r[[i]],s="lambda.min")[-1])
}
colnames(T)<-feature
colMeans(T)

T[62,]
write.csv(T,file="glm_coef_HC_MSFE.csv")
glm_coef<-colMeans(abs(T))
glm_coef[order(glm_coef)[1:5]]<-0



r<-read.csv("logit_HC_MSFE_rounds_1_test_coefficients.csv",row.names = 1)
r
r$x
logit_coef<-abs(r[1,])
names(logit_coef)<-feature


Rank_coef<-matrix(0,4,ncol(X))
rownames(Rank_coef)<-c("glm_coef_rank","logit_coef_rank","glm_coef_absValue","logit_coef_absValue")
colnames(Rank_coef)<-feature
Rank_coef[1,]<-rank(-abs(glm_coef))
Rank_coef[2,]<-rank(-abs(logit_coef))
Rank_coef[3,]<-round(glm_coef,2)
Rank_coef[4,]<-round(as.numeric(logit_coef),2)
# Rank_coef[3,]<-rank(-abs(colMeans(svm_coef)))
Rank_coef
write.csv(Rank_coef,"coefficent_rank_HC_MSFE.csv")

















r<-readRDS("old_mse/glm_MSFE_MSON_rounds_1_test_modeldeviance.rds")
T<-matrix(0,length(r),22)
for(i in 1:length(r)){
  print(paste0("sample:",i))
  print(length(which(coef(r[[i]],s="lambda.min")[-1]!=0)))
  T[i,]<-as.vector(coef(r[[i]],s="lambda.min")[-1])
}
colnames(T)<-feature
colMeans(abs(T))


for(i in 1:nrow(T)){
  print(paste0("sample",i,":",length(which(T[i,]!=0))))
}

glm_coef<-abs(T[14,])##colMeans(abs(T))
r<-read.csv("logit_MSFE_MSON_rounds_1_test_coefficients.csv",row.names = 1)
r
r$x
logit_coef<-abs(r[44,])




Rank_coef<-matrix(0,4,ncol(X))
rownames(Rank_coef)<-c("glm_coef_rank","logit_coef_rank","glm_coef_absValue","logit_coef_absValue")
colnames(Rank_coef)<-feature
Rank_coef[1,]<-rank(-abs(glm_coef))
Rank_coef[2,]<-rank(-abs(logit_coef))
Rank_coef[3,]<-round(glm_coef,2)
Rank_coef[4,]<-round(as.numeric(logit_coef),2)
# Rank_coef[3,]<-rank(-abs(colMeans(svm_coef)))
Rank_coef
write.csv(Rank_coef,"coefficent_rank_MSFE_MSON.csv")








temp<-read.csv("glm_coef_HC_MSFE.csv",row.names = 1)
feature_in_C<-c("a",paste0(rep(c("h","v","d"),7),rep(7:1,each=3)))
feature0<-c("a",paste0("h",7:1),paste0("v",7:1),paste0("d",7:1))
colMeans(abs(temp))
feature_in_C[c(1,3,17)]

feature0[-order(colMeans(abs(temp)))[1:5]]

feature0[order(colMeans(abs(temp)))[1:5]]

