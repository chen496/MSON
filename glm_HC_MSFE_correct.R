### clear object
args=commandArgs(trailingOnly=TRUE)
library(ISLR)
library(caret)
### loading data

HC_DWT_path<-"HC_split/HC_DWT/"
MSFE_DWT_path<-"MSFE_split/MSFE_DWT/"
library(glmnet)
#### nested CV
#### refer to the paper: Bias in Error Estimation When Using Cross-Validation for Model Selection

feature<-c("a",paste0("h",7:1),paste0("v",7:1),paste0("d",7:1))

Vname<-"bior1.5"
vname<-Vname

HC_DWT_engy_file<-paste0(HC_DWT_path,vname,"_HC.csv")
HC_DWT_engy<-read.csv(HC_DWT_engy_file,header = FALSE)
dim(HC_DWT_engy)
HC_DWT_engy[1,1]



MSFE_DWT_engy_file<-paste0(MSFE_DWT_path,vname,"_MSFE.csv")
MSFE_DWT_engy<-read.csv(MSFE_DWT_engy_file,header = FALSE)
dim(MSFE_DWT_engy)
MSFE_DWT_engy[1,1]



dim(MSFE_DWT_engy)


A_engy<-HC_DWT_engy
dim(A_engy)
A_engy[1,1]

B_engy<-MSFE_DWT_engy
dim(B_engy)

B_engy[1,1]

nfolds<-10
nrounds<-1
nfeatures<-ncol(HC_DWT_engy)
data0<-A_engy
data1<-B_engy
nsample<-nrow(data0)+nrow(data1)

X<-rbind(data0,data1)
X<-matrix(unlist(X),nrow=nrow(X),ncol=ncol(X))
data<-X
Y<-c(rep(0,nrow(data0)),rep(1,nrow(data1)))
X<-scale(X,center = TRUE,scale = TRUE)

fit_type.measure<-args[1]
predict_type<-"response"
#### hyperparameter selection -cv
set.seed(123)


SS<-matrix(0,nrounds,ncol=2)
Labels<-matrix(NA,nrounds,nsample)
Pred.Prob<-matrix(0,nrounds,nsample)
TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)
nfolds<-10
SS_Valid<-matrix(0,nrounds,ncol=2)
SS_Valid_pred.Prob<-matrix(0,nrounds,nsample)
SS_Valid_TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)




nrounds<-1
Glm.fit<-list()
Alpha<-seq(0,1,length.out = 50)
test_pred.Prob<-rep(NA,nsample)
test_pred.Class<-rep(NA,nsample)
Valid_pred.Prob<-list()
### leave one out
#foldID<-sample(rep(seq(nfolds), length =nsample-1))
M<-list()
for(sample_i in 1:nrow(X)){
  
  test_X<-X[sample_i,]
  test_X<-matrix(test_X,nrow=1)
  temp_X<-X[-sample_i,]
  temp_Y<-Y[-sample_i]
  
  
  ###  10-fold cv 
  Lambda<-rep(NA,length(Alpha))
  Mse<-rep(NA,length(Alpha))
  
  for(i in 1:length(Alpha)){
    glm.fit<-cv.glmnet(temp_X,temp_Y,nfolds = nrow(temp_X),alignment="fraction",family="binomial",alpha=Alpha[i],
                       lambda.min.ratio=1e-10,type.measure=fit_type.measure)
    Glm.fit[[i]]<-glm.fit
    Mse[i]<-min(glm.fit$cvm)
  }
  
  test_pred.Prob[sample_i]<-predict(Glm.fit[[which.min(Mse)]],s="lambda.min",test_X,type=predict_type)
  test_pred.Class[sample_i]<-predict(Glm.fit[[which.min(Mse)]],s="lambda.min",test_X,type="class")
  glm.fit$lambda.min
  glm.fit$lambda.1se
  ##predict(glm.fit,s="lambda.min",test_X,type="class")
  M[[sample_i]]<-Glm.fit[[which.min(Mse)]]
}

test_pred.Labels<-ifelse(test_pred.Prob >0.5, 1, 0)
TPR<-length(intersect(which(test_pred.Labels==1),which(Y==1)))/length(which(Y==1))
TNR<-length(intersect(which(test_pred.Labels==0),which(Y==0)))/length(which(Y==0))
SS<-c(TPR,TNR)
SS
tp<-length(intersect(which(test_pred.Labels==1),which(Y==1)))
fp<-length(which(test_pred.Labels==1))-length(intersect(which(test_pred.Labels==1),which(Y==1)))
fn<-length(which(test_pred.Labels==0))-length(intersect(which(test_pred.Labels==0),which(Y==0)))
tn<-length(intersect(which(test_pred.Labels==0),which(Y==0)))
TP_FP_FN_TN<-c(tp,fp,fn,tn)


write.csv(test_pred.Prob,file=paste0("glm_HC_MSFE_rounds_",nrounds,"_test_prob_correct_",fit_type.measure,".csv"))
write.csv(test_pred.Class,file=paste0("glm_HC_MSFE_rounds_",nrounds,"_test_class_correct_",fit_type.measure,".csv"))
write.csv(SS,file=paste0("glm_HC_MSFE_rounds_",nrounds,"_test_TPR_TNR_correct_",fit_type.measure,".csv"))
write.csv(TP_FP_FN_TN,file=paste0("glm_HC_MSFE_rounds_",nrounds,"_test_TP_FP_FN_TN_correct_",fit_type.measure,".csv"))
saveRDS(M,file=paste0("glm_HC_MSFE_rounds_",nrounds,"_test_model",fit_type.measure,".rds"))
