### clear object
args=commandArgs(trailingOnly=TRUE)
library(ISLR)
library(caret)
### loading data
library(PRROC)
interpolate<-function(Pos_class,Neg_class,c){
  
  x <- Pos_class
  y <- Neg_class
  par(mfrow=c(1,2))
  # compute area under ROC curve for the hard-labeled case
  roc <- roc.curve(x, y,curve = TRUE );
  
  xa<-roc$curve[,1]
  yb<-roc$curve[,2]
  
  
  K<-vector()
  for(i in 1:(length(xa)-1)){
    xa_1<-xa[i]
    xa_2<-xa[i+1]
    yb_1<-yb[i]
    yb_2<-yb[i+1]
    
    if(xa_2!=xa_1){
      k<-(yb_2-yb_1)/(xa_2-xa_1)
      #y<-k*(x-xa_1)+xa_1
    }else{
      k<-NA
      #y=mean(c(yb_1,yb_2))
    }
    K[i]<-k
  }
  
  c_y<-rep(NA,length(c))
  for(i in 1:(length(xa)-1)){
    xa_1<-xa[i]
    xa_2<-xa[i+1]
    yb_1<-yb[i]
    yb_2<-yb[i+1]
    index<-which((c>=xa_1)&(c<=xa_2))
    if(is.na(K[i])){
      c_y[index]<-mean(c(yb_1,yb_2))
    }else {
      if(K[i]==0){
        c_y[index]<-yb_1
      }
      else{
        c_y[index]<-K[i]*(c[index]-xa_1)+yb_1
      }
    }
    
  }
  
  c_y
}

interpolate_PR<-function(Pos_class,Neg_class,c){
  
  x <- Pos_class
  y <- Neg_class
  par(mfrow=c(1,2))
  # compute area under ROC curve for the hard-labeled case
  pr<-pr.curve( x, y, curve = TRUE );
  
  xa<-  rev(pr$curve[,1])
  yb<-  rev(pr$curve[,2])
  
  
  
  K<-vector()
  for(i in 1:(length(xa)-1)){
    xa_1<-xa[i]
    xa_2<-xa[i+1]
    yb_1<-yb[i]
    yb_2<-yb[i+1]
    
    if(xa_2!=xa_1){
      k<-(yb_2-yb_1)/(xa_2-xa_1)
      #y<-k*(x-xa_1)+xa_1
    }else{
      k<-NA
      #y=mean(c(yb_1,yb_2))
    }
    K[i]<-k
  }
  
  c_y<-rep(NA,length(c))
  for(i in 1:(length(xa)-1)){
    xa_1<-xa[i]
    xa_2<-xa[i+1]
    yb_1<-yb[i]
    yb_2<-yb[i+1]
    index<-which((c>=xa_1)&(c<=xa_2))
    if(is.na(K[i])){
      c_y[index]<-mean(c(yb_1,yb_2))
    }else {
      if(K[i]==0){
        c_y[index]<-yb_1
      }
      else{
        c_y[index]<-K[i]*(c[index]-xa_1)+yb_1
      }
    }
    
  }
  
  c_y
}



#### calculate Area under curve
auc<-function(a,b){
  p<-length(a)
  k=0
  
  if(a[1]){
    k<-a[1]*b[1]
    c=a[1]
    for(i in 2:p){
      if(a[i-1]!=a[i]){
        k=k+max(b[i-1],b[i])*(a[i]-c)
        c=a[i]
      }
    }
  }else{
    k<-a[2]*b[2]
    c=a[2]
    for(i in 3:p){
      if(a[i-1]!=a[i]){
        k=k+max(b[i-1],b[i])*(a[i]-c)
        c=a[i]
      }
    }
  }
  
  
  k
}


setwd("/media/root/10AF08F010AF08F03/root/Projects_clinic/prepare_results_for_papers/paper1_MSON/nestedCV/final_result")
HC_DWT_path<-"/media/root/10AF08F010AF08F03/root//Projects_clinic/HC_split/HC_DWT/"
MSON_DWT_path<-"/media/root/10AF08F010AF08F03/root//Projects_clinic/MSON_split/MSON_DWT/"
MSFE_DWT_path<-"/media/root/10AF08F010AF08F03/root//Projects_clinic/MSFE_split/MSFE_DWT/"
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

fit_type.measure<-"deviance"##args[1]
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
Coeff<-matrix(0,nrow(X),22)
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
  
  
  glm.fit<-cv.glmnet(temp_X,temp_Y,nfolds = nrow(temp_X),alignment="fraction",family="binomial",alpha=0, grouped=FALSE,
                     lambda.min.ratio=1e-10,type.measure=fit_type.measure)
  
  test_pred.Prob[sample_i]<-predict(glm.fit,s="lambda.min",test_X,type=predict_type)
  test_pred.Class[sample_i]<-predict(glm.fit,s="lambda.min",test_X,type="class")
  glm.fit$lambda.min
  Coeff[sample_i,]<-coef(glm.fit,s="lambda.min")[-1]


}

write.csv(Coeff,file=paste0("code_LR/logit_HC_MSFE_rounds_",nrounds,"_test_coefficients.csv"))


test_pred.Labels<-ifelse(test_pred.Prob >0.5, 1, 0)
TPR<-length(intersect(which(test_pred.Labels==1),which(Y==1)))/length(which(Y==1))
TNR<-length(intersect(which(test_pred.Labels==0),which(Y==0)))/length(which(Y==0))
SS<-c(TPR,TNR)
SS
write.csv(test_pred.Prob,file=paste0("code_LR/logit_HC_MSFE_rounds_",nrounds,"_test_prob.csv"))
write.csv(test_pred.Labels,file=paste0("code_LR/logit_HC_MSFE_rounds_",nrounds,"_test_labels.csv"))
write.csv(SS,file=paste0("final_result/code_LR/logit_HC_MSFE_rounds_",nrounds,"_test_TPR_TNR.csv"))



n_HC<-63
logit_prob<-matrix(test_pred.Prob,nrow=1)
c<-seq(0,1,length.out = 10001)
c_Y<-matrix(NA,nrow=nrounds,ncol=length(c))
Pos_class<-as.numeric(logit_prob[-c(1:n_HC)])
Neg_class<-as.numeric(logit_prob[c(1:n_HC)])
c_y<-interpolate(Pos_class,Neg_class,c)

logit_Sensitivity<-c_y
logit_auc<-auc(c,logit_Sensitivity)
logit_auc
write.csv(logit_auc,file=paste0("code_LR/logit_HC_MSFE_rounds_",nrounds,"_test_auc.csv"))
temp<-cbind(c,c_y)
colnames(temp)<-c("x","y")
write.csv(temp,file=paste0("code_LR/logit_HC_MSFE_rounds_",nrounds,"_test_plot_data.csv"))

