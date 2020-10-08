rm(list = ls())

library(kernlab)
library(e1071)
### loading data
MSFE_DWT_path<-"/media/root/10AF08F010AF08F03/root/Projects_clinic/MSFE_split/MSFE_DWT/"
MSON_DWT_path<-"/media/root/10AF08F010AF08F03/root/Projects_clinic/MSON_split/MSON_DWT/"



feature<-c("a",paste0("h",7:1),paste0("v",7:1),paste0("d",7:1))
Vname<-"bior1.5"
vname<-Vname
MSFE_DWT_engy_file<-paste0(MSFE_DWT_path,vname,"_MSFE.csv")
MSFE_DWT_engy<-read.csv(MSFE_DWT_engy_file,header = FALSE)
dim(MSFE_DWT_engy)
MSFE_DWT_engy[1,1]



MSON_DWT_engy_file<-paste0(MSON_DWT_path,vname,"_MSON.csv")
MSON_DWT_engy<-read.csv(MSON_DWT_engy_file,header = FALSE)
dim(MSON_DWT_engy)
MSON_DWT_engy[1,1]


dim(MSON_DWT_engy)


A_engy<-MSFE_DWT_engy
dim(A_engy)
A_engy[1,1]

B_engy<-MSON_DWT_engy
dim(B_engy)

B_engy[1,1]




nfeatures<-ncol(MSFE_DWT_engy)
data0<-A_engy
data1<-B_engy
nsample<-nrow(data0)+nrow(data1)
nrounds<-1
nfolds<-nsample-1

X<-rbind(data0,data1)
X<-matrix(unlist(X),nrow=nrow(X),ncol=ncol(X))
data<-X
Y<-c(rep("A",nrow(data0)),rep("B",nrow(data1)))

##X<-scale(X,center = TRUE,scale = TRUE)



Data<-cbind.data.frame(X,Y)
names(Data)<-c(feature,"label")

SS<-matrix(0,nrounds,ncol=2)
Pred.Prob<-matrix(0,nrounds,ncol=nsample)
TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)

SS_Valid<-matrix(0,nrounds,ncol=2)
SS_Valid_pred.Prob<-matrix(0,nrounds,nsample)
SS_Valid_TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)

nfolds<-nsample-1
M<-vector()

for(seed_i in 12){
  set.seed(seed_i)
  test_pred.Prob<-rep(NA,nsample)
  test_pred.Labels1<-rep(NA,nsample)
  Valid_pred.Prob<-list()
  ### leave one out
  for(sample_i in 1:nrow(X)){
    print("sample:")
    print(sample_i)
    test_X<-X[sample_i,]
    test_X<-matrix(test_X,nrow=1)
    temp_X<-X[-sample_i,]
    temp_Y<-Y[-sample_i]
    
    data<-cbind.data.frame(temp_X,temp_Y)
    names(data)<-c(feature,"label")
    ###  10-fold cv 
    foldID<-sample(rep(seq(nfolds), length =nsample-1))
    valid_pred.Prob<-rep(NA,nsample-1)
    
    tuned<-tune.svm(label ~.,data=data,gamma = 10^(-4:0), cost = 10^(-2:2),
                    tunecontrol=tune.control(cross=nfolds),probability=TRUE,
                    kernel="radial")
    
    tuned<-svm(label ~.,data=data,gamma = 0.01, cost = 10,
                    tunecontrol=tune.control(cross=nfolds),probability=TRUE,
                    kernel="radial")
    temp<-predict(tuned$best.model,Data[sample_i,],probability=TRUE,kernel="radial")
    test_pred.Prob[sample_i]<-attr(temp,"probabilities")[,2]
    temp1<-predict(tuned$best.model,Data[sample_i,])
    test_pred.Labels1[sample_i]<-as.character(temp1)
    temp<-predict(tuned$best.model,Data,probability=TRUE,kernel="radial")
  }
  
  test_pred.Labels<-ifelse(test_pred.Prob>0.5,"B","A")
  
  TPR<-length(intersect(which(test_pred.Labels=="B"),which(Y=="B")))/length(which(Y=="B"))
  TNR<-length(intersect(which(test_pred.Labels=="A"),which(Y=="A")))/length(which(Y=="A"))
  c(TPR,TNR)
  
  
  svm_prob<-test_pred.Prob
  n_MSFE<-25
  c<-seq(0,1,length.out = 1001)
  Pos_class<-as.numeric(svm_prob[-c(1:n_MSFE)])
  Neg_class<-as.numeric(svm_prob[c(1:n_MSFE)])
  c_y<-interpolate(Pos_class,Neg_class,c)
  svm_Sensitivity<-c_y
  svm_auc<-auc(c,(svm_Sensitivity))
  svm_auc
  print(svm_auc)
  M[seed_i]<-svm_auc
  print(paste0("seed:",seed_i,"auc:",M[seed_i]))
}



Pred.Prob<-test_pred.Prob
tp<-length(intersect(which(test_pred.Labels=="B"),which(Y=="B")))
fp<-length(which(test_pred.Labels=="B"))-length(intersect(which(test_pred.Labels=="B"),which(Y=="B")))
fn<-length(which(test_pred.Labels=="A"))-length(intersect(which(test_pred.Labels=="A"),which(Y=="A")))
tn<-length(intersect(which(test_pred.Labels=="A"),which(Y=="A")))                     
TP_FP_FN_TN<-c(tp,fp,fn,tn)


test_pred.Labels
TPR<-length(intersect(which(test_pred.Labels1=="B"),which(Y=="B")))/length(which(Y=="B"))
TNR<-length(intersect(which(test_pred.Labels1=="A"),which(Y=="A")))/length(which(Y=="A"))
SS<-c(TPR,TNR)
SS
write.csv(test_pred.Labels1,file=paste0("svm_MSFE_MSON_rounds_",nrounds,"_test_labels.csv"))
write.csv(Pred.Prob,file=paste0("svm_MSFE_MSON_rounds_",nrounds,"_test_prob.csv"))
write.csv(SS,file=paste0("svm_MSFE_MSON_rounds_",nrounds,"_test_TPR_TNR.csv"))
write.csv(TP_FP_FN_TN,file=paste0("svm_MSFE_MSON_rounds_",nrounds,"_test_TP_FP_FN_TN.csv"))
