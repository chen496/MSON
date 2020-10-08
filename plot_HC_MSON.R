library(glmnet)
library(stringr)
library(PRROC)
setwd("/media/root/10AF08F010AF08F03/root/Projects_clinic/prepare_results_for_papers/paper1_MSON/nestedCV/final_result")
n_HC<-63
# True_labels<-c(rep(0,n_HC),rep(1,n_DR))
nrounds<-1

glm_TPR_TNR<-read.csv("glm_HC_MSON_rounds_1_test_TPR_TNR.csv",row.names = 1)
glm_TPR_TNR<-glm_TPR_TNR$x
glm_Sensitivity<-read.csv("glm_HC_MSON_rounds_1_test_plot_data.csv",row.names = 1)
dim(glm_Sensitivity)
glm_auc<-read.csv("glm_HC_MSON_rounds_1_test_auc.csv",row.names = 1)
glm_auc<-glm_auc$x

svm_TPR_TNR<-read.csv("svm_HC_MSON_rounds_1_test_TPR_TNR.csv",row.names = 1)
svm_TPR_TNR<-as.vector(svm_TPR_TNR)
svm_Sensitivity<-read.csv("svm_HC_MSON_rounds_1_test_plot_data.csv",row.names = 1)
dim(svm_Sensitivity)
svm_auc<-read.csv("svm_HC_MSON_rounds_1_test_auc.csv",row.names = 1)
svm_auc<-svm_auc$x

logit_TPR_TNR<-read.csv("logit_HC_MSON_rounds_1_test_TPR_TNR.csv",row.names = 1)
logit_TPR_TNR<-logit_TPR_TNR$x
logit_Sensitivity<-read.csv("logit_HC_MSON_rounds_1_test_plot_data.csv",row.names = 1)
dim(logit_Sensitivity)
logit_auc<-read.csv("logit_HC_MSON_rounds_1_test_auc.csv",row.names = 1)
logit_auc<-logit_auc$x


##### ROC-curve
# plot(xlim=c(0,1),ylim=c(0,1),glm_PRROC$ROC$curve[,1],glm_PRROC$ROC$curve[,2],xlab="1-Specificity",ylab="Sensitivity",
#      col="red",type="l",main=("DR-HC_ROC_curve"))
jpeg(file=paste0("MSON_HC_SVM_LR_LREN_test_tmp.jpeg"),width=867/3,height=795/3,units="mm",res=300)
par(mfrow=c(1,1))
plot(xlim=c(0,1),ylim=c(0,1),glm_Sensitivity[,1],glm_Sensitivity[,2],xlab="1-Specificity",ylab="Sensitivity",
     col="red",type='l',cex=0.6,lwd=2,main=("MSON vs. HC"))
legend("bottomright", c(paste0("SVM auc=",round(svm_auc,2),
                               " TPR=",round(svm_TPR_TNR[1],2)," TNR=",round(svm_TPR_TNR[2],2)),
                        paste0("LR auc=",round(logit_auc,2),
                               " TPR=",round(logit_TPR_TNR[1],2)," TNR=",round(logit_TPR_TNR[2],2)),
                        paste0("LR_EN auc=",round(glm_auc,2),
                               " TPR=",round(glm_TPR_TNR[1],2)," TNR=",round(glm_TPR_TNR[2],2))),
       cex=1, col=c("blue","green","red"), lty=c(4,3,2,1),lwd=c(2,2,2,2))
lines(xlim=c(0,1),ylim=c(0,1),c(0,glm_Sensitivity[,1]),c(0,glm_Sensitivity[,2]),type="l",col="red",cex=0.6,lwd=2)
lines(xlim=c(0,1),ylim=c(0,1),c(0,svm_Sensitivity[,1]),c(0,svm_Sensitivity[,2]),type="l",col="blue",lty=4,cex=0.6,lwd=2)
lines(xlim=c(0,1),ylim=c(0,1),c(0,logit_Sensitivity[,1]),c(0,logit_Sensitivity[,2]),type="l",col="green",lty=2,cex=0.6,lwd=2)
lines(xlim=c(0,1),ylim=c(0,1),seq(0,1,by=0.1),seq(0,1,by=0.1),type="l",col="grey",lty=2,cex=0.6,lwd=2)
dev.off()



jpeg(file=paste0("MSON_HC_SVM_LR_LREN_test.jpeg"),width=867/3,height=795/3,units="mm",res=300)
par(mfrow=c(1,1))

plot(xlim=c(0,1),ylim=c(0,1),glm_Sensitivity[,1],glm_Sensitivity[,2],xlab="1-Specificity",ylab="Sensitivity",
     col="red",type='l',cex=0.6,lwd=2,main=("MSON vs. HC"))

legend("bottomright", c(paste0("SVM     ","auc=",round(svm_auc,2)),
                        paste0("LR        ","auc=",round(logit_auc,2)),
                        paste0("LR_EN auc=",round(glm_auc,2))),
       cex=1, col=c("blue","green","red"), lty=c(4,3,2,1),lwd=c(2,2,2,2))
lines(xlim=c(0,1),ylim=c(0,1),c(0,glm_Sensitivity[,1]),c(0,glm_Sensitivity[,2]),type="l",col="red",cex=0.6,lwd=2)
lines(xlim=c(0,1),ylim=c(0,1),c(0,svm_Sensitivity[,1]),c(0,svm_Sensitivity[,2]),type="l",col="blue",lty=4,cex=0.6,lwd=2)
lines(xlim=c(0,1),ylim=c(0,1),c(0,logit_Sensitivity[,1]),c(0,logit_Sensitivity[,2]),type="l",col="green",lty=2,cex=0.6,lwd=2)
lines(xlim=c(0,1),ylim=c(0,1),seq(0,1,by=0.1),seq(0,1,by=0.1),type="l",col="grey",lty=2,cex=0.6,lwd=2)

dev.off()



