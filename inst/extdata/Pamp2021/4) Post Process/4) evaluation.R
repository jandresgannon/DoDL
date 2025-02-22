##### Evaluation


# remove the old stuff
rm(list=ls())

# load some libraries 
library("sampleSelection")
library("pROC")
library("PRROC")
# load the data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')


load("1) Data/data_ready.RData")

data<-as.data.frame(data)
data[, -c(which(colnames(data)=="sender_id"),which(colnames(data)=="receiver_id"))  ]<- lapply(data[,   -c(which(colnames(data)=="sender_id"),which(colnames(data)=="receiver_id")) ], function(x) as.numeric(as.character(x)))



data$time_reduce<-(data$time-1949)/100
data$timesq<-data$time_reduce^2
data$timecube<-data$time_reduce^3
# valued Statistics in log(1+x) versions
data$outdeg_i_arms<-data$outdeg_i_arms*100
data$outdeg_j_arms<-data$outdeg_j_arms*100
data$indeg_i_arms<-data$indeg_i_arms*100
data$indeg_j_arms<-data$indeg_j_arms*100
data$trans_ij_arms <-data$trans_ij_arms*100        # Arms Network Triadic
data$revtrans_ij_arms<-data$revtrans_ij_arms*100
data$share_sup_ij_arms<-data$share_sup_ij_arms*100
data$share_cust_ij_arms<-data$share_cust_ij_arms*100

data$alliance<-data$response_da_t_1
##########################################

# Model without exclusion restriction: - conflict_j
##########################################

pr_1_insample<-list()
pr_2_insample<-list()
pr_2_insample_withoutda<-list()
pr_2_insample_withoutnw<-list()

pr_1_outsample<-list()
pr_2_outsample<-list()
pr_2_outsample_withoutda<-list()
pr_2_outsample_withoutnw<-list()

val_1_insample<-list()
val_2_insample<-list()
val_2_insample_withoutda<-list()
val_2_insample_withoutnw<-list()

val_1_outsample<-list()
val_2_outsample<-list()
val_2_outsample_withoutda<-list()
val_2_outsample_withoutnw<-list()

for (t in 1965:2018){
print(t)
  
  
  

  
  # Model with conf_j as exclustion restriction
  model_joint_excl3<- selection(response_arms     ~ 1 # Intercept
                                +response_arms_t_1    # Arms Network Dyadic
                                +recip_ji_arms
                                +outdeg_i_arms
                                +outdeg_j_arms
                                +indeg_i_arms
                                +indeg_j_arms 
                                +trans_ij_arms        # Arms Network Triadic
                                +revtrans_ij_arms
                                +share_sup_ij_arms
                                +share_cust_ij_arms
                                +path_dependence      # Path Dependence
                                +alliance             # Defence Agreement
                                +lgdp_i               # log GDP_i
                                +lgdp_j               # log GDP_j
                                +poldiff_ij           # absolute Difference Polity Score
                                +LMilex_i             # log Milex_i
                                +LMilex_j             # log Milex_j
                                +Pop_i                # log Population_i
                                +Pop_j                # log Population_j
                                +distance_ij          # log distance ij
                                +inter_intraconf_j         # intrastate conflict j
                                +embargo_j   ,      # embargo j
                              #  +  as.factor(time),   
                                response_arms_val   ~ # Intercept
                                  +response_arms_t_1    # Arms Network Dyadic
                                +recip_ji_arms
                                +outdeg_i_arms
                                +outdeg_j_arms
                                +indeg_i_arms
                                +indeg_j_arms 
                                +trans_ij_arms        # Arms Network Triadic
                                +revtrans_ij_arms
                                +share_sup_ij_arms
                                +share_cust_ij_arms
                                +path_dependence      # Path Dependence
                                +alliance      # Defence Agreement
                                +lgdp_i               # log GDP_i
                                +lgdp_j               # log GDP_j
                                +poldiff_ij           # absolute Difference Polity Score
                                +LMilex_i             # log Milex_i
                                +LMilex_j             # log Milex_j
                                +Pop_i                # log Population_i
                                +Pop_j                # log Population_j
                                +distance_ij          # log distance ij
                                +embargo_j,         # embargo j
                            #    +  as.factor(time),    
                               data[data$time==t,])
  
  # Model with conf_j as exclustion restriction and no nw stats

  
  model_joint_excl3_nonet<- selection(response_arms     ~ 1 # Intercept
                                    #  +response_arms_t_1    # Arms Network Dyadic
                                    #  +recip_ji_arms
                                    #  +outdeg_i_arms
                                    #  +outdeg_j_arms
                                    #  +indeg_i_arms
                                    #  +indeg_j_arms 
                                    #  +trans_ij_arms        # Arms Network Triadic
                                    #  +revtrans_ij_arms
                                    #  +share_sup_ij_arms
                                    #  +share_cust_ij_arms
                                      +path_dependence      # Path Dependence
                                      +alliance             # Defence Agreement
                                      +lgdp_i               # log GDP_i
                                      +lgdp_j               # log GDP_j
                                      +poldiff_ij           # absolute Difference Polity Score
                                      +LMilex_i             # log Milex_i
                                      +LMilex_j             # log Milex_j
                                      +Pop_i                # log Population_i
                                      +Pop_j                # log Population_j
                                      +distance_ij          # log distance ij
                                      +inter_intraconf_j         # intrastate conflict j
                                      +embargo_j   ,      # embargo j
                                      #  +  as.factor(time),   
                                      response_arms_val   ~ # Intercept
                                   #     +response_arms_t_1    # Arms Network Dyadic
                                    #  +recip_ji_arms
                                    #  +outdeg_i_arms
                                    #  +outdeg_j_arms
                                    #  +indeg_i_arms
                                    #  +indeg_j_arms 
                                    #  +trans_ij_arms        # Arms Network Triadic
                                    #  +revtrans_ij_arms
                                    #  +share_sup_ij_arms
                                    #  +share_cust_ij_arms
                                      +path_dependence      # Path Dependence
                                      +alliance      # Defence Agreement
                                      +lgdp_i               # log GDP_i
                                      +lgdp_j               # log GDP_j
                                      +poldiff_ij           # absolute Difference Polity Score
                                      +LMilex_i             # log Milex_i
                                      +LMilex_j             # log Milex_j
                                      +Pop_i                # log Population_i
                                      +Pop_j                # log Population_j
                                      +distance_ij          # log distance ij
                                      +embargo_j,         # embargo j
                                      #    +  as.factor(time),    
                                      data[data$time==t,])
  
  # Model with emgargo_j as exclusion restriction
  
  
  model_joint_excl3_noda<- selection(response_arms     ~ 1 # Intercept
                                     +response_arms_t_1    # Arms Network Dyadic
                                     +recip_ji_arms
                                     +outdeg_i_arms
                                     +outdeg_j_arms
                                     +indeg_i_arms
                                     +indeg_j_arms 
                                     +trans_ij_arms        # Arms Network Triadic
                                     +revtrans_ij_arms
                                     +share_sup_ij_arms
                                     +share_cust_ij_arms
                                     +path_dependence      # Path Dependence
                                     +alliance             # Defence Agreement
                                     +lgdp_i               # log GDP_i
                                     +lgdp_j               # log GDP_j
                                     +poldiff_ij           # absolute Difference Polity Score
                                     +LMilex_i             # log Milex_i
                                     +LMilex_j             # log Milex_j
                                     +Pop_i                # log Population_i
                                     +Pop_j                # log Population_j
                                     +distance_ij          # log distance ij
                                     +inter_intraconf_j         # intrastate conflict j
                                     +embargo_j   ,      # embargo j
                                     #  +  as.factor(time),   
                                     response_arms_val   ~ # Intercept
                                       +response_arms_t_1    # Arms Network Dyadic
                                     +recip_ji_arms
                                     +outdeg_i_arms
                                     +outdeg_j_arms
                                     +indeg_i_arms
                                     +indeg_j_arms 
                                     +trans_ij_arms        # Arms Network Triadic
                                     +revtrans_ij_arms
                                     +share_sup_ij_arms
                                     +share_cust_ij_arms
                                     +path_dependence      # Path Dependence
                                     +alliance      # Defence Agreement
                                     +lgdp_i               # log GDP_i
                                     +lgdp_j               # log GDP_j
                                     +poldiff_ij           # absolute Difference Polity Score
                                     +LMilex_i             # log Milex_i
                                     +LMilex_j             # log Milex_j
                                     +Pop_i                # log Population_i
                                     +Pop_j                # log Population_j
                                     +distance_ij          # log distance ij
                                     +inter_intraconf_j,         # embargo j
                                     #    +  as.factor(time),    
                                     data[data$time==t,])
  
  
  # Model with embargo_j as exclustion restriction and no nw stats
  
  
  model_joint_excl3_nonw<- selection(response_arms     ~ 1 # Intercept
                                 #    +response_arms_t_1    # Arms Network Dyadic
                                #     +recip_ji_arms
                                 #    +outdeg_i_arms
                                #     +outdeg_j_arms
                                #     +indeg_i_arms
                                #     +indeg_j_arms 
                                #     +trans_ij_arms        # Arms Network Triadic
                                #     +revtrans_ij_arms
                                #     +share_sup_ij_arms
                                #     +share_cust_ij_arms
                                     +path_dependence      # Path Dependence
                                     +alliance             # Defence Agreement
                                     +lgdp_i               # log GDP_i
                                     +lgdp_j               # log GDP_j
                                     +poldiff_ij           # absolute Difference Polity Score
                                     +LMilex_i             # log Milex_i
                                     +LMilex_j             # log Milex_j
                                     +Pop_i                # log Population_i
                                     +Pop_j                # log Population_j
                                     +distance_ij          # log distance ij
                                     +inter_intraconf_j         # intrastate conflict j
                                     +embargo_j   ,      # embargo j
                                     #  +  as.factor(time),   
                                     response_arms_val   ~ # Intercept
                                #       +response_arms_t_1    # Arms Network Dyadic
                                #     +recip_ji_arms
                                #     +outdeg_i_arms
                                #     +outdeg_j_arms
                                #     +indeg_i_arms
                                #     +indeg_j_arms 
                                #     +trans_ij_arms        # Arms Network Triadic
                                #     +revtrans_ij_arms
                                #     +share_sup_ij_arms
                                #     +share_cust_ij_arms
                                     +path_dependence      # Path Dependence
                                     +alliance      # Defence Agreement
                                     +lgdp_i               # log GDP_i
                                     +lgdp_j               # log GDP_j
                                     +poldiff_ij           # absolute Difference Polity Score
                                     +LMilex_i             # log Milex_i
                                     +LMilex_j             # log Milex_j
                                     +Pop_i                # log Population_i
                                     +Pop_j                # log Population_j
                                     +distance_ij          # log distance ij
                                     +inter_intraconf_j,         # embargo j
                                     #    +  as.factor(time),    
                                     data[data$time==t,])

  # Binary model with nw stats
  
  model_joint_excl3_probit<- glm( response_arms     ~ 1 # Intercept
                                 +response_arms_t_1    # Arms Network Dyadic
                                 +recip_ji_arms
                                 +outdeg_i_arms
                                 +outdeg_j_arms
                                 +indeg_i_arms
                                 +indeg_j_arms 
                                 +trans_ij_arms        # Arms Network Triadic
                                 +revtrans_ij_arms
                                 +share_sup_ij_arms
                                 +share_cust_ij_arms
                                 +path_dependence      # Path Dependence
                                 +alliance             # Defence Agreement
                                 +lgdp_i               # log GDP_i
                                 +lgdp_j               # log GDP_j
                                 +poldiff_ij           # absolute Difference Polity Score
                                 +LMilex_i             # log Milex_i
                                 +LMilex_j             # log Milex_j
                                 +Pop_i                # log Population_i
                                 +Pop_j                # log Population_j
                                 +distance_ij          # log distance ij
                                 +inter_intraconf_j         # intrastate conflict j
                                 +embargo_j   
,data[data$time==t,],family=binomial(link="probit"))

  # binary model without nw stats
  
model_joint_excl3_nonet_probit<- glm(response_arms     ~ 1 # Intercept
                                #     +response_arms_t_1    # Arms Network Dyadic
                                #     +recip_ji_arms
                                #     +outdeg_i_arms
                                #     +outdeg_j_arms
                                #     +indeg_i_arms
                                #     +indeg_j_arms 
                                #     +trans_ij_arms        # Arms Network Triadic
                                #     +revtrans_ij_arms
                                #     +share_sup_ij_arms
                                #     +share_cust_ij_arms
                                     +path_dependence      # Path Dependence
                                     +alliance             # Defence Agreement
                                     +lgdp_i               # log GDP_i
                                     +lgdp_j               # log GDP_j
                                     +poldiff_ij           # absolute Difference Polity Score
                                     +LMilex_i             # log Milex_i
                                     +LMilex_j             # log Milex_j
                                     +Pop_i                # log Population_i
                                     +Pop_j                # log Population_j
                                     +distance_ij          # log distance ij
                                     +inter_intraconf_j         # intrastate conflict j
                                     +embargo_j   
,data[data$time==t,],family=binomial(link="probit"))




pr_1_insample[[t-1964]]<-cbind(predict(model_joint_excl3_probit,type="response"),data[data$time==t,2])
pr_2_insample[[t-1964]]<-cbind(predict(model_joint_excl3_nonet_probit,type="response"),data[data$time==t,2])
#pr_2_insample_withoutda[[t-1964]]<-cbind(predict(model_joint_excl3_noda_probit,type="response"),data[data$time==t,2])
#pr_2_insample_withoutnw[[t-1964]]<-cbind(predict(model_joint_excl3_nonw_probit,type="response"),data[data$time==t,2])

val_1_insample[[t-1964]]<-cbind(predict(model_joint_excl3),data[data$time==t,4])
val_2_insample[[t-1964]]<-cbind(predict(model_joint_excl3_nonet),data[data$time==t,4])
val_2_insample_withoutda[[t-1964]]<-cbind(predict(model_joint_excl3_noda),data[data$time==t,4])
val_2_insample_withoutnw[[t-1964]]<-cbind(predict(model_joint_excl3_nonw),data[data$time==t,4])


val_1_insample[[t-1964]]<-val_1_insample[[t-1964]][is.na(val_1_insample[[t-1964]][,1])==F,]
val_2_insample[[t-1964]]<-val_2_insample[[t-1964]][is.na(val_2_insample[[t-1964]][,1])==F,]
val_2_insample_withoutda[[t-1964]]<-val_2_insample_withoutda[[t-1964]][is.na(val_2_insample_withoutda[[t-1964]][,1])==F,]
val_2_insample_withoutnw[[t-1964]]<-val_2_insample_withoutnw[[t-1964]][is.na(val_2_insample_withoutnw[[t-1964]][,1])==F,]


if (t<2018){
  pr_1_outsample[[t-1964]]<-cbind(predict(model_joint_excl3_probit,newdata = data[data$time==t+1,],type="response"),data[data$time==t+1,2])
  pr_2_outsample[[t-1964]]<-cbind(predict(model_joint_excl3_nonet_probit,newdata = data[data$time==t+1,],type="response"),data[data$time==t+1,2])
  #pr_2_outsample_withoutda[[t-1964]]<-cbind(predict(model_joint_excl3_noda_probit,newdata = data[data$time==t+1,],type="response"),data[data$time==t+1,2])
  #pr_2_outsample_withoutnw[[t-1964]]<-cbind(predict(model_joint_excl3_nonw_probit,newdata = data[data$time==t+1,],type="response"),data[data$time==t+1,2])
  
  val_1_outsample[[t-1964]]<-cbind(predict(model_joint_excl3,newdata = data[data$time==t+1,]),data[data$time==t+1,4])
  val_2_outsample[[t-1964]]<-cbind(predict(model_joint_excl3_nonet,newdata = data[data$time==t+1,]),data[data$time==t+1,4])
 
   val_2_outsample_withoutda[[t-1964]]<-cbind(predict(model_joint_excl3_noda,newdata = data[data$time==t+1,]),data[data$time==t+1,4])
  val_2_outsample_withoutnw[[t-1964]]<-cbind(predict(model_joint_excl3_nonw,newdata = data[data$time==t+1,]),data[data$time==t+1,4])
  
  val_1_outsample[[t-1964]]<-val_1_outsample[[t-1964]][data[data$time==t+1,4]!=0,]
  val_2_outsample[[t-1964]]<-val_2_outsample[[t-1964]][data[data$time==t+1,4]!=0,]
 
  val_2_outsample_withoutda[[t-1964]]<-val_2_outsample_withoutda[[t-1964]][data[data$time==t+1,4]!=0,]
  val_2_outsample_withoutnw[[t-1964]]<-val_2_outsample_withoutnw[[t-1964]][data[data$time==t+1,4]!=0,]
  
  
  }

}


R2_1_insample<-c()
R2_1_outsample<-c()
R2_2_insample<-c()
R2_2_outsample<-c()

R2_2_insample_noda<-c()
R2_2_outsample_noda<-c()

R2_2_insample_nonw<-c()
R2_2_outsample_nonw<-c()

for (i in 1:length(pr_1_insample)){
  print(i)
  R2_1_insample<-c(R2_1_insample,mean((val_1_insample[[i]][,1]-val_1_insample[[i]][,2])  ^2))
  R2_2_insample<-c(R2_2_insample,mean((val_2_insample[[i]][,1]-val_2_insample[[i]][,2])  ^2))
 
  R2_2_insample_noda<-c(R2_2_insample_noda,mean((val_2_insample_withoutda[[i]][,1]-val_2_insample_withoutda[[i]][,2])  ^2))
  R2_2_insample_nonw<-c(R2_2_insample_nonw,mean((val_2_insample_withoutnw[[i]][,1]-val_2_insample_withoutnw[[i]][,2])  ^2))
  if (i<52){
    R2_1_outsample<-c(R2_1_outsample,mean((val_1_outsample[[i]][,1]-val_1_outsample[[i]][,2])  ^2))
    R2_2_outsample<-c(R2_2_outsample,mean((val_2_outsample[[i]][,1]-val_2_outsample[[i]][,2])  ^2))
    R2_2_outsample_noda<-c(R2_2_outsample_noda,mean((val_2_outsample_withoutda[[i]][,1]-val_2_outsample_withoutda[[i]][,2])  ^2))
    R2_2_outsample_nonw<-c(R2_2_outsample_nonw,mean((val_2_outsample_withoutnw[[i]][,1]-val_2_outsample_withoutnw[[i]][,2])  ^2))
  }
  
}




setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')



pdf("4) Post Process/MSR_insample_outsample_conflict.pdf",width = 10,height = 4)
par (mfrow=c(1,2))

plot(R2_1_insample,type="l",ylim=c(1,5),xaxt="n",lwd=2,ylab="MSR",main="In-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(R2_2_insample,col="red",ylim=c(0.95,1),lwd=2)
axis(1,1:54,1965:2018,cex.axis=1.5)
#abline(v=which(R2_1_insample>R2_2_insample_nonw),lty=2)

plot(R2_1_outsample,type="l",ylim=c(1,5),xaxt="n",lwd=2,ylab="MSR",main="Out-of-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(R2_2_outsample,col="red",lwd=2)
#abline(v=which(R2_1_outsample>R2_2_outsample_nonw),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)

dev.off()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

pdf("4) Post Process/MSR_insample_outsample_embargo.pdf",width = 10,height = 4)
par (mfrow=c(1,2))

plot(R2_2_insample_noda,type="l",ylim=c(1,5),xaxt="n",lwd=2,ylab="MSR",main="In-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(R2_2_insample_nonw,col="red",ylim=c(0.95,1),lwd=2)
axis(1,1:54,1965:2018,cex.axis=1.5)
#abline(v=which(R2_1_insample>R2_2_insample_nonw),lty=2)

plot(R2_2_outsample_noda,type="l",ylim=c(1,5),xaxt="n",lwd=2,ylab="MSR",main="Out-of-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(R2_2_outsample_nonw,col="red",lwd=2)
#abline(v=which(R2_1_outsample>R2_2_outsample_nonw),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)

dev.off()

roc_1_insample<-c()
roc_2_insample<-c()
roc_2_insample_noda<-c()
roc_2_insample_nonw<-c()

roc_1_outsample<-c()
roc_2_outsample<-c()
roc_2_outsample_noda<-c()
roc_2_outsample_nonw<-c()


prec_1_insample<-c()
prec_2_insample<-c()
prec_2_insample_noda<-c()
prec_2_insample_nonw<-c()

prec_1_outsample<-c()
prec_2_outsample<-c()
prec_2_outsample_noda<-c()
prec_2_outsample_nonw<-c()

for (i in 1:length(pr_1_insample)){
 print(i)
  roc_1_insample<-c(roc_1_insample,auc(roc(pr_1_insample[[i]][,2],pr_1_insample[[i]][,1])))
  roc_2_insample<-c(roc_2_insample,auc(roc(pr_2_insample[[i]][,2],pr_2_insample[[i]][,1])))
  #roc_2_insample_noda<-c(roc_2_insample_noda,auc(roc(pr_2_insample_withoutda[[i]][,2],pr_2_insample_withoutda[[i]][,1])))
  #roc_2_insample_nonw<-c(roc_2_insample_nonw,auc(roc(pr_2_insample_withoutnw[[i]][,2],pr_2_insample_withoutnw[[i]][,1])))
  
  
  prec_1_insample<-c(prec_1_insample,pr.curve(pr_1_insample[[i]][pr_1_insample[[i]][,2]==1,1],pr_1_insample[[i]][pr_1_insample[[i]][,2]==0,1])$auc.integral)
  prec_2_insample<-c(prec_2_insample,pr.curve(pr_2_insample[[i]][pr_2_insample[[i]][,2]==1,1],pr_2_insample[[i]][pr_2_insample[[i]][,2]==0,1])$auc.integral)
  #prec_2_insample_noda<-c(prec_2_insample_noda,pr.curve(pr_2_insample_withoutda[[i]][pr_2_insample_withoutda[[i]][,2]==1,1],pr_2_insample_withoutda[[i]][pr_2_insample_withoutda[[i]][,2]==0,1])$auc.integral)
  #prec_2_insample_nonw<-c(prec_2_insample_nonw,pr.curve(pr_2_insample_withoutnw[[i]][pr_2_insample_withoutnw[[i]][,2]==1,1],pr_2_insample_withoutnw[[i]][pr_2_insample_withoutnw[[i]][,2]==0,1])$auc.integral)
  
  
  if (i<52){
  roc_1_outsample<-c(roc_1_outsample,auc(roc(pr_1_outsample[[i]][,2],pr_1_outsample[[i]][,1])))
  roc_2_outsample<-c(roc_2_outsample,auc(roc(pr_2_outsample[[i]][,2],pr_2_outsample[[i]][,1])))
  #roc_2_outsample_noda<-c(roc_2_outsample_noda,auc(roc(pr_2_outsample_withoutda[[i]][,2],pr_2_outsample_withoutda[[i]][,1])))
  #roc_2_outsample_nonw<-c(roc_2_outsample_nonw,auc(roc(pr_2_outsample_withoutnw[[i]][,2],pr_2_outsample_withoutnw[[i]][,1])))
  
  
  prec_1_outsample<-c(prec_1_outsample,pr.curve(pr_1_outsample[[i]][pr_1_outsample[[i]][,2]==1,1],pr_1_outsample[[i]][pr_1_outsample[[i]][,2]==0,1])$auc.integral)
  prec_2_outsample<-c(prec_2_outsample,pr.curve(pr_2_outsample[[i]][pr_2_outsample[[i]][,2]==1,1],pr_2_outsample[[i]][pr_2_outsample[[i]][,2]==0,1])$auc.integral)
#  prec_2_outsample_noda<-c(prec_2_outsample_noda,pr.curve(pr_2_outsample_withoutda[[i]][pr_2_outsample_withoutda[[i]][,2]==1,1],pr_2_outsample_withoutda[[i]][pr_2_outsample_withoutda[[i]][,2]==0,1])$auc.integral)
#  prec_2_outsample_nonw<-c(prec_2_outsample_nonw,pr.curve(pr_2_outsample_withoutnw[[i]][pr_2_outsample_withoutnw[[i]][,2]==1,1],pr_2_outsample_withoutnw[[i]][pr_2_outsample_withoutnw[[i]][,2]==0,1])$auc.integral)
}
   
}



pdf("4) Post Process/AUC_ROC_insample_outsample.pdf",width = 10,height = 4)
par (mfrow=c(1,2))

plot(roc_1_insample,type="l",ylim=c(0.95,1),xaxt="n",lwd=2,ylab="AUC (ROC)",main="In-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(roc_2_insample,col="red",ylim=c(0.95,1),lwd=2)
axis(1,1:54,1965:2018,cex.axis=1.5)
#abline(v=which(roc_1_insample<roc_2_insample),lty=2)

plot(roc_1_outsample,type="l",ylim=c(0.95,1),xaxt="n",lwd=2,ylab="AUC (ROC)",main="Out-of-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(roc_2_outsample,col="red",ylim=c(0.95,1),lwd=2)
#abline(v=which(roc_1_outsample<roc_2_outsample),lty=2)
axis(1,1:54,1965:2018,cex.axis=1.5)
dev.off()





pdf("4) Post Process/AUC_PR_insample_outsample.pdf",width = 10,height = 4)
par (mfrow=c(1,2))

plot(prec_1_insample,type="l",ylim=c(0.5,0.9),xaxt="n",lwd=2,ylab="AUC (PR)",main="In-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(prec_2_insample,col="red",lwd=2)
axis(1,1:54,1965:2018,cex.axis=1.5)
#abline(v=which(prec_1_insample<prec_2_insample),lty=2)

plot(prec_1_outsample,type="l",ylim=c(0.5,0.9),xaxt="n",lwd=2,ylab="AUC (PR)", main="Out-of-sample (without network statistics in red)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(prec_2_outsample,col="red",ylim=c(0,1),lwd=2)
#abline(v=which(prec_1_outsample<prec_2_outsample),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)


dev.off()



# Now again some plots that look nicely
pdf("4) Post Process/PR_MSR_out.pdf",width = 12,height = 4)
par (mfrow=c(1,2))


plot(prec_1_outsample,type="l",ylim=c(0.5,0.9),xaxt="n",lwd=2,ylab="AUC (PR)", main="Out-of-sample (without network statistics in dashed)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(prec_2_outsample,lty=2,ylim=c(0,1),lwd=2)
#abline(v=which(prec_1_outsample<prec_2_outsample),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)


plot(R2_1_outsample,type="l",ylim=c(1,5),xaxt="n",lwd=2,ylab="MSR",main="Out-of-sample (without network statistics in dashed)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(R2_2_outsample,lty=2,lwd=2)
#abline(v=which(R2_1_outsample>R2_2_outsample_nonw),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)


dev.off()

pdf("4) Post Process/PR_MSR_in.pdf",width = 12,height = 4)
par (mfrow=c(1,2))


plot(prec_1_insample,type="l",ylim=c(0.5,0.9),xaxt="n",lwd=2,ylab="AUC (PR)", main="In-sample (without network statistics in dashed)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(prec_2_insample,lty=2,ylim=c(0,1),lwd=2)
#abline(v=which(prec_1_outsample<prec_2_outsample),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)


plot(R2_1_insample,type="l",ylim=c(1,5),xaxt="n",lwd=2,ylab="MSR",main="In-sample (without network statistics in dashed)",xlab="t",cex.lab=1.5,cex.axis=1.5)
lines(R2_2_insample,lty=2,lwd=2)
#abline(v=which(R2_1_outsample>R2_2_outsample_nonw),lty=2)
axis(1,1:53,1966:2018,cex.axis=1.5)


dev.off()




