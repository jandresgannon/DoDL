#-----------------------------------------------------------------------------------------------------------#
# Regression Model
#-----------------------------------------------------------------------------------------------------------#

# remove the old stuff
rm(list=ls())

# load some libraries 
library("sampleSelection")
library("stargazer")
library("coefplot")

memory.limit(size=56000)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')





#-----------------------------------------------------------------------------------------------------------#
# Feature Extraction: Identification of countries that are assume to be producers
#-----------------------------------------------------------------------------------------------------------#

load("1) Data/Data.RData")

rS_pre=c()
rS_post = c()
for (i in 1955:2018){
  
  if (i-1950<=41){
    
    rS_pre=cbind(rS_pre, rowSums(amk[[i-1950]][1:224,1:224]))    
  }
  
  if (i-1950>1){
    
    rS_post=cbind(rS_post, rowSums(amk[[i-1950]][1:224,1:224]))    
  }  
  
}

rS_pre_comb = rowSums(rS_pre)>0
rS_post_comb = rowSums(rS_post)>0

producer_pre=names(rS_pre_comb)[rS_pre_comb==TRUE]
producer_post=names(rS_post_comb)[rS_post_comb==TRUE]
producer_list = c("United States", "United Kingdom", "France", "Italy", "Russia", "Soviet Union", "Japan", "Germany", "German Democratic Republic", "Israel", "Sweden", "India", "South Korea", "Singapore", "Poland", "Australia", "Ukraine", "Brazil", "Switzerland", "Canada", "Spain")


# cleanup
rm(list=ls()[- c(which(ls()=='producer_pre'), which(ls()=='producer_post'), which(ls()=='producer_list'))])


#-----------------------------------------------------------------------------------------------------------#
# Data loading and manipulation
#-----------------------------------------------------------------------------------------------------------#


# load the data needed for regression ----
load("1) Data/data_ready.RData")


partial_dependence= function(model,data,var='outdeg_i_arms', spacing=1,add=1){
  
  min_var=min(data[,colnames(data)==var])
  max_var=max(data[,colnames(data)==var])+add
  seq_var = seq(from=min_var,to=max_var, by=spacing)
  out = c()
  for (i in 1:length(seq_var)){
    print(i/length(seq_var))
     data_c=data
     data_c[,colnames(data)==var] = seq_var[i]
     pxb=pnorm(predict(model,data_c,type='link'))
     out = c(out,mean(pxb))
    
    
    
  }
  return(list(out=out, seq_var=seq_var,data=data,var=var))
  
}







avg_margins = function(model, data ,start=1, end=1){
  names =   names(model$estimate)[start:end]
  est = model$estimate[start:end]
  marg_eff=c()
  #pxb=predict(model,data,type='response')
  pxb=pnorm(predict(model,data,type='link'))
  dxb=dnorm(predict(model,data,type='link'))
  
  for (i in names){
    
    if (i == "(Intercept)" ){
      
      marg = dxb*est[names(est)==i]
      marg_eff =c(marg_eff,mean(marg))
      
    }
    
    if (i != "(Intercept)" ){
      
      if (length(data[,colnames(data)==i])==0){
        marg = NaN
        marg_eff=c(marg_eff,marg)}
      if (length(data[,colnames(data)==i])>0){ 
        if (length(unique(data[,colnames(data)==i]))==2){
          
          #pxb=pnorm(predict(model,data,type='link'))
          data1 = data
          data0 = data
          data1[,colnames(data1)==i]=1
          data0[,colnames(data0)==i]=0
          
          
          marg = pnorm(predict(model,data1,type='link'))-pnorm(predict(model,data0,type='link'))
          
          
          marg_eff =c(marg_eff,mean(marg))  
        }  
        
        if (length(unique(data[,colnames(data)==i]))!=2){
          marg = dxb*est[names(est)==i]
          marg_eff =c(marg_eff,mean(marg))  
        }  
        
      }
      
    }
    print(i)
    print(mean(marg))
    
    
  }
  names(marg_eff)=names
  return(marg_eff)
  
}


# for probabilities

avg_margins_quantiles = function(model, data ,start=1, end=1){
  names =   names(model$estimate)[start:end]
  est = model$estimate[start:end]
  
  # calculate quantiles
  q = quantile(c(data[names(data)==names])[[1]])
  
  
  marg_eff=c()

  for (q_ in q){
  data_ = data
  data_[names(data_)==names]=q_
  pxb=pnorm(predict(model,data_,type='link'))
  dxb=dnorm(predict(model,data_,type='link'))
  
  for (i in names){
    
    if (i == "(Intercept)" ){
      
      marg = dxb*est[names(est)==i]
      marg_eff =c(marg_eff,mean(marg))
      
    }
    
    if (i != "(Intercept)" ){
      
      if (length(data_[,colnames(data_)==i])==0){
        marg = NaN
        marg_eff=c(marg_eff,marg)}
      if (length(data[,colnames(data_)==i])>0){ 
        if (length(unique(data_[,colnames(data_)==i]))==1){
          
          pxb=pnorm(predict(model,data_,type='link'))
          data1 = data_
          data0 = data_
          data1[,colnames(data1)==i]=1
          data0[,colnames(data0)==i]=0
          
          
          marg = pnorm(predict(model,data_,type='link'))-pnorm(predict(model,data0,type='link'))
          
          marg_eff =c(marg_eff,mean(marg))  
        }  
        
        if (length(unique(data_[,colnames(data_)==i]))!=1){
          marg = dxb*est[names(est)==i]
          marg_eff =c(marg_eff,mean(marg))  
        }  
        
      }
      
    }
    print(i)
    print(mean(marg))
    
    
  }
  }
  return(marg_eff)
  
}



# for second_stade

margins_quantiles = function(model, data ,start=1, end=1){
  names =   names(model$estimate)[start:end]
  est = model$estimate[start:end]
  
  # calculate quantiles
  q = quantile(c(data[names(data)==names])[[1]])
  
  
  marg_eff=c()
  
  for (q_ in q){
    data_ = data
    data_[names(data_)==names]=q_
    #pxb=pnorm(predict(model,data_,type='link'))
    #dxb=dnorm(predict(model,data_,type='link'))
    
    for (i in names){
      
      if (i == "(Intercept)" ){
        
        marg = est[names(est)==i]
        marg_eff =c(marg_eff,mean(marg))
        
      }
      
      if (i != "(Intercept)" ){
        
        if (length(data_[,colnames(data_)==i])==0){
          marg = NaN
          marg_eff=c(marg_eff,marg)}
        if (length(data[,colnames(data_)==i])>0){ 
          if (length(unique(data_[,colnames(data_)==i]))==1){
            
            pxb=pnorm(predict(model,data_,type='link'))
            data1 = data_
            data0 = data_
            data1[,colnames(data1)==i]=1
            data0[,colnames(data0)==i]=0
            
            
            marg = est[names(est)==i]*q_
            
            marg_eff =c(marg_eff,mean(marg))  
          }  
          
          if (length(unique(data_[,colnames(data_)==i]))!=1){
            marg = est[names(est)==i]*q_
            marg_eff =c(marg_eff,mean(marg))  
          }  
          
        }
        
      }
      print(i)
      print(mean(marg))
      
      
    }
  }
  return(marg_eff)
  
}




data<-as.data.frame(data)
data[, -c(which(colnames(data)=="sender_id"),which(colnames(data)=="receiver_id"))  ]<- lapply(data[,   -c(which(colnames(data)=="sender_id"),which(colnames(data)=="receiver_id")) ], function(x) as.numeric(as.character(x)))
data$producer_sender_pre =  data$sender_id%in%producer_pre
data$producer_sender_post =  data$sender_id%in%producer_post

data$producer_receiver_pre =  data$receiver_id%in%producer_pre
data$producer_receiver_post =  data$receiver_id%in%producer_post

data$producer_sender_list =  data$sender_id%in%producer_list
data$producer_receiver_list =  data$receiver_id%in%producer_list


## Data Transformation----

# Distance in Logs
data[,which(colnames(data)=="distance_ij")]<-log(1+exp(data[,which(colnames(data)=="distance_ij")]))

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



#-----------------------------------------------------------------------------------------------------------#
# Modelling - Pre Cold War ----
#-----------------------------------------------------------------------------------------------------------#

# inter_intra_conf_j is the exclusion restriction ----
# with time dummies
model_pre_conf_excl<- selection(response_arms     ~ 1 # Intercept
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
                              +embargo_j         # embargo j
                              +as.factor(time),
                               response_arms_val   ~ 1# Intercept
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
                              +embargo_j         # embargo j
                              +as.factor(time),   
                              data=data[which(data$time<1992),])


aic_model_pre_conf_excl<-AIC(model_pre_conf_excl)
bic_model_pre_conf_excl<-BIC(model_pre_conf_excl)
ll_model_pre_conf_excl<-logLik(model_pre_conf_excl)
nobs_model_pre_conf_excl<-summary(model_pre_conf_excl)$param$nObs
nobs2_model_pre_conf_excl<-summary(model_pre_conf_excl)$param$N1
margins_model_pre_conf_excl=avg_margins(model_pre_conf_excl,data[which(data$time<1992),],start=1, end=24)

library("stargazer")
stargazer(as.matrix(margins_model_pre_conf_excl[1:23]),type='text')





# inter_intra_conf_j is the exclusion restriction ----
# with time dummies and interaction terms based on the list
model_pre_conf_excl_inter<- selection(response_arms     ~ 1 # Intercept
                                
                                +producer_sender_list
                                +producer_receiver_list
                                        
                                +response_arms_t_1*producer_sender_list
                                +response_arms_t_1*producer_receiver_list
                                +response_arms_t_1 
                                
                                +recip_ji_arms*producer_sender_list
                                +recip_ji_arms*producer_receiver_list
                                +recip_ji_arms
                                
                                +outdeg_i_arms*producer_sender_list
                                +outdeg_i_arms*producer_receiver_list
                                +outdeg_i_arms
                                
                                +outdeg_j_arms*producer_sender_list
                                +outdeg_j_arms*producer_receiver_list
                                +outdeg_j_arms
                                
                                +indeg_i_arms*producer_sender_list
                                +indeg_i_arms*producer_receiver_list
                                +indeg_i_arms
                                
                                +indeg_j_arms*producer_sender_list
                                +indeg_j_arms*producer_receiver_list
                                +indeg_j_arms
                                
                                +trans_ij_arms*producer_sender_list
                                +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                +trans_ij_arms
                                
                                +revtrans_ij_arms*producer_sender_list
                                +revtrans_ij_arms*producer_receiver_list
                                +revtrans_ij_arms
                                
                                +share_sup_ij_arms*producer_sender_list
                                +share_sup_ij_arms*producer_receiver_list
                                +share_sup_ij_arms
                                
                                +share_cust_ij_arms*producer_sender_list
                                +share_cust_ij_arms*producer_receiver_list
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
                                +embargo_j         # embargo j
                                +as.factor(time),
                                response_arms_val   ~ 1# Intercept
                                  
                                +producer_sender_list
                                +producer_receiver_list
                                
                                +response_arms_t_1*producer_sender_list
                                +response_arms_t_1*producer_receiver_list
                                +response_arms_t_1 
                                
                                +recip_ji_arms*producer_sender_list
                                +recip_ji_arms*producer_receiver_list
                                +recip_ji_arms
                                
                                +outdeg_i_arms*producer_sender_list
                                +outdeg_i_arms*producer_receiver_list
                                +outdeg_i_arms
                                
                                +outdeg_j_arms*producer_sender_list
                                +outdeg_j_arms*producer_receiver_list
                                +outdeg_j_arms
                                
                                +indeg_i_arms*producer_sender_list
                                +indeg_i_arms*producer_receiver_list
                                +indeg_i_arms
                                
                                +indeg_j_arms*producer_sender_list
                                +indeg_j_arms*producer_receiver_list
                                +indeg_j_arms
                                
                                +trans_ij_arms*producer_sender_list
                                +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                +trans_ij_arms
                                
                                +revtrans_ij_arms*producer_sender_list
                                +revtrans_ij_arms*producer_receiver_list
                                +revtrans_ij_arms
                                
                                +share_sup_ij_arms*producer_sender_list
                                +share_sup_ij_arms*producer_receiver_list
                                +share_sup_ij_arms
                                
                                +share_cust_ij_arms*producer_sender_list
                                +share_cust_ij_arms*producer_receiver_list
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
                                +embargo_j         # embargo j
                                +as.factor(time),   
                                data=data[which(data$time<1992),])


aic_model_pre_conf_excl_inter<-AIC(model_pre_conf_excl_inter)
bic_model_pre_conf_excl_inter<-BIC(model_pre_conf_excl_inter)
ll_model_pre_conf_excl_inter<-logLik(model_pre_conf_excl_inter)
nobs_model_pre_conf_excl_inter<-summary(model_pre_conf_excl_inter)$param$nObs
nobs2_model_pre_conf_excl_inter<-summary(model_pre_conf_excl_inter)$param$N1
margins_model_pre_conf_excl_inter=avg_margins(model_pre_conf_excl_inter,data[which(data$time<1992),])


# inter_intra_conf_j is the exclusion restriction -----
# with time dummies but without network statistics
model_pre_conf_excl_no_nw<- selection(response_arms     ~ 1 # Intercept
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
                                +embargo_j         # embargo j
                                +as.factor(time),
                                 response_arms_val   ~ # Intercept
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
                                +embargo_j         # embargo j
                                +as.factor(time),   
                                data=data[which(data$time<1992),])


aic_model_pre_conf_excl_no_nw<-AIC(model_pre_conf_excl_no_nw)
bic_model_pre_conf_excl_no_nw<-BIC(model_pre_conf_excl_no_nw)
ll_model_pre_conf_excl_no_nw<-logLik(model_pre_conf_excl_no_nw)
nobs_model_pre_conf_excl_no_nw<-summary(model_pre_conf_excl_no_nw)$param$nObs
nobs2_model_pre_conf_excl_no_nw<-summary(model_pre_conf_excl_no_nw)$param$N1


# the same model with the second stage as a simple regression model ----
model_pre_conf_excl_lm<- lm(response_arms_val   ~ 1# Intercept
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
                                +embargo_j         # embargo j
                                +as.factor(time),   
                                data=data[which((data$time<1992)&(data$response_arms_val!=0)),])

aic_model_pre_conf_excl_lm<-AIC(model_pre_conf_excl_lm)
bic_model_pre_conf_excl_lm<-BIC(model_pre_conf_excl_lm)
ll_model_pre_conf_excl_lm<-logLik(model_pre_conf_excl_lm)
nobs_model_pre_conf_excl_lm<-length(summary(model_pre_conf_excl_lm)$residuals)


# inter_intra_conf_j is the exclusion restriction ----
# with polynomial trend 
model_pre_conf_excl_poly<- selection(response_arms     ~ 1+ # Intercept
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
                                +time_reduce 
                                +timesq
                                +timecube,   
                                 response_arms_val   ~1+ # Intercept
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
                                +embargo_j         # embargo j
                                +time_reduce
                                +timesq
                                +timecube, 
                                data=data[which(data$time<1992),])


aic_model_pre_conf_excl_poly<-AIC(model_pre_conf_excl_poly)
bic_model_pre_conf_excl_poly<-BIC(model_pre_conf_excl_poly)
ll_model_pre_conf_excl_poly<-logLik(model_pre_conf_excl_poly)
nobs_model_pre_conf_excl_poly<-summary(model_pre_conf_excl_poly)$param$nObs
nobs2_model_pre_conf_excl_poly<-summary(model_pre_conf_excl_poly)$param$N1
margins_model_pre_conf_excl_poly=avg_margins(model_pre_conf_excl_poly,data[which(data$time<1992),],start=1,end=22)

library("stargazer")
stargazer(as.matrix(margins_model_pre_conf_excl_poly[1:23]),type='text')

# inter_intra_conf_j is the exclusion restriction ----
# with polynomial trend and interaction terms from the list
model_pre_conf_excl_poly_inter<- selection(response_arms     ~ 1+ # Intercept
                                     +producer_sender_list
                                     +producer_receiver_list
                                     
                                     +response_arms_t_1*producer_sender_list
                                     +response_arms_t_1*producer_receiver_list
                                     +response_arms_t_1 
                                     
                                     +recip_ji_arms*producer_sender_list
                                     +recip_ji_arms*producer_receiver_list
                                     +recip_ji_arms
                                     
                                     +outdeg_i_arms*producer_sender_list
                                     +outdeg_i_arms*producer_receiver_list
                                     +outdeg_i_arms
                                     
                                     +outdeg_j_arms*producer_sender_list
                                     +outdeg_j_arms*producer_receiver_list
                                     +outdeg_j_arms
                                     
                                     +indeg_i_arms*producer_sender_list
                                     +indeg_i_arms*producer_receiver_list
                                     +indeg_i_arms
                                     
                                     +indeg_j_arms*producer_sender_list
                                     +indeg_j_arms*producer_receiver_list
                                     +indeg_j_arms
                                     
                                     +trans_ij_arms*producer_sender_list
                                     +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                     +trans_ij_arms
                                     
                                     +revtrans_ij_arms*producer_sender_list
                                     +revtrans_ij_arms*producer_receiver_list
                                     +revtrans_ij_arms
                                     
                                     +share_sup_ij_arms*producer_sender_list
                                     +share_sup_ij_arms*producer_receiver_list
                                     +share_sup_ij_arms
                                     
                                     +share_cust_ij_arms*producer_sender_list
                                     +share_cust_ij_arms*producer_receiver_list
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
                                     +time_reduce
                                     +timesq
                                     +timecube,   
                                      response_arms_val   ~1+ # Intercept
                                     +producer_sender_list
                                     +producer_receiver_list
                                     
                                     +response_arms_t_1*producer_sender_list
                                     +response_arms_t_1*producer_receiver_list
                                     +response_arms_t_1 
                                     
                                     +recip_ji_arms*producer_sender_list
                                     +recip_ji_arms*producer_receiver_list
                                     +recip_ji_arms
                                     
                                     +outdeg_i_arms*producer_sender_list
                                     +outdeg_i_arms*producer_receiver_list
                                     +outdeg_i_arms
                                     
                                     +outdeg_j_arms*producer_sender_list
                                     +outdeg_j_arms*producer_receiver_list
                                     +outdeg_j_arms
                                     
                                     +indeg_i_arms*producer_sender_list
                                     +indeg_i_arms*producer_receiver_list
                                     +indeg_i_arms
                                     
                                     +indeg_j_arms*producer_sender_list
                                     +indeg_j_arms*producer_receiver_list
                                     +indeg_j_arms
                                     
                                     +trans_ij_arms*producer_sender_list
                                     +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                     +trans_ij_arms
                                     
                                     +revtrans_ij_arms*producer_sender_list
                                     +revtrans_ij_arms*producer_receiver_list
                                     +revtrans_ij_arms
                                     
                                     +share_sup_ij_arms*producer_sender_list
                                     +share_sup_ij_arms*producer_receiver_list
                                     +share_sup_ij_arms
                                     
                                     +share_cust_ij_arms*producer_sender_list
                                     +share_cust_ij_arms*producer_receiver_list
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
                                     +embargo_j         # embargo j
                                     +time_reduce
                                     + timesq
                                     +timecube, 
                                     data=data[which(data$time<1992),])


aic_model_pre_conf_excl_poly_inter<-AIC(model_pre_conf_excl_poly_inter)
bic_model_pre_conf_excl_poly_inter<-BIC(model_pre_conf_excl_poly_inter)
ll_model_pre_conf_excl_poly_inter<-logLik(model_pre_conf_excl_poly_inter)
nobs_model_pre_conf_excl_poly_inter<-summary(model_pre_conf_excl_poly_inter)$param$nObs
nobs2_model_pre_conf_excl_poly_inter<-summary(model_pre_conf_excl_poly_inter)$param$N1


# inter_intra_conf_j is the exclusion restriction ----
# with polynomial trend but without nw stats
model_pre_conf_excl_poly_no_nw<- selection(response_arms     ~ 1+ # Intercept
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
                                     +time_reduce
                                     +timesq
                                     +timecube,   
                                      response_arms_val   ~1+ # Intercept
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
                                     +embargo_j         # embargo j
                                     +time_reduce
                                     +timesq
                                     +timecube, 
                                     data=data[which(data$time<1992),])


aic_model_pre_conf_excl_poly_no_nw<-AIC(model_pre_conf_excl_poly_no_nw)
bic_model_pre_conf_excl_poly_no_nw<-BIC(model_pre_conf_excl_poly_no_nw)
ll_model_pre_conf_excl_poly_no_nw<-logLik(model_pre_conf_excl_poly_no_nw)
nobs_model_pre_conf_excl_poly_no_nw<-summary(model_pre_conf_excl_poly_no_nw)$param$nObs
nobs2_model_pre_conf_excl_poly_no_nw<-summary(model_pre_conf_excl_poly_no_nw)$param$N1


# the same model with the second stage as a simple regression model ----
model_pre_conf_excl_poly_lm<- lm(response_arms_val   ~1+ # Intercept
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
                                     +embargo_j         # embargo j
                                     +time_reduce
                                     +timesq
                                     +timecube, 
                                 data=data[which((data$time<1992)&(data$response_arms_val!=0)),])

aic_model_pre_conf_excl_poly_lm<-AIC(model_pre_conf_excl_poly_lm)
bic_model_pre_conf_excl_poly_lm<-BIC(model_pre_conf_excl_poly_lm)
ll_model_pre_conf_excl_poly_lm<-logLik(model_pre_conf_excl_poly_lm)
nobs_model_pre_conf_excl_poly_lm<-length(summary(model_pre_conf_excl_poly_lm)$residuals)


#-----------------------------------------------------------------------------------------------------------#
# Modelling: Post Cold War
#-----------------------------------------------------------------------------------------------------------#


# inter_intra_conf_j is the exclusion restriction ----
# with time dummies
model_post_conf_excl<- selection(response_arms     ~ 1 # Intercept
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
                                +embargo_j         # embargo j
                                +as.factor(time),   
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
                                +embargo_j         # embargo j
                                +as.factor(time),   
                                data=data[which(data$time>1991),])

aic_model_post_conf_excl<-AIC(model_post_conf_excl)
bic_model_post_conf_excl<-BIC(model_post_conf_excl)
ll_model_post_conf_excl<-logLik(model_post_conf_excl)
nobs_model_post_conf_excl<-summary(model_post_conf_excl)$param$nObs
nobs2_model_post_conf_excl<-summary(model_post_conf_excl)$param$N1
margins_model_post_conf_excl=avg_margins(model_post_conf_excl,data[which(data$time>1991),],start=1,end=23)

library("stargazer")
stargazer(as.matrix(margins_model_post_conf_excl[1:23]),type='text')



# inter_intra_conf_j is the exclusion restriction ----
# with time dummies and interactions from list
model_post_conf_excl_inter<- selection(response_arms     ~ 1 # Intercept
                                 +producer_sender_list
                                 +producer_receiver_list
                                 
                                 +response_arms_t_1*producer_sender_list
                                 +response_arms_t_1*producer_receiver_list
                                 +response_arms_t_1 
                                 
                                 +recip_ji_arms*producer_sender_list
                                 +recip_ji_arms*producer_receiver_list
                                 +recip_ji_arms
                                 
                                 +outdeg_i_arms*producer_sender_list
                                 +outdeg_i_arms*producer_receiver_list
                                 +outdeg_i_arms
                                 
                                 +outdeg_j_arms*producer_sender_list
                                 +outdeg_j_arms*producer_receiver_list
                                 +outdeg_j_arms
                                 
                                 +indeg_i_arms*producer_sender_list
                                 +indeg_i_arms*producer_receiver_list
                                 +indeg_i_arms
                                 
                                 +indeg_j_arms*producer_sender_list
                                 +indeg_j_arms*producer_receiver_list
                                 +indeg_j_arms
                                 
                                 +trans_ij_arms*producer_sender_list
                                 +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                 +trans_ij_arms
                                 
                                 +revtrans_ij_arms*producer_sender_list
                                 +revtrans_ij_arms*producer_receiver_list
                                 +revtrans_ij_arms
                                 
                                 +share_sup_ij_arms*producer_sender_list
                                 +share_sup_ij_arms*producer_receiver_list
                                 +share_sup_ij_arms
                                 
                                 +share_cust_ij_arms*producer_sender_list
                                 +share_cust_ij_arms*producer_receiver_list
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
                                 +embargo_j         # embargo j
                                 +as.factor(time),   
                                  response_arms_val   ~ # Intercept
                                 +producer_sender_list
                                 +producer_receiver_list
                                 
                                 +response_arms_t_1*producer_sender_list
                                 +response_arms_t_1*producer_receiver_list
                                 +response_arms_t_1 
                                 
                                 +recip_ji_arms*producer_sender_list
                                 +recip_ji_arms*producer_receiver_list
                                 +recip_ji_arms
                                 
                                 +outdeg_i_arms*producer_sender_list
                                 +outdeg_i_arms*producer_receiver_list
                                 +outdeg_i_arms
                                 
                                 +outdeg_j_arms*producer_sender_list
                                 +outdeg_j_arms*producer_receiver_list
                                 +outdeg_j_arms
                                 
                                 +indeg_i_arms*producer_sender_list
                                 +indeg_i_arms*producer_receiver_list
                                 +indeg_i_arms
                                 
                                 +indeg_j_arms*producer_sender_list
                                 +indeg_j_arms*producer_receiver_list
                                 +indeg_j_arms
                                 
                                 +trans_ij_arms*producer_sender_list
                                 +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                 +trans_ij_arms
                                 
                                 +revtrans_ij_arms*producer_sender_list
                                 +revtrans_ij_arms*producer_receiver_list
                                 +revtrans_ij_arms
                                 
                                 +share_sup_ij_arms*producer_sender_list
                                 +share_sup_ij_arms*producer_receiver_list
                                 +share_sup_ij_arms
                                 
                                 +share_cust_ij_arms*producer_sender_list
                                 +share_cust_ij_arms*producer_receiver_list
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
                                 +embargo_j         # embargo j
                                 +as.factor(time),   
                                 data=data[which(data$time>1991),])

aic_model_post_conf_excl_inter<-AIC(model_post_conf_excl_inter)
bic_model_post_conf_excl_inter<-BIC(model_post_conf_excl_inter)
ll_model_post_conf_excl_inter<-logLik(model_post_conf_excl_inter)
nobs_model_post_conf_excl_inter<-summary(model_post_conf_excl_inter)$param$nObs
nobs2_model_post_conf_excl_inter<-summary(model_post_conf_excl_inter)$param$N1


# inter_intra_conf_j is the exclusion restriction ----
# with time dummies but without nw stat
model_post_conf_excl_no_nw<- selection(response_arms     ~ 1 # Intercept
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
                                 +embargo_j         # embargo j
                                 +as.factor(time),   
                                 response_arms_val   ~ # Intercept
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
                                 +embargo_j         # embargo j
                                 +as.factor(time),   
                                 data=data[which(data$time>1991),])

aic_model_post_conf_excl_no_nw<-AIC(model_post_conf_excl_no_nw)
bic_model_post_conf_excl_no_nw<-BIC(model_post_conf_excl_no_nw)
ll_model_post_conf_excl_no_nw<-logLik(model_post_conf_excl_no_nw)
nobs_model_post_conf_excl_no_nw<-summary(model_post_conf_excl_no_nw)$param$nObs
nobs2_model_post_conf_excl_no_nw<-summary(model_post_conf_excl_no_nw)$param$N1


# the same model as a linear model ----
model_post_conf_excl_lm<- lm(    response_arms_val   ~ # Intercept
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
                                 +embargo_j         # embargo j
                                 +as.factor(time),   
                                 data=data[which((data$time>1991)&(data$response_arms_val!=0)),])


aic_model_post_conf_excl_lm<-AIC(model_post_conf_excl_lm)
bic_model_post_conf_excl_lm<-BIC(model_post_conf_excl_lm)
ll_model_post_conf_excl_lm<-logLik(model_post_conf_excl_lm)
nobs_model_post_conf_excl_lm<-length(summary(model_post_conf_excl_lm)$residuals)


# inter_intra_conf_j is the exclusion restriction ----
# with poly
model_post_conf_excl_poly<- selection(response_arms     ~ 1 # Intercept
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
                                   +embargo_j         # embargo j
                                   +time_reduce
                                   +timesq
                                   +timecube,    
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
                                   +embargo_j         # embargo j
                                   +time_reduce
                                   +timesq
                                   +timecube,   
                                   data=data[which(data$time>1991),])

aic_model_post_conf_excl_poly<-AIC(model_post_conf_excl_poly)
bic_model_post_conf_excl_poly<-BIC(model_post_conf_excl_poly)
ll_model_post_conf_excl_poly<-logLik(model_post_conf_excl_poly)
nobs_model_post_conf_excl_poly<-summary(model_post_conf_excl_poly)$param$nObs
nobs2_model_post_conf_excl_poly<-summary(model_post_conf_excl_poly)$param$N1

margins_model_post_conf_excl_poly=avg_margins(model_post_conf_excl_poly,data[which(data$time>1991),],start=1,end=23)

library("stargazer")
stargazer(as.matrix(margins_model_post_conf_excl_poly[1:23]),type='text')


AMPE = cbind(margins_model_pre_conf_excl_poly[1:23],margins_model_pre_conf_excl[1:23],margins_model_post_conf_excl_poly[1:23],margins_model_post_conf_excl[1:23])
colnames(AMPE)= c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")
stargazer(AMPE,type='text')

stargazer(AMPE,type='latex')



data_pre=data[which(data$time<1992),]
data_post=data[which(data$time>1991),]

#---- different scalling
# outdeg_i_arms
PDP_model_pre_conf_excl_poly_outdeg_i_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='outdeg_i_arms')
PDP_model_pre_conf_excl_outdeg_i_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='outdeg_i_arms')
PDP_model_post_conf_excl_poly_outdeg_i_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='outdeg_i_arms')
PDP_model_post_conf_excl_outdeg_i_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='outdeg_i_arms')


pdf("2) Modelling/pdp/pdp_outdeg_i.pdf",width = 12,height = 5)
par(mfrow=c(1,2))

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_outdeg_i_arms$out~PDP_model_pre_conf_excl_poly_outdeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_outdeg_i_arms$out),max(PDP_model_pre_conf_excl_poly_outdeg_i_arms$out)+0.001),lty=2,xlab='# Exporting Links (i)', ylab='Probability')
lines(PDP_model_pre_conf_excl_outdeg_i_arms$out~PDP_model_pre_conf_excl_outdeg_i_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_outdeg_i_arms$data[,colnames(PDP_model_pre_conf_excl_poly_outdeg_i_arms$data)==PDP_model_pre_conf_excl_poly_outdeg_i_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_outdeg_i_arms$out~PDP_model_post_conf_excl_poly_outdeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),xlim=c(0,max(PDP_model_post_conf_excl_poly_outdeg_i_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_i_arms$data)==PDP_model_post_conf_excl_poly_outdeg_i_arms$var])),ylim=c(min(PDP_model_post_conf_excl_poly_outdeg_i_arms$out),max(PDP_model_post_conf_excl_poly_outdeg_i_arms$out)+0.005),lty=2,xlab='# Exporting Links (i)', ylab='Probability')
lines(PDP_model_post_conf_excl_outdeg_i_arms$out~PDP_model_post_conf_excl_outdeg_i_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_outdeg_i_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_i_arms$data)==PDP_model_post_conf_excl_poly_outdeg_i_arms$var])
dev.off()


pdf("2) Modelling/pdp/pdp_outdeg_j.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
# outdeg_j_arms
PDP_model_pre_conf_excl_poly_outdeg_j_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='outdeg_j_arms')
PDP_model_pre_conf_excl_outdeg_j_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='outdeg_j_arms')
PDP_model_post_conf_excl_poly_outdeg_j_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='outdeg_j_arms')
PDP_model_post_conf_excl_outdeg_j_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='outdeg_j_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_outdeg_j_arms$out~PDP_model_pre_conf_excl_poly_outdeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_outdeg_j_arms$out),max(PDP_model_pre_conf_excl_poly_outdeg_j_arms$out)),lty=2,xlab='# Exporting Links (j)', ylab='Probability')
lines(PDP_model_pre_conf_excl_outdeg_j_arms$out~PDP_model_pre_conf_excl_outdeg_j_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_outdeg_j_arms$data[,colnames(PDP_model_pre_conf_excl_poly_outdeg_j_arms$data)==PDP_model_pre_conf_excl_poly_outdeg_j_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_outdeg_j_arms$out~PDP_model_post_conf_excl_poly_outdeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),xlim=c(0,max(PDP_model_post_conf_excl_poly_outdeg_j_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_j_arms$data)==PDP_model_post_conf_excl_poly_outdeg_j_arms$var])),ylim=c(min(PDP_model_post_conf_excl_poly_outdeg_j_arms$out),max(PDP_model_post_conf_excl_poly_outdeg_j_arms$out)),lty=2,xlab='# Exporting Links (j)', ylab='Probability')
lines(PDP_model_post_conf_excl_outdeg_j_arms$out~PDP_model_post_conf_excl_outdeg_j_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_outdeg_j_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_j_arms$data)==PDP_model_post_conf_excl_poly_outdeg_j_arms$var])

dev.off()


pdf("2) Modelling/pdp/pdp_indeg_i.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
# indeg_i_arms 
PDP_model_pre_conf_excl_poly_indeg_i_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='indeg_i_arms')
PDP_model_pre_conf_excl_indeg_i_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='indeg_i_arms')
PDP_model_post_conf_excl_poly_indeg_i_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='indeg_i_arms')
PDP_model_post_conf_excl_indeg_i_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='indeg_i_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_indeg_i_arms$out~PDP_model_pre_conf_excl_poly_indeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_indeg_i_arms$out),max(PDP_model_pre_conf_excl_poly_indeg_i_arms$out)+0.001),lty=2,xlab='# Importing Links (i)', ylab='Probability')
lines(PDP_model_pre_conf_excl_indeg_i_arms$out~PDP_model_pre_conf_excl_indeg_i_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_indeg_i_arms$data[,colnames(PDP_model_pre_conf_excl_poly_indeg_i_arms$data)==PDP_model_pre_conf_excl_poly_indeg_i_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_indeg_i_arms$out~PDP_model_post_conf_excl_poly_indeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(min(PDP_model_post_conf_excl_poly_indeg_i_arms$out),max(PDP_model_post_conf_excl_poly_indeg_i_arms$out)+0.005),lty=2,xlab='# Importing Links (i)', ylab='Probability')
lines(PDP_model_post_conf_excl_indeg_i_arms$out~PDP_model_post_conf_excl_indeg_i_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_indeg_i_arms$data[,colnames(PDP_model_post_conf_excl_poly_indeg_i_arms$data)==PDP_model_post_conf_excl_poly_indeg_i_arms$var])
dev.off()


pdf("2) Modelling/pdp/pdp_indeg_j.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
#indeg_j_arms 
PDP_model_pre_conf_excl_poly_indeg_j_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='indeg_j_arms')
PDP_model_pre_conf_excl_indeg_j_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='indeg_j_arms')
PDP_model_post_conf_excl_poly_indeg_j_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='indeg_j_arms')
PDP_model_post_conf_excl_indeg_j_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='indeg_j_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_indeg_j_arms$out~PDP_model_pre_conf_excl_poly_indeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_indeg_j_arms$out),max(PDP_model_pre_conf_excl_poly_indeg_j_arms$out)),lty=2,xlab='# Importing Links (j)', ylab='Probability')
lines(PDP_model_pre_conf_excl_indeg_j_arms$out~PDP_model_pre_conf_excl_indeg_j_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_indeg_j_arms$data[,colnames(PDP_model_pre_conf_excl_poly_indeg_j_arms$data)==PDP_model_pre_conf_excl_poly_indeg_j_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_indeg_j_arms$out~PDP_model_post_conf_excl_poly_indeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(min(PDP_model_post_conf_excl_poly_indeg_j_arms$out)-0.0005,max(PDP_model_post_conf_excl_poly_indeg_j_arms$out)+0.0015),lty=2,xlab='# Importing Links (j)', ylab='Probability')
lines(PDP_model_post_conf_excl_indeg_j_arms$out~PDP_model_post_conf_excl_indeg_j_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_indeg_j_arms$data[,colnames(PDP_model_post_conf_excl_poly_indeg_j_arms$data)==PDP_model_post_conf_excl_poly_indeg_j_arms$var])
dev.off()


pdf("2) Modelling/pdp/pdp_trans_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
#trans_ij_arms         
PDP_model_pre_conf_excl_poly_trans_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='trans_ij_arms')
PDP_model_pre_conf_excl_trans_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='trans_ij_arms')
PDP_model_post_conf_excl_poly_trans_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='trans_ij_arms')
PDP_model_post_conf_excl_trans_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='trans_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_trans_ij_arms$out~PDP_model_pre_conf_excl_poly_trans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_trans_ij_arms$out),max(PDP_model_pre_conf_excl_poly_trans_ij_arms$out)+0.001),lty=2,xlab='Exporting Transitivity', ylab='Probability')
lines(PDP_model_pre_conf_excl_trans_ij_arms$out~PDP_model_pre_conf_excl_trans_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_trans_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_trans_ij_arms$data)==PDP_model_pre_conf_excl_poly_trans_ij_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_trans_ij_arms$out~PDP_model_post_conf_excl_poly_trans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(min(PDP_model_post_conf_excl_poly_trans_ij_arms$out),max(PDP_model_post_conf_excl_poly_trans_ij_arms$out)+0.005),lty=2,xlab='Exporting Transitivity', ylab='Probability')
lines(PDP_model_post_conf_excl_trans_ij_arms$out~PDP_model_post_conf_excl_trans_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_trans_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_trans_ij_arms$data)==PDP_model_post_conf_excl_poly_trans_ij_arms$var])

dev.off()


pdf("2) Modelling/pdp/pdp_revtrans_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
#revtrans_ij_arms        
PDP_model_pre_conf_excl_poly_revtrans_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='revtrans_ij_arms')
PDP_model_pre_conf_excl_revtrans_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='revtrans_ij_arms')
PDP_model_post_conf_excl_poly_revtrans_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='revtrans_ij_arms')
PDP_model_post_conf_excl_revtrans_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='revtrans_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$out~PDP_model_pre_conf_excl_poly_revtrans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$out),max(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$out)),lty=2,xlab='Importing Cycle', ylab='Probability')
lines(PDP_model_pre_conf_excl_revtrans_ij_arms$out~PDP_model_pre_conf_excl_revtrans_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$data)==PDP_model_pre_conf_excl_poly_revtrans_ij_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_revtrans_ij_arms$out~PDP_model_post_conf_excl_poly_revtrans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(min(PDP_model_post_conf_excl_poly_revtrans_ij_arms$out),max(PDP_model_post_conf_excl_poly_revtrans_ij_arms$out)),lty=2,xlab='Importing Cycle', ylab='Probability')
lines(PDP_model_post_conf_excl_revtrans_ij_arms$out~PDP_model_post_conf_excl_revtrans_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_revtrans_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_revtrans_ij_arms$data)==PDP_model_post_conf_excl_poly_revtrans_ij_arms$var])

dev.off()


pdf("2) Modelling/pdp/pdp_share_sup_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))

#share_sup_ij_arms    
PDP_model_pre_conf_excl_poly_share_sup_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='share_sup_ij_arms')
PDP_model_pre_conf_excl_share_sup_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='share_sup_ij_arms')
PDP_model_post_conf_excl_poly_share_sup_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='share_sup_ij_arms')
PDP_model_post_conf_excl_share_sup_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='share_sup_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$out~PDP_model_pre_conf_excl_poly_share_sup_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$out),max(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$out)),lty=2,xlab='Shared Supplier', ylab='Probability')
lines(PDP_model_pre_conf_excl_share_sup_ij_arms$out~PDP_model_pre_conf_excl_share_sup_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$data)==PDP_model_pre_conf_excl_poly_share_sup_ij_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_share_sup_ij_arms$out~PDP_model_post_conf_excl_poly_share_sup_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(min(PDP_model_post_conf_excl_poly_share_sup_ij_arms$out),max(PDP_model_post_conf_excl_poly_share_sup_ij_arms$out)),lty=2,xlab='Shared Supplier', ylab='Probability')
lines(PDP_model_post_conf_excl_share_sup_ij_arms$out~PDP_model_post_conf_excl_share_sup_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_share_sup_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_share_sup_ij_arms$data)==PDP_model_post_conf_excl_poly_share_sup_ij_arms$var])
dev.off()

pdf("2) Modelling/pdp/pdp_share_cust_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))

#share_cust_ij_arms
PDP_model_pre_conf_excl_poly_share_cust_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='share_cust_ij_arms')
PDP_model_pre_conf_excl_share_cust_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='share_cust_ij_arms')
PDP_model_post_conf_excl_poly_share_cust_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='share_cust_ij_arms')
PDP_model_post_conf_excl_share_cust_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='share_cust_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$out~PDP_model_pre_conf_excl_poly_share_cust_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(min(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$out),max(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$out)),lty=2,xlab='Shared Customer', ylab='Probability')
lines(PDP_model_pre_conf_excl_share_cust_ij_arms$out~PDP_model_pre_conf_excl_share_cust_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$data)==PDP_model_pre_conf_excl_poly_share_cust_ij_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_share_cust_ij_arms$out~PDP_model_post_conf_excl_poly_share_cust_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(min(PDP_model_post_conf_excl_poly_share_cust_ij_arms$out),max(PDP_model_post_conf_excl_poly_share_cust_ij_arms$out)),lty=2,xlab='Shared Customer', ylab='Probability')
lines(PDP_model_post_conf_excl_share_cust_ij_arms$out~PDP_model_post_conf_excl_share_cust_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_share_cust_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_share_cust_ij_arms$data)==PDP_model_post_conf_excl_poly_share_cust_ij_arms$var])


dev.off()




# same scalling
# outdeg_i_arms
PDP_model_pre_conf_excl_poly_outdeg_i_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=5,var='outdeg_i_arms')
PDP_model_pre_conf_excl_outdeg_i_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=5,var='outdeg_i_arms')
PDP_model_post_conf_excl_poly_outdeg_i_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='outdeg_i_arms')
PDP_model_post_conf_excl_outdeg_i_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='outdeg_i_arms')


pdf("2) Modelling/pdp2/pdp_outdeg_i.pdf",width = 12,height = 5)
par(mfrow=c(1,2))

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_outdeg_i_arms$out~PDP_model_pre_conf_excl_poly_outdeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='# Exporting Links (i)', ylab='Probability')
lines(PDP_model_pre_conf_excl_outdeg_i_arms$out~PDP_model_pre_conf_excl_outdeg_i_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_outdeg_i_arms$data[,colnames(PDP_model_pre_conf_excl_poly_outdeg_i_arms$data)==PDP_model_pre_conf_excl_poly_outdeg_i_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_outdeg_i_arms$out~PDP_model_post_conf_excl_poly_outdeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),xlim=c(0,max(PDP_model_post_conf_excl_poly_outdeg_i_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_i_arms$data)==PDP_model_post_conf_excl_poly_outdeg_i_arms$var])),ylim=c(0.005,0.1),lty=2,xlab='# Exporting Links (i)', ylab='Probability')
lines(PDP_model_post_conf_excl_outdeg_i_arms$out~PDP_model_post_conf_excl_outdeg_i_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_outdeg_i_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_i_arms$data)==PDP_model_post_conf_excl_poly_outdeg_i_arms$var])
dev.off()


pdf("2) Modelling/pdp2/pdp_outdeg_j.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
# outdeg_j_arms
PDP_model_pre_conf_excl_poly_outdeg_j_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='outdeg_j_arms')
PDP_model_pre_conf_excl_outdeg_j_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='outdeg_j_arms')
PDP_model_post_conf_excl_poly_outdeg_j_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='outdeg_j_arms')
PDP_model_post_conf_excl_outdeg_j_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='outdeg_j_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_outdeg_j_arms$out~PDP_model_pre_conf_excl_poly_outdeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='# Exporting Links (j)', ylab='Probability')
lines(PDP_model_pre_conf_excl_outdeg_j_arms$out~PDP_model_pre_conf_excl_outdeg_j_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_outdeg_j_arms$data[,colnames(PDP_model_pre_conf_excl_poly_outdeg_j_arms$data)==PDP_model_pre_conf_excl_poly_outdeg_j_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_outdeg_j_arms$out~PDP_model_post_conf_excl_poly_outdeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),xlim=c(0,max(PDP_model_post_conf_excl_poly_outdeg_j_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_j_arms$data)==PDP_model_post_conf_excl_poly_outdeg_j_arms$var])),ylim=c(0.005,0.1),lty=2,xlab='# Exporting Links (j)', ylab='Probability')
lines(PDP_model_post_conf_excl_outdeg_j_arms$out~PDP_model_post_conf_excl_outdeg_j_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_outdeg_j_arms$data[,colnames(PDP_model_post_conf_excl_poly_outdeg_j_arms$data)==PDP_model_post_conf_excl_poly_outdeg_j_arms$var])

dev.off()


pdf("2) Modelling/pdp2/pdp_indeg_i.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
# indeg_i_arms 
PDP_model_pre_conf_excl_poly_indeg_i_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='indeg_i_arms')
PDP_model_pre_conf_excl_indeg_i_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='indeg_i_arms')
PDP_model_post_conf_excl_poly_indeg_i_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='indeg_i_arms')
PDP_model_post_conf_excl_indeg_i_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='indeg_i_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_indeg_i_arms$out~PDP_model_pre_conf_excl_poly_indeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='# Importing Links (i)', ylab='Probability')
lines(PDP_model_pre_conf_excl_indeg_i_arms$out~PDP_model_pre_conf_excl_indeg_i_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_indeg_i_arms$data[,colnames(PDP_model_pre_conf_excl_poly_indeg_i_arms$data)==PDP_model_pre_conf_excl_poly_indeg_i_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_indeg_i_arms$out~PDP_model_post_conf_excl_poly_indeg_i_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(0.005,0.1),lty=2,xlab='# Importing Links (i)', ylab='Probability')
lines(PDP_model_post_conf_excl_indeg_i_arms$out~PDP_model_post_conf_excl_indeg_i_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_indeg_i_arms$data[,colnames(PDP_model_post_conf_excl_poly_indeg_i_arms$data)==PDP_model_post_conf_excl_poly_indeg_i_arms$var])
dev.off()


pdf("2) Modelling/pdp2/pdp_indeg_j.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
#indeg_j_arms 
PDP_model_pre_conf_excl_poly_indeg_j_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='indeg_j_arms')
PDP_model_pre_conf_excl_indeg_j_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='indeg_j_arms')
PDP_model_post_conf_excl_poly_indeg_j_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='indeg_j_arms')
PDP_model_post_conf_excl_indeg_j_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='indeg_j_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_indeg_j_arms$out~PDP_model_pre_conf_excl_poly_indeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='# Importing Links (j)', ylab='Probability')
lines(PDP_model_pre_conf_excl_indeg_j_arms$out~PDP_model_pre_conf_excl_indeg_j_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_indeg_j_arms$data[,colnames(PDP_model_pre_conf_excl_poly_indeg_j_arms$data)==PDP_model_pre_conf_excl_poly_indeg_j_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_indeg_j_arms$out~PDP_model_post_conf_excl_poly_indeg_j_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(0.005,0.1),lty=2,xlab='# Importing Links (j)', ylab='Probability')
lines(PDP_model_post_conf_excl_indeg_j_arms$out~PDP_model_post_conf_excl_indeg_j_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_indeg_j_arms$data[,colnames(PDP_model_post_conf_excl_poly_indeg_j_arms$data)==PDP_model_post_conf_excl_poly_indeg_j_arms$var])
dev.off()


pdf("2) Modelling/pdp2/pdp_trans_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
#trans_ij_arms         
PDP_model_pre_conf_excl_poly_trans_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='trans_ij_arms')
PDP_model_pre_conf_excl_trans_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='trans_ij_arms')
PDP_model_post_conf_excl_poly_trans_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='trans_ij_arms')
PDP_model_post_conf_excl_trans_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='trans_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_trans_ij_arms$out~PDP_model_pre_conf_excl_poly_trans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='Exporting Transitivity', ylab='Probability')
lines(PDP_model_pre_conf_excl_trans_ij_arms$out~PDP_model_pre_conf_excl_trans_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_trans_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_trans_ij_arms$data)==PDP_model_pre_conf_excl_poly_trans_ij_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_trans_ij_arms$out~PDP_model_post_conf_excl_poly_trans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(0.005,0.1),lty=2,xlab='Exporting Transitivity', ylab='Probability')
lines(PDP_model_post_conf_excl_trans_ij_arms$out~PDP_model_post_conf_excl_trans_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_trans_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_trans_ij_arms$data)==PDP_model_post_conf_excl_poly_trans_ij_arms$var])

dev.off()


pdf("2) Modelling/pdp2/pdp_revtrans_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
#revtrans_ij_arms        
PDP_model_pre_conf_excl_poly_revtrans_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='revtrans_ij_arms')
PDP_model_pre_conf_excl_revtrans_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='revtrans_ij_arms')
PDP_model_post_conf_excl_poly_revtrans_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='revtrans_ij_arms')
PDP_model_post_conf_excl_revtrans_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='revtrans_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$out~PDP_model_pre_conf_excl_poly_revtrans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='Importing Cycle', ylab='Probability')
lines(PDP_model_pre_conf_excl_revtrans_ij_arms$out~PDP_model_pre_conf_excl_revtrans_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_revtrans_ij_arms$data)==PDP_model_pre_conf_excl_poly_revtrans_ij_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_revtrans_ij_arms$out~PDP_model_post_conf_excl_poly_revtrans_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(0.005,0.1),lty=2,xlab='Importing Cycle', ylab='Probability')
lines(PDP_model_post_conf_excl_revtrans_ij_arms$out~PDP_model_post_conf_excl_revtrans_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_revtrans_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_revtrans_ij_arms$data)==PDP_model_post_conf_excl_poly_revtrans_ij_arms$var])

dev.off()


pdf("2) Modelling/pdp2/pdp_share_sup_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))

#share_sup_ij_arms    
PDP_model_pre_conf_excl_poly_share_sup_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='share_sup_ij_arms')
PDP_model_pre_conf_excl_share_sup_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='share_sup_ij_arms')
PDP_model_post_conf_excl_poly_share_sup_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='share_sup_ij_arms')
PDP_model_post_conf_excl_share_sup_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='share_sup_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$out~PDP_model_pre_conf_excl_poly_share_sup_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='Shared Supplier', ylab='Probability')
lines(PDP_model_pre_conf_excl_share_sup_ij_arms$out~PDP_model_pre_conf_excl_share_sup_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_share_sup_ij_arms$data)==PDP_model_pre_conf_excl_poly_share_sup_ij_arms$var])

# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_share_sup_ij_arms$out~PDP_model_post_conf_excl_poly_share_sup_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(0.005,0.1),lty=2,xlab='Shared Supplier', ylab='Probability')
lines(PDP_model_post_conf_excl_share_sup_ij_arms$out~PDP_model_post_conf_excl_share_sup_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_share_sup_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_share_sup_ij_arms$data)==PDP_model_post_conf_excl_poly_share_sup_ij_arms$var])
dev.off()

pdf("2) Modelling/pdp2/pdp_share_cust_ij.pdf",width = 12,height = 5)
par(mfrow=c(1,2))

#share_cust_ij_arms
PDP_model_pre_conf_excl_poly_share_cust_ij_arms = partial_dependence(model=model_pre_conf_excl_poly, data=data_pre,spacing=1,var='share_cust_ij_arms')
PDP_model_pre_conf_excl_share_cust_ij_arms = partial_dependence(model=model_pre_conf_excl, data=data_pre,spacing=1,var='share_cust_ij_arms')
PDP_model_post_conf_excl_poly_share_cust_ij_arms = partial_dependence(model=model_post_conf_excl_poly, data=data_post,spacing=1,var='share_cust_ij_arms')
PDP_model_post_conf_excl_share_cust_ij_arms = partial_dependence(model=model_post_conf_excl, data=data_post,spacing=1,var='share_cust_ij_arms')

# dashed is always poly!
plot(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$out~PDP_model_pre_conf_excl_poly_share_cust_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1955-1991'),ylim=c(0.005,0.1),lty=2,xlab='Shared Customer', ylab='Probability')
lines(PDP_model_pre_conf_excl_share_cust_ij_arms$out~PDP_model_pre_conf_excl_share_cust_ij_arms$seq_var,lty=1)
rug(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$data[,colnames(PDP_model_pre_conf_excl_poly_share_cust_ij_arms$data)==PDP_model_pre_conf_excl_poly_share_cust_ij_arms$var])


# dashed is always poly!
plot(PDP_model_post_conf_excl_poly_share_cust_ij_arms$out~PDP_model_post_conf_excl_poly_share_cust_ij_arms$seq_var,type='l',main=paste('Average Partial Probability Effect: 1992-2018'),ylim=c(0.005,0.1),lty=2,xlab='Shared Customer', ylab='Probability')
lines(PDP_model_post_conf_excl_share_cust_ij_arms$out~PDP_model_post_conf_excl_share_cust_ij_arms$seq_var,lty=1)
rug(PDP_model_post_conf_excl_poly_share_cust_ij_arms$data[,colnames(PDP_model_post_conf_excl_poly_share_cust_ij_arms$data)==PDP_model_post_conf_excl_poly_share_cust_ij_arms$var])


dev.off()





# Pre Cold war,FE
#  quantile effects

m_=model_pre_conf_excl
Q_s1_model_pre_conf_excl=c()


s_1start=2
s1_end=23
dat=data_pre

for (i in seq(s_1start,s1_end,1)){
  Q_s1_model_pre_conf_excl=rbind(Q_s1_model_pre_conf_excl,avg_margins_quantiles(m_,dat,start=i, end=i))
}


rownames(Q_s1_model_pre_conf_excl)=names(m_$estimate)[s_1start:s1_end]
colnames(Q_s1_model_pre_conf_excl)=c('0','0.25','0.5','0.75','1')


stargazer(Q_s1_model_pre_conf_excl)

Q_s2_model_pre_conf_excl=c()
s_2start=61
s2_end=81
for (i in seq(s_2start,s2_end,1)){
  Q_s2_model_pre_conf_excl=rbind(Q_s2_model_pre_conf_excl,margins_quantiles(m_,dat,start=i, end=i))
}

rownames(Q_s2_model_pre_conf_excl)=names(m_$estimate)[s_2start:s2_end]
colnames(Q_s2_model_pre_conf_excl)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s2_model_pre_conf_excl)


# Pre Cold war,poly
#  quantile effects

m_=model_pre_conf_excl_poly
Q_s1_model_pre_conf_excl_poly=c()


s_1start=2
s1_end=23
dat=data_pre

for (i in seq(s_1start,s1_end,1)){
  Q_s1_model_pre_conf_excl_poly=rbind(Q_s1_model_pre_conf_excl_poly,avg_margins_quantiles(m_,dat,start=i, end=i))
}


rownames(Q_s1_model_pre_conf_excl_poly)=names(m_$estimate)[s_1start:s1_end]
colnames(Q_s1_model_pre_conf_excl_poly)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s1_model_pre_conf_excl_poly)


Q_s2_model_pre_conf_excl_poly=c()
s_2start=28
s2_end=48
for (i in seq(s_2start,s2_end,1)){
  Q_s2_model_pre_conf_excl_poly=rbind(Q_s2_model_pre_conf_excl_poly,margins_quantiles(m_,dat,start=i, end=i))
}

rownames(Q_s2_model_pre_conf_excl_poly)=names(m_$estimate)[s_2start:s2_end]
colnames(Q_s2_model_pre_conf_excl_poly)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s2_model_pre_conf_excl_poly)


# Post Cold war,FE
#  quantile effects

m_=model_post_conf_excl
Q_s1_model_post_conf_excl=c()


s_1start=2
s1_end=23
dat=data_post

for (i in seq(s_1start,s1_end,1)){
  Q_s1_model_post_conf_excl=rbind(Q_s1_model_post_conf_excl,avg_margins_quantiles(m_,dat,start=i, end=i))
}


rownames(Q_s1_model_post_conf_excl)=names(m_$estimate)[s_1start:s1_end]
colnames(Q_s1_model_post_conf_excl)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s1_model_post_conf_excl)


Q_s2_model_post_conf_excl=c()
s_2start=51
s2_end=71
for (i in seq(s_2start,s2_end,1)){
  Q_s2_model_post_conf_excl=rbind(Q_s2_model_post_conf_excl,margins_quantiles(m_,dat,start=i, end=i))
}

rownames(Q_s2_model_post_conf_excl)=names(m_$estimate)[s_2start:s2_end]
colnames(Q_s2_model_post_conf_excl)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s2_model_post_conf_excl)

# Pre Cold war,poly
#  quantile effects

m_=model_post_conf_excl_poly
Q_s1_model_post_conf_excl_poly=c()


s_1start=2
s1_end=23
dat=data_post

for (i in seq(s_1start,s1_end,1)){
  Q_s1_model_post_conf_excl_poly=rbind(Q_s1_model_post_conf_excl_poly,avg_margins_quantiles(m_,dat,start=i, end=i))
}


rownames(Q_s1_model_post_conf_excl_poly)=names(m_$estimate)[s_1start:s1_end]
colnames(Q_s1_model_post_conf_excl_poly)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s1_model_post_conf_excl_poly)

Q_s2_model_post_conf_excl_poly=c()
s_2start=28
s2_end=48
for (i in seq(s_2start,s2_end,1)){
  Q_s2_model_post_conf_excl_poly=rbind(Q_s2_model_post_conf_excl_poly,margins_quantiles(m_,dat,start=i, end=i))
}

rownames(Q_s2_model_post_conf_excl_poly)=names(m_$estimate)[s_2start:s2_end]
colnames(Q_s2_model_post_conf_excl_poly)=c('0','0.25','0.5','0.75','1')

stargazer(Q_s2_model_post_conf_excl_poly)
























# inter_intra_conf_j is the exclusion restriction ----
# with poly and interactions from list
model_post_conf_excl_poly_inter<- selection(response_arms     ~ 1 # Intercept
                                      +producer_sender_list
                                      +producer_receiver_list
                                      
                                      +response_arms_t_1*producer_sender_list
                                      +response_arms_t_1*producer_receiver_list
                                      +response_arms_t_1 
                                      
                                      +recip_ji_arms*producer_sender_list
                                      +recip_ji_arms*producer_receiver_list
                                      +recip_ji_arms
                                      
                                      +outdeg_i_arms*producer_sender_list
                                      +outdeg_i_arms*producer_receiver_list
                                      +outdeg_i_arms
                                      
                                      +outdeg_j_arms*producer_sender_list
                                      +outdeg_j_arms*producer_receiver_list
                                      +outdeg_j_arms
                                      
                                      +indeg_i_arms*producer_sender_list
                                      +indeg_i_arms*producer_receiver_list
                                      +indeg_i_arms
                                      
                                      +indeg_j_arms*producer_sender_list
                                      +indeg_j_arms*producer_receiver_list
                                      +indeg_j_arms
                                      
                                      +trans_ij_arms*producer_sender_list
                                      +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                      +trans_ij_arms
                                      
                                      +revtrans_ij_arms*producer_sender_list
                                      +revtrans_ij_arms*producer_receiver_list
                                      +revtrans_ij_arms
                                      
                                      +share_sup_ij_arms*producer_sender_list
                                      +share_sup_ij_arms*producer_receiver_list
                                      +share_sup_ij_arms
                                      
                                      +share_cust_ij_arms*producer_sender_list
                                      +share_cust_ij_arms*producer_receiver_list
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
                                      +embargo_j         # embargo j
                                      +time_reduce
                                      +timesq
                                      +timecube,    
                                       response_arms_val   ~ # Intercept
                                      +producer_sender_list
                                      +producer_receiver_list
                                      
                                      +response_arms_t_1*producer_sender_list
                                      +response_arms_t_1*producer_receiver_list
                                      +response_arms_t_1 
                                      
                                      +recip_ji_arms*producer_sender_list
                                      +recip_ji_arms*producer_receiver_list
                                      +recip_ji_arms
                                      
                                      +outdeg_i_arms*producer_sender_list
                                      +outdeg_i_arms*producer_receiver_list
                                      +outdeg_i_arms
                                      
                                      +outdeg_j_arms*producer_sender_list
                                      +outdeg_j_arms*producer_receiver_list
                                      +outdeg_j_arms
                                      
                                      +indeg_i_arms*producer_sender_list
                                      +indeg_i_arms*producer_receiver_list
                                      +indeg_i_arms
                                      
                                      +indeg_j_arms*producer_sender_list
                                      +indeg_j_arms*producer_receiver_list
                                      +indeg_j_arms
                                      
                                      +trans_ij_arms*producer_sender_list
                                      +trans_ij_arms*producer_receiver_list# Arms Network Triadic
                                      +trans_ij_arms
                                      
                                      +revtrans_ij_arms*producer_sender_list
                                      +revtrans_ij_arms*producer_receiver_list
                                      +revtrans_ij_arms
                                      
                                      +share_sup_ij_arms*producer_sender_list
                                      +share_sup_ij_arms*producer_receiver_list
                                      +share_sup_ij_arms
                                      
                                      +share_cust_ij_arms*producer_sender_list
                                      +share_cust_ij_arms*producer_receiver_list
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
                                      +embargo_j         # embargo j
                                      + time_reduce + timesq+timecube,   
                                      data=data[which(data$time>1991),])

aic_model_post_conf_excl_poly_inter<-AIC(model_post_conf_excl_poly_inter)
bic_model_post_conf_excl_poly_inter<-BIC(model_post_conf_excl_poly_inter)
ll_model_post_conf_excl_poly_inter<-logLik(model_post_conf_excl_poly_inter)
nobs_model_post_conf_excl_poly_inter<-summary(model_post_conf_excl_poly_inter)$param$nObs
nobs2_model_post_conf_excl_poly_inter<-summary(model_post_conf_excl_poly_inter)$param$N1


# inter_intra_conf_j is the exclusion restriction ----
# with poly but without nw stat
model_post_conf_excl_poly_no_nw<- selection(response_arms     ~ 1 # Intercept
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
                                      +embargo_j         # embargo j
                                      + time_reduce + timesq+timecube,    
                                       response_arms_val   ~ # Intercept
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
                                      +embargo_j         # embargo j
                                      +time_reduce
                                      +timesq
                                      +timecube,   
                                      data=data[which(data$time>1991),])

aic_model_post_conf_excl_poly_no_nw<-AIC(model_post_conf_excl_poly_no_nw)
bic_model_post_conf_excl_poly_no_nw<-BIC(model_post_conf_excl_poly_no_nw)
ll_model_post_conf_excl_poly_no_nw<-logLik(model_post_conf_excl_poly_no_nw)
nobs_model_post_conf_excl_poly_no_nw<-summary(model_post_conf_excl_poly_no_nw)$param$nObs
nobs2_model_post_conf_excl_poly_no_nw<-summary(model_post_conf_excl_poly_no_nw)$param$N1


# with a linear model
model_post_conf_excl_poly_lm<- lm(response_arms_val   ~ # Intercept
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
                                      +embargo_j         # embargo j
                                      +time_reduce
                                      +timesq
                                      +timecube,   
                                      data=data[which((data$time>1991)&(data$response_arms_val!=0)),])

aic_model_post_conf_excl_poly_lm<-AIC(model_post_conf_excl_poly_lm)
bic_model_post_conf_excl_poly_lm<-BIC(model_post_conf_excl_poly_lm)
ll_model_post_conf_excl_poly_lm<-logLik(model_post_conf_excl_poly_lm)
nobs_model_post_conf_excl_poly_lm<-length(summary(model_post_conf_excl_poly_lm)$residuals)


save.image("2) Modelling/results.RData")
