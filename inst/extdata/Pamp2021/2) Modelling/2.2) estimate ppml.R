# remove the old stuff
rm(list=ls())

# load some libraries 
library("sampleSelection")
library("stargazer")
library("coefplot")
library('gravity')
library('texreg')
memory.limit(size=11756000)

# load the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
load("1) Data/data_ready.RData")


data<-as.data.frame(data)
data[, -c(which(colnames(data)=="sender_id"),which(colnames(data)=="receiver_id"))  ]<- lapply(data[,   -c(which(colnames(data)=="sender_id"),which(colnames(data)=="receiver_id")) ], function(x) as.numeric(as.character(x)))



## Data Transformation----

# Distance in Logs
data[,which(colnames(data)=="distance_ij")]<-exp(data[,which(colnames(data)=="distance_ij")])

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

data$response_val_level = exp(data$response_arms_val)
data$response_val_level[data$response_val_level==1]=0
data$time_Fac = as.factor(data$time)


data$sender_t <- as.factor(paste(data$sender_id,data$time_Fac, sep="_"))
data$receiver_t<- as.factor(paste(data$receiver_id,data$time_Fac, sep="_"))



# ppml pre ----

fit_pre <- ppml(
  dependent_variable = "response_val_level",
  distance = "distance_ij",
  additional_regressors = c("response_arms_t_1",
                            "recip_ji_arms",
                            "outdeg_i_arms",
                            "outdeg_j_arms",
                            "indeg_i_arms",
                            "indeg_j_arms", 
                            "trans_ij_arms",        # Arms Network Triadic
                            "revtrans_ij_arms",
                            "share_sup_ij_arms",
                            "share_cust_ij_arms",
                            "path_dependence",      # Path Dependence
                            "alliance",             # Defence Agreement
                            "lgdp_i",               # log GDP_i
                            "lgdp_j",               # log GDP_j
                            "poldiff_ij",           # absolute Difference Polity Score
                            "LMilex_i",             # log Milex_i
                            "LMilex_j",             # log Milex_j
                            "Pop_i",                # log Population_i
                            "Pop_j",                # log Population_j
                          #  "distance_ij",          # log distance ij
                            "inter_intraconf_j",         # intrastate conflict j
                            "embargo_j",         # embargo j
                            "sender_id",
                            "receiver_id",
                            "time_Fac"),
  data = data[which(data$time<1992),]
)


# ppml post ----

fit_post <- ppml(
  dependent_variable = "response_val_level",
  distance = "distance_ij",
  additional_regressors = c("response_arms_t_1",
                            "recip_ji_arms",
                            "outdeg_i_arms",
                            "outdeg_j_arms",
                            "indeg_i_arms",
                            "indeg_j_arms", 
                            "trans_ij_arms",        # Arms Network Triadic
                            "revtrans_ij_arms",
                            "share_sup_ij_arms",
                            "share_cust_ij_arms",
                            "path_dependence",      # Path Dependence
                            "alliance",             # Defence Agreement
                            "lgdp_i",               # log GDP_i
                            "lgdp_j",               # log GDP_j
                            "poldiff_ij",           # absolute Difference Polity Score
                            "LMilex_i",             # log Milex_i
                            "LMilex_j",             # log Milex_j
                            "Pop_i",                # log Population_i
                            "Pop_j",                # log Population_j
                           # "distance_ij",          # log distance ij
                            "inter_intraconf_j",         # intrastate conflict j
                            "embargo_j",         # embargo j
                            "sender_id",
                            "receiver_id",
                            "time_Fac"),
  data = data[which(data$time>1991),]
)


save.image("2) Modelling/results_ppml.RData")



#### original model ----

fit_pre_est<-summary(fit_pre)$coef
fit_pre_est<-fit_pre_est[-c(24:336),]


fit_post_est<-summary(fit_post)$coef
fit_post_est<-fit_post_est[-c(24:380),]


p_val<-list()
p_val[[1]]<-fit_pre_est[,4]
p_val[[2]]<-fit_post_est[,4]

lgof<-list()
lgof[[1]] <- c(dim(data[which(data$time<1992),])[1])
lgof[[2]] <- c(dim(data[which(data$time>1991),])[1])

gof.names <- c( "Num. obs.") #names of GOFs

est <- cbind(fit_pre_est[,1],fit_post_est[,1])
colnames(est) <- c("Pre (Time, Sender and Receiver fixed eff.)","Post (Time, Sender and Receiver fixed eff.)")
se <- cbind(fit_pre_est[,2],fit_post_est[,2])

# add row labels:
rownames(est) <- rownames(fit_pre_est)

# create a texreg object:
tr <- list()
for (j in 1:ncol(est)) {
  tr[[j]] <- createTexreg(
    coef.names = rownames(est), 
    coef = est[, j], 
    se = se[, j],
    pvalues = p_val[[j]],
    ci.low = numeric(0),
    ci.up = numeric(0),
    gof.names <- c("Num. obs."),
    gof =lgof[[j]]
    
  )
}

# for text output:
screenreg(tr, custom.model.names = colnames(est), 
          custom.note = "",stars = c(0.001,  0.01, 0.05),gof=lgof)

# for LaTeX output:
texreg(tr, custom.model.names = colnames(est),
       custom.note = "")



