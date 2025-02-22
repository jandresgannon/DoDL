##### Create tables with results


# remove the old stuff
rm(list=ls())
# load packages
library("texreg")
memory.limit(size=5600000)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
load("2) Modelling/results.RData")

#### original model ----

estimates_pre_conf_excl_poly<-summary(model_pre_conf_excl_poly)$estimate
estimates_pre_conf_excl_poly<-estimates_pre_conf_excl_poly[-c(which(rownames(estimates_pre_conf_excl_poly)=="time_reduce"),which(rownames(estimates_pre_conf_excl_poly)=="timesq"),which(rownames(estimates_pre_conf_excl_poly)=="timecube")),]
estimates_pre_conf_excl<-summary(model_pre_conf_excl)$estimate
estimates_pre_conf_excl<-estimates_pre_conf_excl[-c(24:59,82:117),]

estimates_post_conf_excl_poly<-summary(model_post_conf_excl_poly)$estimate
estimates_post_conf_excl_poly<-estimates_post_conf_excl_poly[-c(which(rownames(estimates_post_conf_excl_poly)=="time_reduce"),which(rownames(estimates_post_conf_excl_poly)=="timesq"),which(rownames(estimates_post_conf_excl_poly)=="timecube")),]
estimates_post_conf_excl<-summary(model_post_conf_excl)$estimate
estimates_post_conf_excl<-estimates_post_conf_excl[-c(24:49,72:97),]


p_val<-list()
p_val[[1]]<-estimates_pre_conf_excl_poly[,4]
p_val[[2]]<-estimates_pre_conf_excl[,4]
p_val[[3]]<-estimates_post_conf_excl_poly[,4]
p_val[[4]]<-estimates_post_conf_excl[,4]

lgof<-list()
lgof[[1]] <- c(aic_model_pre_conf_excl_poly,bic_model_pre_conf_excl,ll_model_pre_conf_excl,nobs_model_pre_conf_excl,nobs2_model_pre_conf_excl)
lgof[[2]] <- c(aic_model_pre_conf_excl,bic_model_pre_conf_excl_poly,ll_model_pre_conf_excl_poly,nobs_model_pre_conf_excl_poly,nobs2_model_pre_conf_excl_poly)
lgof[[3]] <- c(aic_model_post_conf_excl_poly,bic_model_post_conf_excl,ll_model_post_conf_excl,nobs_model_post_conf_excl,nobs2_model_post_conf_excl)
lgof[[4]] <- c(aic_model_post_conf_excl,bic_model_post_conf_excl_poly,ll_model_post_conf_excl_poly,nobs_model_post_conf_excl_poly,nobs2_model_post_conf_excl_poly)

gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.", "Num. obs. second stage") #names of GOFs

est <- cbind(estimates_pre_conf_excl_poly[,1],estimates_pre_conf_excl[,1],estimates_post_conf_excl_poly[,1],estimates_post_conf_excl[,1])
colnames(est) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")
se <- cbind(estimates_pre_conf_excl_poly[,2],estimates_pre_conf_excl[,2],estimates_post_conf_excl_poly[,2],estimates_post_conf_excl[,2])
colnames(est) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")

# add row labels:
rownames(est) <- rownames(estimates_pre_conf_excl)

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
    gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.", "Num. obs. second stage"),
    gof =lgof[[j]]

  )
}

# for text output:
screenreg(tr, custom.model.names = colnames(est), 
          custom.note = "",stars = c(0.001,  0.01, 0.05),gof=lgof)

# for LaTeX output:
texreg(tr, custom.model.names = colnames(est),
       custom.note = "")




#### model with interaction terms ----

estimates_pre_conf_excl_poly_inter<-summary(model_pre_conf_excl_poly_inter)$estimate
estimates_pre_conf_excl_poly_inter<-estimates_pre_conf_excl_poly_inter[-c(which(rownames(estimates_pre_conf_excl_poly_inter)=="time_reduce"),which(rownames(estimates_pre_conf_excl_poly_inter)=="timesq"),which(rownames(estimates_pre_conf_excl_poly_inter)=="timecube")),]
estimates_pre_conf_excl_inter<-summary(model_pre_conf_excl_inter)$estimate
estimates_pre_conf_excl_inter<-estimates_pre_conf_excl_inter[-c(26:61,106:141),]

estimates_post_conf_excl_poly_inter<-summary(model_post_conf_excl_poly_inter)$estimate
estimates_post_conf_excl_poly_inter<-estimates_post_conf_excl_poly_inter[-c(which(rownames(estimates_post_conf_excl_poly_inter)=="time_reduce"),which(rownames(estimates_post_conf_excl_poly_inter)=="timesq"),which(rownames(estimates_post_conf_excl_poly_inter)=="timecube")),]
estimates_post_conf_excl_inter<-summary(model_post_conf_excl_inter)$estimate
estimates_post_conf_excl_inter<-estimates_post_conf_excl_inter[-c(26:51,96:121),]


p_val_inter<-list()
p_val_inter[[1]]<-estimates_pre_conf_excl_poly_inter[,4]
p_val_inter[[2]]<-estimates_pre_conf_excl_inter[,4]
p_val_inter[[3]]<-estimates_post_conf_excl_poly_inter[,4]
p_val_inter[[4]]<-estimates_post_conf_excl_inter[,4]

lgof_inter<-list()
lgof_inter[[1]] <- c(aic_model_pre_conf_excl_poly_inter,bic_model_pre_conf_excl_inter,ll_model_pre_conf_excl_inter,nobs_model_pre_conf_excl_inter,nobs2_model_pre_conf_excl_inter)
lgof_inter[[2]] <- c(aic_model_pre_conf_excl_inter,bic_model_pre_conf_excl_poly_inter,ll_model_pre_conf_excl_poly_inter,nobs_model_pre_conf_excl_poly_inter,nobs2_model_pre_conf_excl_poly_inter)
lgof_inter[[3]] <- c(aic_model_post_conf_excl_poly_inter,bic_model_post_conf_excl_inter,ll_model_post_conf_excl_inter,nobs_model_post_conf_excl_inter,nobs2_model_post_conf_excl_inter)
lgof_inter[[4]] <- c(aic_model_post_conf_excl_inter,bic_model_post_conf_excl_poly_inter,ll_model_post_conf_excl_poly_inter,nobs_model_post_conf_excl_poly_inter,nobs2_model_post_conf_excl_poly_inter)

gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.", "Num. obs. second stage") #names of GOFs

est_inter <- cbind(estimates_pre_conf_excl_poly_inter[,1],estimates_pre_conf_excl_inter[,1],estimates_post_conf_excl_poly_inter[,1],estimates_post_conf_excl_inter[,1])
colnames(est_inter) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")
se_inter <- cbind(estimates_pre_conf_excl_poly_inter[,2],estimates_pre_conf_excl_inter[,2],estimates_post_conf_excl_poly_inter[,2],estimates_post_conf_excl_inter[,2])
colnames(est_inter) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")

# add row labels:
rownames(est_inter) <- rownames(estimates_pre_conf_excl_inter)

# create a texreg object:
tr_inter <- list()
for (j in 1:ncol(est_inter)) {
  tr_inter[[j]] <- createTexreg(
    coef.names = rownames(est_inter), 
    coef = est_inter[, j], 
    se = se_inter[, j],
    pvalues = p_val_inter[[j]],
    ci.low = numeric(0),
    ci.up = numeric(0),
    gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.", "Num. obs. second stage"),
    gof =lgof_inter[[j]]
    
  )
}

# for text output:
screenreg(tr_inter, custom.model.names = colnames(est_inter), 
          custom.note = "",stars = c(0.001,  0.01, 0.05),gof=lgof_inter)

# for LaTeX output:
texreg(tr_inter, custom.model.names = colnames(est_inter),
       custom.note = "")


#### model without nw stats ----

estimates_pre_conf_excl_poly_no_nw<-summary(model_pre_conf_excl_poly_no_nw)$estimate
estimates_pre_conf_excl_poly_no_nw<-estimates_pre_conf_excl_poly_no_nw[-c(which(rownames(estimates_pre_conf_excl_poly_no_nw)=="time_reduce"),which(rownames(estimates_pre_conf_excl_poly_no_nw)=="timesq"),which(rownames(estimates_pre_conf_excl_poly_no_nw)=="timecube")),]
estimates_pre_conf_excl_no_nw<-summary(model_pre_conf_excl_no_nw)$estimate
estimates_pre_conf_excl_no_nw<-estimates_pre_conf_excl_no_nw[-c(14:49,62:97),]

estimates_post_conf_excl_poly_no_nw<-summary(model_post_conf_excl_poly_no_nw)$estimate
estimates_post_conf_excl_poly_no_nw<-estimates_post_conf_excl_poly_no_nw[-c(which(rownames(estimates_post_conf_excl_poly_no_nw)=="time_reduce"),which(rownames(estimates_post_conf_excl_poly_no_nw)=="timesq"),which(rownames(estimates_post_conf_excl_poly_no_nw)=="timecube")),]
estimates_post_conf_excl_no_nw<-summary(model_post_conf_excl_no_nw)$estimate
estimates_post_conf_excl_no_nw<-estimates_post_conf_excl_no_nw[-c(14:39,52:77),]


p_val_no_nw<-list()
p_val_no_nw[[1]]<-estimates_pre_conf_excl_poly_no_nw[,4]
p_val_no_nw[[2]]<-estimates_pre_conf_excl_no_nw[,4]
p_val_no_nw[[3]]<-estimates_post_conf_excl_poly_no_nw[,4]
p_val_no_nw[[4]]<-estimates_post_conf_excl_no_nw[,4]

lgof_no_nw<-list()
lgof_no_nw[[1]] <- c(aic_model_pre_conf_excl_poly_no_nw,bic_model_pre_conf_excl_no_nw,ll_model_pre_conf_excl_no_nw,nobs_model_pre_conf_excl_no_nw,nobs2_model_pre_conf_excl_no_nw)
lgof_no_nw[[2]] <- c(aic_model_pre_conf_excl_no_nw,bic_model_pre_conf_excl_poly_no_nw,ll_model_pre_conf_excl_poly_no_nw,nobs_model_pre_conf_excl_poly_no_nw,nobs2_model_pre_conf_excl_poly_no_nw)
lgof_no_nw[[3]] <- c(aic_model_post_conf_excl_poly_no_nw,bic_model_post_conf_excl_no_nw,ll_model_post_conf_excl_no_nw,nobs_model_post_conf_excl_no_nw,nobs2_model_post_conf_excl_no_nw)
lgof_no_nw[[4]] <- c(aic_model_post_conf_excl_no_nw,bic_model_post_conf_excl_poly_no_nw,ll_model_post_conf_excl_poly_no_nw,nobs_model_post_conf_excl_poly_no_nw,nobs2_model_post_conf_excl_poly_no_nw)

gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.", "Num. obs. second stage") #names of GOFs

est_no_nw <- cbind(estimates_pre_conf_excl_poly_no_nw[,1],estimates_pre_conf_excl_no_nw[,1],estimates_post_conf_excl_poly_no_nw[,1],estimates_post_conf_excl_no_nw[,1])
colnames(est_no_nw) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")
se_no_nw <- cbind(estimates_pre_conf_excl_poly_no_nw[,2],estimates_pre_conf_excl_no_nw[,2],estimates_post_conf_excl_poly_no_nw[,2],estimates_post_conf_excl_no_nw[,2])
colnames(est_no_nw) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")

# add row labels:
rownames(est_no_nw) <- rownames(estimates_pre_conf_excl_no_nw)

# create a texreg object:
tr_no_nw <- list()
for (j in 1:ncol(est_no_nw)) {
  tr_no_nw[[j]] <- createTexreg(
    coef.names = rownames(est_no_nw), 
    coef = est_no_nw[, j], 
    se = se_no_nw[, j],
    pvalues = p_val_no_nw[[j]],
    ci.low = numeric(0),
    ci.up = numeric(0),
    gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.", "Num. obs. second stage"),
    gof =lgof_no_nw[[j]]
    
  )
}

# for text output:
screenreg(tr_no_nw, custom.model.names = colnames(est_no_nw), 
          custom.note = "",stars = c(0.001,  0.01, 0.05),gof=lgof_no_nw)

# for LaTeX output:
texreg(tr_no_nw, custom.model.names = colnames(est_no_nw),
       custom.note = "")

## now for the linear model ----


estimates_pre_conf_excl_poly_lm<-summary(model_pre_conf_excl_poly_lm)$coef
estimates_pre_conf_excl_poly_lm<-estimates_pre_conf_excl_poly_lm[-c(which(rownames(estimates_pre_conf_excl_poly_lm)=="time_reduce"),which(rownames(estimates_pre_conf_excl_poly_lm)=="timesq"),which(rownames(estimates_pre_conf_excl_poly_lm)=="timecube")),]
estimates_pre_conf_excl_lm<-summary(model_pre_conf_excl_lm)$coef
estimates_pre_conf_excl_lm<-estimates_pre_conf_excl_lm[-c(23:59),]

estimates_post_conf_excl_poly_lm<-summary(model_post_conf_excl_poly_lm)$coef
estimates_post_conf_excl_poly_lm<-estimates_post_conf_excl_poly_lm[-c(which(rownames(estimates_post_conf_excl_poly_lm)=="time_reduce"),which(rownames(estimates_post_conf_excl_poly_lm)=="timesq"),which(rownames(estimates_post_conf_excl_poly_lm)=="timecube")),]
estimates_post_conf_excl_lm<-summary(model_post_conf_excl_lm)$coef
estimates_post_conf_excl_lm<-estimates_post_conf_excl_lm[-c(23:48),]


p_val_lm<-list()
p_val_lm[[1]]<-estimates_pre_conf_excl_poly_lm[,4]
p_val_lm[[2]]<-estimates_pre_conf_excl_lm[,4]
p_val_lm[[3]]<-estimates_post_conf_excl_poly_lm[,4]
p_val_lm[[4]]<-estimates_post_conf_excl_lm[,4]

lgof_lm<-list()
lgof_lm[[1]] <- c(aic_model_pre_conf_excl_poly_lm,bic_model_pre_conf_excl_lm,ll_model_pre_conf_excl_lm,nobs_model_pre_conf_excl_lm)
lgof_lm[[2]] <- c(aic_model_pre_conf_excl_lm,bic_model_pre_conf_excl_poly_lm,ll_model_pre_conf_excl_poly_lm,nobs_model_pre_conf_excl_poly_lm)
lgof_lm[[3]] <- c(aic_model_post_conf_excl_poly_lm,bic_model_post_conf_excl_lm,ll_model_post_conf_excl_lm,nobs_model_post_conf_excl_lm)
lgof_lm[[4]] <- c(aic_model_post_conf_excl_lm,bic_model_post_conf_excl_poly_lm,ll_model_post_conf_excl_poly_lm,nobs_model_post_conf_excl_poly_lm)

gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs.") #names of GOFs

est_lm <- cbind(estimates_pre_conf_excl_poly_lm[,1],estimates_pre_conf_excl_lm[,1],estimates_post_conf_excl_poly_lm[,1],estimates_post_conf_excl_lm[,1])
colnames(est_lm) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")
se_lm <- cbind(estimates_pre_conf_excl_poly_lm[,2],estimates_pre_conf_excl_lm[,2],estimates_post_conf_excl_poly_lm[,2],estimates_post_conf_excl_lm[,2])
colnames(est_lm) <- c("Pre (Poly. time)","pre (Time fixed eff.)","Post (Poly. time)","Post (Time fixed eff.)")

# add row labels:
rownames(est_lm) <- rownames(estimates_pre_conf_excl_lm)

# create a texreg object:
tr_lm <- list()
for (j in 1:ncol(est_lm)) {
  tr_lm[[j]] <- createTexreg(
    coef.names = rownames(est_lm), 
    coef = est_lm[, j], 
    se = se_lm[, j],
    pvalues = p_val_lm[[j]],
    ci.low = numeric(0),
    ci.up = numeric(0),
    gof.names <- c("AIC", "BIC", "Log Likelihood", "Num. obs."),
    gof =lgof_lm[[j]]
    
  )
}

# for text output:
screenreg(tr_lm, custom.model.names = colnames(est_lm), 
          custom.note = "",stars = c(0.001,  0.01, 0.05),gof=lgof_lm)

# for LaTeX output:
texreg(tr_lm, custom.model.names = colnames(est_lm),
       custom.note = "")


