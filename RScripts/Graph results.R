# ===============================================================================================================
# === Graph results data ABT MSE ==========================================================================
# ===============================================================================================================

# Prerequisites --------------------------------------

rm(list=ls(all=TRUE))
setwd("F:/ABT-MSE/")
source("Source/Diagnostics.r")
load(file="Results/Resdat")
library(wordcloud)

# --- Performance distribution plots -----------------

jpeg("Images/marginal.jpg",res=300,width=7.5,height=13,units='in')
par(mfrow=c(8,3),mai=c(0.2,0.3,0.15,0.01),omi=c(0.1,0.35,0.2,0.02))
sumplot2(dat,fieldv=c("MP","M","h","Rec. grad.","SH","obs","ie","Dep"),pm=4)
dev.off()

jpeg("Images/marginal_DDgood.jpg",res=300,width=7.5,height=9,units='in')
par(mfrow=c(6,3),mai=c(0.3,0.3,0.15,0.01),omi=c(0.1,0.35,0.2,0.02))
sumplot2(dat[dat$MP=="DD"&dat$obs=="Good quality data",],fieldv=c("M","h","Rec. grad.","SH","ie","Dep"),pm=4)
dev.off()

par(mfrow=c(1,3),mai=c(0.2,0.4,0.15,0.01),omi=c(0.3,0.35,0.02,0.02))
sumplot2(dat[dat$MP=="DD"&dat$obs=="Good quality data",],fieldv=c("SH"),pm=4)

# --- Trade-off plots -------------------------------

jpeg("Images/Tplot.jpg",res=300,width=6.5,height=11,units='in')
par(mfrow=c(5,2),mai=c(0.2,0.3,0.15,0.01),omi=c(0.3,0.35,0.02,0.02))
Tplot2(dat,fieldv=c("M","h","SH","Dep","obs"))
dev.off()

par(mfrow=c(1,2),mai=c(0.2,0.3,0.15,0.01),omi=c(0.3,0.35,0.02,0.02))
Tplot2(dat,fieldv=c("SH"))
# ---- Summary statistics ---------------------------

names(dat)[5]<-"RG"
test<-lm(Y5~SH+Dep+M+h+RG+obs+ie,dat=dat)
write.csv(summary(test)$coefficients,"Results/anova.csv")

datDD<-dat[dat$MP=="DD",]
test<-lm(Y5~SH+Dep+M+h+RG+obs+ie,dat=datDD)
write.csv(summary(test)$coefficients,"Results/anova DD.csv")


# --- Write the GeNie table -----------------------

write.csv(dat,file="Results/Results.csv",sep=",",row.names=F)
