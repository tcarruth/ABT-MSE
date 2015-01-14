# ===============================================================================================================
# === Consolidate results data ABT MSE ==========================================================================
# ===============================================================================================================

# Prerequisites ===================================================================================

rm(list=ls(all=TRUE))
setwd("H:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")
source("Source/Methods.r")
source("Source/MPs.r")
source("Source/IE_models.r")
source("Source/Diagnostics.r")


OMS<-c("SCRS_OM1","SCRS_OM2","SCRS_OM3")
Dmod<-c(0.66, 1.5)
Mmod<-c(0.8,1.25)
hmod<-c(0.8,1.25)
recgrad<-c(0,-0.5) # additional % annual change in recruitment devs
Obsmod<-c("Preliminary_Obs","Poor_Obs")
IEs<-c("Umax","Overage")

mastnam<-expand.grid(1:length(OMS),1:length(Dmod),1:length(Mmod),1:length(hmod),1:length(recgrad),1:length(Obsmod),1:length(IEs))
OMnam<-c("SH1 2 pop no cont","SH2 2 pop with cont","SH3 Meta Pop")
Dnam<-c("More depleted","Less depleted")
Mnam<-c("Low M", "High M")
hnam<-c("Low rec. comp","High rec. comp")
rgnam<-c("Flat rec.","Decling rec.")
obsnam<-c("Good quality data", "Bad quality data")
ienam<-c("no overages","20% overages")

master<-expand.grid(OMS,Dmod,Mmod,hmod,recgrad,Obsmod,IEs)
MPs<-c("SPslope","DD","DD4010","SBT2","Fadapt",
       "Islope1","LstepCC4","UMSY","UMSY_PI")
nMPs<-length(MPs)
nsim<-32
proyears<-30
ind<-cbind(rep(MPs,nsim),rep(nsim,each=nMPs))
yind<-1:proyears
refMP<-9

for(i in 1:nrow(master)){
  
  load(file=paste("Results/PR",i,paste(master[i,],collapse="-"),sep="_"))
  flev<-array(rep(c(OMnam[mastnam[i,1]],Dnam[mastnam[i,2]],Mnam[mastnam[i,3]],hnam[mastnam[i,4]],
          rgnam[mastnam[i,5]],obsnam[mastnam[i,6]],ienam[mastnam[i,7]]),each=nsim*nMPs),c(nsim*nMPs,7))
  PGK<-round(as.vector(tsum$Pgreen),4)
  AAVY<-round(as.vector(tsum$AAVY)/as.vector(apply(tsum$C,1:2,mean)),4)
  Y<-apply(tsum$C,1:2,mean)
  Y5<-apply(tsum$C*array(rep(0.95^yind,each=nMPs*nsim),c(nMPs,nsim,proyears)),1:2,mean)
  Y10<-apply(tsum$C*array(rep(0.9^yind,each=nMPs*nsim),c(nMPs,nsim,proyears)),1:2,mean)
  Y<-round(as.vector(Y/array(rep(Y[refMP,],each=nMPs),c(nMPs,nsim))),4)
  Y5<-round(as.vector(Y5/array(rep(Y5[refMP,],each=nMPs),c(nMPs,nsim))),4)
  Y10<-round(as.vector(Y10/array(rep(Y10[refMP,],each=nMPs),c(nMPs,nsim))),4)
  
  tdat<-cbind(flev,ind,AAVY,PGK,Y,Y5,Y10)
  
  if(i ==1)dat<-tdat
  if(i>1)dat<-rbind(dat,tdat)
  
}
dat<-as.data.frame(dat)
for(i in 10:14)dat[,i]<-as.numeric(as.character(dat[,i]))
names(dat)=c("SH","Dep","M","h","Rec. grad.","obs","ie","MP","sim","AAVY","PGK","Y","Y5","Y10")


