
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Object-Oriented Management Strategy Evaluation using parallel processing
# OMd > OM + Obs > MSE 

# Tom Carruthers UBC
# Laurie Kell ICCAT
# Campbell Davies CSIRO

# 10th October 2014

# Prerequisites ===================================================================================
rm(list=ls(all=TRUE))
setwd("D:/ABT-MSE/")
library(maps)
library(mapdata)
library(snowfall)
library(abind)
library(FLCore)
library(FLAssess)
library(FLXSA)
source("Source/MSE_source.r")
source("Source/Objects.r")
source("Source/Methods.r")
source("Source/MPs.r")
source("Source/IE_models.r")
source("Source/Diagnostics.r")
sfInit(parallel=T,cpus=8)


OMS<-c("SCRS_OM1","SCRS_OM2","SCRS_OM3")
Dmod<-c(0.66, 1.5)
Mmod<-c(0.8,1.25)
hmod<-c(0.8,1.25)
recgrad<-c(0,-0.5) # additional % annual change in recruitment devs
Obsmod<-c("Preliminary_Obs","Poor_Obs")
IEs<-c("Umax","Overage")

master<-expand.grid(OMS,Dmod,Mmod,hmod,recgrad,Obsmod,IEs)

for(i in 1:length(master)){
 
  print("---------------------------------------")
  print(i)
  print("---------------------------------------")
  
  load(paste("Objects/",master[i,1],sep=""))
  OMd@nsim<-as.integer(32)
  
  OMd@D<-OMd@D*master[i,2]
  OMd@Magemu<-OMd@Magemu*master[i,3]
  OMd@h<-OMd@h*master[i,4]
  OMd@recgrad<-OMd@recgrad+master[i,5]
  
  OM<-new('OM',OMd)
  load(paste("Objects/",master[i,6],sep=""))
  
  tmse<-new('MSE',OM,Obs,
            MPs<-c("SPslope","DD","DD4010","SBT2","Fadapt",
                   "Islope1","LstepCC4","UMSY","UMSY_PI"),
            interval=3,IE=as.character(master[i,7]))

  tsum<-stats(tmse)
  save(tsum,file=paste("Results/PR",i,paste(master[i,],collapse="-"),sep="_"))
   
}


