
# ============================================================================================================================================
# === Build a preliminary Observation model object 'Obs' ============================================================================
# ============================================================================================================================================

# Indexing: S sim   P pop   A age   Y year   M subyear   R region   rng sample range

# --- Set working directory ------

setwd("H:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")

# --- Create a blank Obs definition object

Obs<-new('Obs')

Obs@Ccv<-Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.2,0.5)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.4
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-1
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-0.05
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-0.2
Obs@nCAAobs<-c(1000,2000)
Obs@nCALobs<-1000
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.66,1.5)#exp(runif(nsim,log(0.75),log(1.25)))

save(Obs,file=paste(getwd(),"/Objects/Poor_Obs",sep=""))
