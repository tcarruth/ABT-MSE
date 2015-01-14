
# ============================================================================================================================================
# === Build a preliminary Operating Model definition object 'OMd' ============================================================================
# ============================================================================================================================================

# Indexing: S sim   P pop   A age   Y year   M subyear   R region   rng sample range

# --- Set working directory ------

setwd("F:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")

# --- Create a blank OM definition object

OMd<-new('OMd')

# --- Description ------

OMd@Name<-"MAST_EW"
OMd@Date<-"10th October 2014"
OMd@Author<-"T.Carruthers"
OMd@Notes<-"This is a straw man demonstration of ABT-MSE functionality. It is only partly-empirically derived and should not be taken too seriously"
OMd@PrimarySource<-"http://www.iccat.int/Documents/Meetings/Docs/2012_BFT_ASSESS.pdf"


# --- Dimensions ------

OMd@nsim<-as.integer(20)       # Number of simulations
OMd@npop<-as.integer(2)        # 2 populations (stocks): East/Med, West
OMd@nages<-as.integer(32)      # Up to a maximum age of 32 but survival is nil at age 15 given M at age so..
OMd@nyears<-as.integer(43)     # 1971-2013
OMd@nsubyears<-as.integer(4)   # Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec
OMd@nareas<-as.integer(5)      # GOM, GSL, WAtl, EAtl, Med
OMd@proyears<-as.integer(30)   # Number projected years


# --- Ranges for inputs --------

OMd@Magemu<-t(array(c(0.49,rep(0.24,4),0.2,0.175,0.15,0.125,rep(0.10,23),
                 rep(0.14,OMd@nages)),c(OMd@nages,OMd@npop))) # P, A  mean M at age
OMd@Mrange<-array(c(0.9,0.9,1.1,1.1),c(2,2))                 # P, rng    Sample range for mean M at age
OMd@Msd<-array(c(0.05,0.05,0.2,0.2),c(2,2))                  # P, rng    Interannual variability in M same for both stocks and sampled independently
OMd@Mgrad<-array(c(-0.25,-0.25,0.25,0.25),c(2,2))            # P, rng    Gradient in M ranging from -1/4 to 1/4 % yr-1
OMd@SRrel<-rep(as.integer(1),2)                              # P         The SR relationship 1:Beverton Holt 2:Ricker
OMd@h<-array(c(0.35,0.35,0.65,0.65),c(2,2))                  # P, rng    Steepness eyeballed from 2012 assessment document (west) Figure 30
OMd@recgrad<-array(c(-0.25,-0.25,0.25,0.25),c(2,2))          # P, rng    Gradient in recruitment ranging from -1/4 to 1/4 % yr-1
OMd@Reccv<-array(c(0.1,0.1,0.3,0.3),c(2,2))                  # P, rng    Internannual variability in recruitment
OMd@AC<-array(c(0,0,0.5,0.5),c(2,2))                         # P, rng    Recruitment autocorrelation
OMd@Recsubyr<-rep(as.integer(2),2)                           # P         Both spawn in second quarter
OMd@Linf<-array(c(313,317,317,341),c(2,2))                   # P, rng    Von-B maximum length
OMd@K<-array(c(0.087,0.091,0.091,0.095),c(2,2))              # P, rng    Von-B maximum growth rate
OMd@t0<-c(-1.13,-0.97)                                       # P         Von-B theoretical age at length zero
OMd@Ksd<-array(c(0.001,0.001,0.002,0.002),c(2,2))            # P, rng    Inter-annual variability in von B K
OMd@Kgrad<-array(c(-0.001,-0.001,0.001,0.001),c(2,2))        # P, rng    Gradient in von B K ranging from -1/4 to 1/4 % yr-1
OMd@Linfsd<-array(c(0.001,0.001,0.002,0.002),c(2,2))         # P, rng    Inter-annual variability in von B K
OMd@Linfgrad<-array(c(-0.001,-0.001,0.001,0.001),c(2,2))     # P, rng    Gradient in von B Linf ranging from -1/4 to 1/4 % yr-1
OMd@a<-c(2.95*10^-5,1.96*10^-5)                              # P         Weight length paramter a (W=aL^b)
OMd@b<-c(2.899,3.009)                                        # P         Weight length paramter b (W=aL^b)
OMd@ageM<-array(c(3.5,8.5,4.5,9.5),c(OMd@npop,2))            # P, rng    Inflection point of logistic maturity model: age at 50% maturity
OMd@ageMsd<-array(c(0.1,0.1,0.3,0.3),c(OMd@npop,2))          # P, rng    Slope of the logistic maturity model: spread of maturity ogive
OMd@ageMgrad<-array(c(-0.25,-0.25,0.25,0.25),c(OMd@npop,2))  # P, rng    Gradient in age at  50% maturity
OMd@D<-array(c(0.35*0.1,15/50,0.35*0.75,20/50),c(OMd@npop,2))# P, rng    Depletion relative to SSB0 East assuming BMSY is 35% of B0, West SSB0 is 50000 
OMd@R0<-array(c(225,15,450,20),c(OMd@npop,2))                # P, rng    Unfished recruitment proxy is current spawning stock biomass
OMd@Size_area<-array(1,c(OMd@npop,OMd@nareas))               # P, R      Equal fishing area across regions
OMd@nfleets<-as.integer(2)                                   # P         Model two fleets
OMd@age05<-array(c(2,2,3,3),c(2,2))                          # F, rng    The range of youngest age at 5% selectivity 
OMd@Vmaxage<-array(c(0.7,0.7,1,1),c(2,2))                    # F, rng    The range of vulnerability of the oldest age class
OMd@AFS<-array(c(5,5,8,8),c(2,2))                            # F, rng    The range of age at full selection
OMd@Fsd<-array(c(0.05,0.05,0.2,0.2),c(2,2))                  # F, rng    The range of inter-annual vulnerability in F
OMd@Fgrad<-array(c(-5,-5,5,5),c(2,2))                        # F, rng    The range of final gradient in F
OMd@Frat<-0.5                                                # F         The ratio of mean apical fishing mortality rate
OMd@Spat_targ<-array(c(1,1,1.5,1.5),c(2,2))                  # F, rng    The range of spatial targetting

# ----- Get movement matrices from 2010 MAST model ----------------------------------------------------------
# ----- Non mature fish --------------------------------------------------
refsy<-c(4,1,2,3)# Nathan's model assigns movement to subyear based on what happened during the time step. For programming purposes its much easier to move this one step back 

dato<-read.csv(paste(getwd(),"/Data/movemat.dat",sep=""),header=F)
dat<-dato[!((1:96)%in%((0:15)*6+1)),]
ind<-expand.grid(1:OMd@nareas,1:OMd@nsubyears,1:2,1:OMd@npop)[,c(1,4,3,2)]
movi<-array(NA,c(OMd@nareas,OMd@npop,2,OMd@nsubyears,OMd@nareas))
for(i in 1:length(dat))  movi[ind[i,1],ind[i,2],ind[i,3],ind[i,4],]<-as.numeric(unlist(strsplit(as.character(dat[i])," ")))[2:6]

# temporary fix because nathan has lots of all zero matrices in season 2 and other all zero rows
ind<-expand.grid(1:OMd@nareas,1:OMd@npop,1:2,1:OMd@nsubyears)
test<-ind[apply(movi,1:4,sum)==0,]    # record the index where a row is zero
test<-as.matrix(cbind(test,test[,1])) # duplicate the area (positive diagonal)
movi[test]<-1                         # Keep all fishing the same area when a row is all zeros

mov<-array(NA,c(OMd@npop,OMd@nages,OMd@nyears+OMd@proyears,OMd@nsubyears,OMd@nareas,OMd@nareas))
ind<-as.matrix(expand.grid(1:OMd@npop,1:OMd@nages,1:(OMd@nyears+OMd@proyears),1:OMd@nsubyears,1:OMd@nareas,1:OMd@nareas))
indi<-ind[,c(5,1,2,4,6)]
indi[,3][indi[,3]<9]<-1
indi[,3][indi[,3]>8]<-2
ind[,1]<-as.integer((ind[,1]==2))
ind[ind[,1]==0,1]<-2
ind[,4]<-refsy[ind[,4]] # remap sub years to happen one timestep earlier
mov[ind]<-movi[indi]


# ----- Mature fish -----------------------------------------------------
dato<-read.csv(paste(getwd(),"/Data/spmovemat.dat",sep=""),header=F)
dat<-dato[!((1:360)%in%((0:59)*6+1)),]
ind<-expand.grid(1:OMd@nareas,1:30,1:OMd@npop)[,c(1,3,2)]
movi<-array(NA,c(OMd@nareas,OMd@npop,30,OMd@nareas))
for(i in 1:length(dat))  movi[ind[i,1],ind[i,2],ind[i,3],]<-as.numeric(unlist(strsplit(as.character(dat[i])," ")))[2:6]

# temporary fix because nathan has lots of all zero matrices in season 2 and other all zero rows
ind<-expand.grid(1:OMd@nareas,1:OMd@npop,1:30)
test<-ind[apply(movi,1:3,sum)==0,]    # record the index where a row is zero
test<-as.matrix(cbind(test,test[,1])) # duplicate the area (positive diagonal)
movi[test]<-1                         # Keep all fishing the same area when a row is all zeros

Mmov<-mov
ind<-expand.grid(1:OMd@npop,1:OMd@nages,1:(OMd@nyears+OMd@proyears),1:OMd@nareas,1:OMd@nareas)
ind<-as.matrix(cbind(ind[,1:3],refsy[OMd@Recsubyr[ind[,1]]],ind[,4:5]))

indi<-ind[,c(5,1,2,6)]
indi[,3][indi[,3]>30]<-30
ind[,1]<-as.integer((ind[,1]==2))
ind[ind[,1]==0,1]<-2
Mmov[ind]<-movi[indi]

OMd@mov<-mov
OMd@movvar<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))        # P, rng    Inter-simulation variability in mean movement
OMd@movsd<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))         # P, rng    Inter-annual variability in movement
OMd@movgrad<-array(c(-0.25,-0.25,0.25,0.25),c(OMd@npop,2))   # P, rng    Gradient in gravity weight by area (log space)

OMd@Mmov<-Mmov
OMd@Mmovvar<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))       # P, rng    Inter-simulation variability in mean movement
OMd@Mmovsd<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))        # P, rng    Inter-annual variability in movement
OMd@Mmovgrad<-array(c(-0.25,-0.25,0.25,0.25),c(OMd@npop,2))  # P, rng    Gradient in gravity weight by area (log space)

OMd@excl<-t(array(c(0,0,1,1,1,1,1,1,1,0),c(OMd@nareas,OMd@npop))) # P, R  Exclusion matrix [0,1] what areas are the available to each population

OMd@Area_names<-c("GOM", "GSL", "WAtl", "EAtlw", "Med")
for(i in 1:OMd@nareas)OMd@Area_defs[[i]]<-AreaDefs[[match(OMd@Area_names[i],AreaNames)]]
plotareas(OMd) 
OMd@targpop<-1 # just the eastern stock is of concern
OMd@seed<-1

# --- save object -------

save(OMd,file=paste(getwd(),"/Objects/MAST_EW",sep=""))


# ==========================================================================================================================
# End of make sript ========================================================================================================
# ==========================================================================================================================








