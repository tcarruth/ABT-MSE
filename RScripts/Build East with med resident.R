
# Inventing a Mediterranean stock with a resident subpopulation between 10% and 100% that of the transitory population

setwd("F:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")

# --- Create a blank OM definition object

OMd<-new('OMd')

# --- Description ------

OMd@Name<-"Med resident"
OMd@Date<-"12th October 2014"
OMd@Author<-"T.Carruthers"
OMd@Notes<-"This is a straw man demonstration of ABT-MSE functionality. It is only partly-empirically derived and should not be taken too seriously"
OMd@PrimarySource<-"http://www.iccat.int/Documents/Meetings/Docs/2012_BFT_ASSESS.pdf"


# --- Dimensions ------

OMd@nsim<-as.integer(20)       # Number of simulations
OMd@npop<-as.integer(2)        # 2 populations (stocks): East/Med, West
OMd@nages<-as.integer(32)      # Up to a maximum age of 32 but survival is nil at age 15 given M at age so..
OMd@nyears<-as.integer(43)     # 1971-2013
OMd@nsubyears<-as.integer(4)   # Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec
OMd@nareas<-as.integer(5)      # WAtl, EAtl, WMed, EMed, BS
OMd@proyears<-as.integer(30)   # Number projected years


# --- Ranges for inputs --------

OMd@Magemu<-array(rep(c(0.49,rep(0.24,4),0.2,0.175,0.15,0.125,rep(0.10,23)),each=2),c(OMd@npop,OMd@nages)) # P, A  mean M at age
OMd@Mrange<-array(c(0.9,0.9,1.1,1.1),c(2,2))                 # P, rng    Sample range for mean M at age
OMd@Msd<-array(c(0.05,0.05,0.2,0.2),c(2,2))                  # P, rng    Interannual variability in M same for both stocks and sampled independently
OMd@Mgrad<-array(c(-0.25,-0.25,0.25,0.25),c(2,2))            # P, rng    Gradient in M ranging from -1/4 to 1/4 % yr-1
OMd@SRrel<-rep(as.integer(1),2)                              # P         The SR relationship 1:Beverton Holt 2:Ricker
OMd@h<-array(c(0.35,0.35,0.65,0.65),c(2,2))                  # P, rng    Steepness eyeballed from 2012 assessment document (west) Figure 30
OMd@recgrad<-array(c(-0.25,-0.25,0.25,0.25),c(2,2))          # P, rng    Gradient in recruitment ranging from -1/4 to 1/4 % yr-1
OMd@Reccv<-array(c(0.1,0.1,0.3,0.3),c(2,2))                  # P, rng    Internannual variability in recruitment
OMd@AC<-array(c(0,0,0.5,0.5),c(2,2))                         # P, rng    Recruitment autocorrelation
OMd@Recsubyr<-rep(as.integer(2),2)                           # P         Both spawn in second quarter
OMd@Linf<-array(c(313,317,313,317),c(2,2))                   # P, rng    Von-B maximum length
OMd@K<-array(c(0.087,0.091,0.087,0.091),c(2,2))              # P, rng    Von-B maximum growth rate
OMd@t0<-rep(-1.13,2)                                         # P         Von-B theoretical age at length zero
OMd@Ksd<-array(c(0.001,0.001,0.002,0.002),c(2,2))            # P, rng    Inter-annual variability in von B K
OMd@Kgrad<-array(c(-0.001,-0.001,0.001,0.001),c(2,2))        # P, rng    Gradient in von B K ranging from -1/4 to 1/4 % yr-1
OMd@Linfsd<-array(c(0.001,0.001,0.002,0.002),c(2,2))         # P, rng    Inter-annual variability in von B K
OMd@Linfgrad<-array(c(-0.001,-0.001,0.001,0.001),c(2,2))     # P, rng    Gradient in von B Linf ranging from -1/4 to 1/4 % yr-1
OMd@a<-rep(2.95*10^-5,2)                                     # P         Weight length paramter a (W=aL^b)
OMd@b<-rep(2.899,2)                                          # P         Weight length paramter b (W=aL^b)
OMd@ageM<-array(c(3.5,3.5,4.5,4.5),c(OMd@npop,2))            # P, rng    Inflection point of logistic maturity model: age at 50% maturity
OMd@ageMsd<-array(c(0.1,0.1,0.3,0.3),c(OMd@npop,2))          # P, rng    Slope of the logistic maturity model: spread of maturity ogive
OMd@ageMgrad<-array(c(-0.25,-0.25,0.25,0.25),c(OMd@npop,2))  # P, rng    Gradient in age at  50% maturity
OMd@D<-array(c(0.1,0.1,0.4,0.4),c(OMd@npop,2))               # P, rng    Depletion relative to SSB0 East assuming BMSY is 35% of B0, West SSB0 is 50000 
OMd@R0<-array(c(100,10,100.1,50),c(OMd@npop,2))              # P, rng    Unfished recruitment: proxy is current spawning stock biomass
OMd@Size_area<-array(1,c(OMd@npop,OMd@nareas))               # P, R      Equal fishing area across regions
OMd@nfleets<-as.integer(2)                                   # P         Model two fleets
OMd@age05<-array(c(2,2,3,3),c(2,2))                          # F, rng    The range of youngest age at 5% selectivity 
OMd@Vmaxage<-array(c(0.7,0.7,1,1),c(2,2))                    # F, rng    The range of vulnerability of the oldest age class
OMd@AFS<-array(c(5,5,8,8),c(2,2))                            # F, rng    The range of age at full selection
OMd@Fsd<-array(c(0.05,0.05,0.2,0.2),c(2,2))                  # F, rng    The range of inter-annual vulnerability in F
OMd@Fgrad<-array(c(-5,-5,5,5),c(2,2))                        # F, rng    The range of final gradient in F
OMd@Frat<-0.5                                                # F the ratio of mean apical fishing mortality rate
OMd@Spat_targ<-array(c(1,1,1.5,1.5),c(2,2))                  # F, rng    The range of spatial targetting

# ----- Invent movement matrices ----------------------------------------------------------

OMd@Area_names<-c("WAtlw", "EAtlw", "WCMed", "EMed", "BS")
for(i in 1:OMd@nareas)OMd@Area_defs[[i]]<-AreaDefs[[match(OMd@Area_names[i],AreaNames)]]
plotareas(OMd) 


# areas  WAtl EAtl  WMed EMed BS
gravs<-tomt(array(c(-1,  -0.5, 0.5, 1,   -10,  
         -0.5, 0,   2,   1,   -10,  
         0,    2,   0.5, 0.5, -10,  
         -0.5, 1,   1,   1,   -10,  
         -10, -10,  -1,  2,   -1,     
         -10, -10,  -1,  2,   -1,     
         -10, -10,  -0.5,2,   -0.5,   
         -10, -10,  -0.5,2,   -0.5),  
       c(5,4,2)))

visc<-t(array(c(0.5,2,0.5,2,
              1,  3,3,1),dim=c(4,2)))

notmat<-tomt(array(c(0,0, 1, 1, 0,  
                    0, 0, 1, 1, 0,  
                    0, 0, 1, 1, 0,  
                    0, 0, 1, 1, 0,  
                    0, 0, 1, 1, 1,     
                    0, 0, 1, 1, 1,     
                    0, 0, 1, 1, 1,   
                    0, 0, 1, 1, 1),  
                  c(5,4,2)))

OMd@excl<-t(array(c(1,1,1,1,0,
                    0,0,1,1,1),c(OMd@nareas,OMd@npop))) # P, R  Exclusion matrix [0,1] what areas are the available to each population

movs<-invent_mov(gravs,visc,notmat,excl=OMd@excl,nages=OMd@nages,nyears=OMd@proyears+OMd@nyears)

OMd@mov<-movs[[1]]
OMd@movvar<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))        # P, rng    Inter-simulation variability in mean movement
OMd@movsd<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))         # P, rng    Inter-annual variability in movement
OMd@movgrad<-array(c(-0.25,-0.25,0.25,0.25),c(OMd@npop,2))   # P, rng    Gradient in gravity weight by area (log space)

OMd@Mmov<-movs[[2]]
OMd@Mmovvar<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))       # P, rng    Inter-simulation variability in mean movement
OMd@Mmovsd<-array(c(0.01,0.01,0.2,0.2),c(OMd@npop,2))        # P, rng    Inter-annual variability in movement
OMd@Mmovgrad<-array(c(-0.25,-0.25,0.25,0.25),c(OMd@npop,2))  # P, rng    Gradient in gravity weight by area (log space)


OMd@targpop<-1:2
OMd@seed<-1

# --- save object -------

save(OMd,file=paste(getwd(),"/Objects/Med_Res",sep=""))
