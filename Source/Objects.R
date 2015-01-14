# ===========================================================================================================================================================================
# ==== ABT MSE object classes ===============================================================================================================================================
# ===========================================================================================================================================================================

# Operating model definition object ---------------------------------------------------------------------------------------------------------------------
setClass("OMd",representation(
               
               # Description
               Name="character",Date="character",Author="character",
               Notes="character",PrimarySource="character",
               
               # Dimensions
               nsim="integer",npop="integer",nages="integer",             # MSE dimensions
               nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
               proyears="integer",                                        # Projected years
               
               # Parameter ranges / simulation sample distributions
               Magemu="array",Mrange="array",Msd="array",Mgrad="array",   # Mean natural mortality rate at age, sample range, interannual variability and gradient % yr-1
               SRrel="integer",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient % yr-1
               Reccv="array",AC="array", Recsubyr="integer",              # CV of recruitment deviations and recruitment auto-correlation
               Linf="array",K="array",t0="numeric",                       # Mean growth parameters
               Ksd="array",Kgrad="array",Linfsd="array",Linfgrad="array", # Interannual variability in growth and mean trajectory % yr-1
               a="numeric",b="numeric",                                   # Weight - Length conversion W=aL^b 
               ageM="array",ageMsd="array",ageMgrad="array",              # Age-at-maturity, interannual variability and gradient % yr-1
               D="array",R0="array",                                      # Current stock depletion, abundance                                 
               Size_area="array",mov="array",Mmov="array",                # Size of regions, Markov movement matrix for all fish and mature fish
               movvar="array",movsd="array",movgrad="array",              # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights
               Mmovvar="array",Mmovsd="array",Mmovgrad="array",           # Inter-simulation variability in mature movement, interannual-variability in mature movement, gradient changes in area gravity weights
               excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there
               nfleets="integer",                                         # Number of fleets,
               age05="array",Vmaxage="array", AFS="array",                # age at 5% vulnerability, vulnerability of oldest age class, age at full selection                  
               Fsd="array",Fgrad="array", Frat="numeric",                 # Interannual variability in F, Final gradient in F yr-1
               Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
               Area_names="character", Area_defs="list",                  # Area definitions (polygons)
               targpop="numeric",                                         # The target population for calculation of MSY and depletion reference points
               seed="numeric"                                             # Random seed for the generation of the OM
               
                       
))                                           
               
# Operating model definition object ---------------------------------------------------------------------------------------------------------------------
setClass("OM",representation(
              
              # Description
              Name="character",Date="character",Author="character",
              Notes="character",PrimarySource="character",
              
              # Dimensions
              nsim="integer",npop="integer",nages="integer",             # MSE dimensions
              nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
              proyears="integer",                                        # Proyears
              
              # Sampled parameters 
              Magemu="array",Mrange="array",Msd="array",Mgrad="array",   # Mean natural mortality rate at age, interannual variability and gradient % yr-1
              SRrel="integer",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient % yr-1
              Reccv="array",AC="array",                                  # CV of recruitment deviations and recruitment auto-correlation
              Recsubyr="integer",                    # Sub-year in which recruitment occurs, area in which recruitment occurs
              Linfmu="array",Kmu="array",t0="numeric",                   # Mean growth parameters
              Ksd="array",Kgrad="array",Linfsd="array",Linfgrad="array", # Interannual variability in growth and mean trajectory % yr-1
              a="numeric",b="numeric",                                   # Weight - Length conversion W=aL^b 
              ageMmu="array",ageM="array",ageMsd="array",ageMgrad="array", # Age-at-maturity, interannual variability and gradient % yr-1
              D="array",SSBcur="array",                                  # Current stock depletion, abundance                                 
              Size_area="array",mov="array",Mmov="array",                # Size of regions, Markov movement matrix for all fish and mature fish
              movvar="array",movsd="array",movgrad="array",              # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights
              Mmovvar="array",Mmovsd="array",Mmovgrad="array",           # Inter-simulation variability in mature movement, interannual-variability in mature movement, gradient changes in area gravity weights
              excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there
              nfleets="integer",                                         # Number of fleets,
              age05="array",Vmaxage="array", AFS="array",                # age at 5% vulnerability, vulnerability of oldest age class, age at full selection                  
              Fsd="array",Fgrad="array", Frat="numeric",                 # Interannual variability in F, Final gradient in F yr-1
              Area_names="character", Area_defs="list",                  # Area definitions (polygons)
              Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
              
              # Simulation data -------
              E="array",dFfinal="array",
              q="array",sel="array",
              mat="array",
              Recdevs="array", R0="array",                    # Recruitment deviations and unfished recruitment
              FM="array",M="array",Z="array",                 # Fishing, natural and total instantaneous mortality rate. 
              B="array",SSB="array",NSB="array",              # Biomass, spawning stock biomass
              N="array",SSN="array",NSN="array",              # Numbers, spawning stock numbers
              C="array",CAA="array",                          # Catches taken, catch at age taken
              MSY="numeric",BMSY="numeric", VBMSY="numeric",  # Maximum sustainable yield, biomass at maximum sustainable yield
              SSBMSY="numeric",
              FMSY="numeric",UMSY="numeric",FMSYa="numeric", # Fishing mortality rate at maximum sustainable yield
              SSBMSY_SSB0="numeric",
              IMSY="numeric",                                   # Relative abundance index at maximum sustainable yield
              Linf="array",K="array",
              Idist="array",MIdist="array",
              targpop="numeric",                              # What populations are to be used in MSY / depletion optimization
              seed="numeric"                                  # Random seed from which this object was made
              ))
         
setMethod("initialize", "OM", function(.Object,OMd){
  # Notest
  # Auto-correltion in recrutiment deviations is currently disabled
  set.seed(OMd@seed)
  if(class(OMd)!='OMd'){
    print(paste('Could not build operating model:',deparse(substitute(OMd)),'not of class OMd'))
    stop()
  }
  
  # copy over dimensions ------
  dimslots<-slotNames(OMd)[1:12]
  for(i in 1:12)slot(.Object,dimslots[i])<-slot(OMd,dimslots[i])
#  .Object})
#  .Object<-new('OM',OMd)


  cat("Generating random variables / operating model parameters")
  cat("\n")

  # --- set up M -------
  Mmu<-array(rep(OMd@Magemu,each=OMd@nsim),c(OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears))
  Mrng<-array(runif(OMd@nsim*OMd@npop,rep(t(OMd@Mrange)[1,],each=OMd@nsim),rep(t(OMd@Mrange)[2,],each=OMd@nsim)),c(OMd@nsim,OMd@npop))
  out<-gettempvar2(array(1,c(OMd@npop,2)),OMd@Msd,OMd@Mgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
  gradi<-out[[4]]
  ind<-as.matrix(expand.grid(1:OMd@nsim,1:OMd@npop,1:OMd@nages,1:OMd@nyears+OMd@proyears))  
  ind2<-ind[,c(1,2,4)]
  ind3<-ind[,c(1,2)]
  M<-Mmu
  M[ind]<-Mmu[ind]*gradi[ind2]*Mrng[ind3]
  
  .Object@M<-M
  .Object@Msd<-out[[2]]
  .Object@Mgrad<-out[[3]]
  .Object@Mrange<-Mrng
  
  # ---- Stock-recruit relationships -------
  .Object@SRrel<-OMd@SRrel
  .Object@h<-gettempvar3(OMd@h,OMd@nsim,OMd@npop)
  out<-gettempvar2(array(1,c(OMd@npop,2)),OMd@Reccv,OMd@recgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
  .Object@Recdevs<-out[[4]]
  .Object@Reccv<-out[[2]]
  .Object@recgrad<-out[[3]]
  .Object@AC<-gettempvar3(OMd@AC,OMd@nsim,OMd@npop)
  .Object@Recsubyr<-OMd@Recsubyr
  .Object@t0<-OMd@t0
  out<-gettempvar2(OMd@K,OMd@Ksd,OMd@Kgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
  .Object@Kmu<-out[[1]]
  .Object@Ksd<-out[[2]]
  .Object@Kgrad<-out[[3]]
  .Object@K<-out[[4]]
  out<-gettempvar2(OMd@Linf,OMd@Linfsd,OMd@Linfgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
  .Object@Linfmu<-out[[1]]
  .Object@Linfsd<-out[[2]]
  .Object@Linfgrad<-out[[3]]
  .Object@Linf<-out[[4]]
  .Object@a<-OMd@a
  .Object@b<-OMd@b
  .Object@ageMmu<-array(NA,c(OMd@nsim,OMd@npop))
  .Object@ageMsd<-array(NA,c(OMd@nsim,OMd@npop))
  .Object@ageMgrad<-array(NA,c(OMd@nsim,OMd@npop))
  .Object@ageM<-array(NA,c(OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears))
  .Object@mat<-array(NA,c(OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears))
  for(pp in 1:OMd@npop){
    
    .Object@ageMmu[,pp]<-runif(OMd@nsim,OMd@ageM[pp,1],OMd@ageM[pp,2])   # now predicted by a log-linear model
    .Object@ageMsd[,pp]<-runif(OMd@nsim,OMd@ageMsd[pp,1],OMd@ageMsd[pp,2])
    .Object@ageMgrad[,pp]<-runif(OMd@nsim,OMd@ageMgrad[pp,1],OMd@ageMgrad[pp,2])
    .Object@ageM[,pp,]<-gettempvar(.Object@ageMmu[,pp],rep(0.001,OMd@nsim),
                                             .Object@ageMgrad[,pp],OMd@nyears+OMd@proyears,OMd@nsim)   # creates a time series per simulation that has gradient grad and random normal walk wiht sigma
    
    for(yy in 1:(OMd@nyears+OMd@proyears)){     
      ageMarray<-array(.Object@ageM[,pp,yy],dim=c(OMd@nsim,OMd@nages)) # Age at maturity array
      Agearray<-array(rep(1:OMd@nages,each=OMd@nsim),dim=c(OMd@nsim,OMd@nages))   # Age array
      .Object@mat[,pp,,yy]<-1/(1+exp((ageMarray-(Agearray))/(ageMarray*.Object@ageMsd[,pp])))  # Maturity at age array
    }
  }
  .Object@D<-gettempvar3(OMd@D,OMd@nsim,OMd@npop)
  #.Object@SSB<-rep(OMd@SSBcur[,1],each=OMd@nsim)+(rep(OMd@SSBcur[,2],each=OMd@nsim)-rep(OMd@SSBcur[,1],each=OMd@nsim))*
       (.Object@D-rep(OMd@D[,1],each=OMd@nsim))/(rep(OMd@D[,2],each=OMd@nsim)-rep(OMd@D[,1],each=OMd@nsim))
  
  .Object@Size_area<-OMd@Size_area
  

  sel<-array(NA,c(OMd@nsim,OMd@nfleets,OMd@nages))
  age05<-array(NA,c(OMd@nsim,OMd@nfleets))
  AFS<-array(NA,c(OMd@nsim,OMd@nfleets))
  Vmaxage<-array(NA,c(OMd@nsim,OMd@nfleets))
    
  for(ff in 1:OMd@nfleets){
    age05[,ff]<-runif(OMd@nsim,OMd@age05[ff,1],OMd@age05[ff,2])
    AFS[,ff]<-runif(OMd@nsim,OMd@AFS[ff,1],OMd@AFS[ff,2])
    Vmaxage[,ff]<-runif(OMd@nsim,OMd@Vmaxage[ff,1],OMd@Vmaxage[ff,2])
    sel[,ff,]<-getDNvulnS(AFS[,ff],age05[,ff],Vmaxage[,ff],OMd@nages,OMd@nsim)  
  }

  .Object@nfleets<-OMd@nfleets
  .Object@age05<-age05
  .Object@AFS<-AFS
  .Object@Vmaxage<-Vmaxage
  .Object@sel<-sel
    
  .Object@Fsd=gettempvar3(OMd@Fsd,OMd@nsim,OMd@npop)

  # Interannual variability in F, Final gradient in F yr-1
  .Object@E<-array(NA,c(OMd@nsim,OMd@nfleets,OMd@nyears))
  .Object@dFfinal<-array(NA,c(OMd@nsim,OMd@nfleets))   
    
  for(ff in 1:OMd@nfleets){    
    deriv<-getFhist(.Object@nsim,.Object@Fsd[,ff],OMd@nyears,dFmin=OMd@Fgrad[ff,1],dFmax=OMd@Fgrad[ff,2],bb=2)     # Calculate fishing mortality rate
    .Object@E[,ff,]<-deriv[[1]]     # Calculate fishing mortality rate
    .Object@dFfinal[,ff]<-deriv[[2]]  # Final gradient in F yr-1 
  }
  .Object@Frat<-OMd@Frat
  .Object@Spat_targ<-gettempvar3(OMd@Spat_targ,OMd@nsim,OMd@nfleets)       # Spatial targetting parameter F =prop= V^Spat_targ
  
  .Object@movvar=gettempvar3(OMd@movvar,OMd@nsim,OMd@npop)  
  .Object@movsd=gettempvar3(OMd@movsd,OMd@nsim,OMd@npop)  
  .Object@movgrad=gettempvar3(OMd@movgrad,OMd@nsim,OMd@npop)  
  #.Object@mov<-simmov(OMd@mov,OMd@movvar,OMd@movsd,OMd@movgrad,OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears,OMd@nsubyears,OMd@nareas)
  .Object@mov<-simmov2(OMd@mov,OMd@movvar,OMd@nsim,OMd@npop,OMd@nages,OMd@nsubyears,OMd@nareas)


  .Object@Mmovvar=gettempvar3(OMd@Mmovvar,OMd@nsim,OMd@npop)  
  .Object@Mmovsd=gettempvar3(OMd@Mmovsd,OMd@nsim,OMd@npop)  
  .Object@Mmovgrad=gettempvar3(OMd@Mmovgrad,OMd@nsim,OMd@npop)  
  #.Object@Mmov<-simmov(OMd@Mmov,OMd@Mmovvar,OMd@Mmovsd,OMd@Mmovgrad,OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears,OMd@nsubyears,OMd@nareas)
  .Object@Mmov<-simmov2(OMd@Mmov,OMd@movvar,OMd@nsim,OMd@npop,OMd@nages,OMd@nsubyears,OMd@nareas)

  .Object@excl<-OMd@excl  

  .Object@Area_names<-OMd@Area_names
  .Object@Area_defs<-OMd@Area_defs

  Idist<-array((1-.Object@mat[,,,1])/.Object@nareas,c(.Object@nsim,.Object@npop,.Object@nages,.Object@nareas))
  MIdist<-array(.Object@mat[,,,1]/.Object@nareas,c(.Object@nsim,.Object@npop,.Object@nages,.Object@nareas))
  #Idist<-array(runif(.Object@nsim*.Object@npop*.Object@nages*.Object@nareas),c(.Object@nsim,.Object@npop,.Object@nages,.Object@nareas))
  ind<-as.matrix(expand.grid(1:.Object@nsim,1:.Object@npop,1:.Object@nages,1:.Object@nareas))  
  indPR<-ind[,c(2,4)]
  indP<-ind[,2]
  sumP<-apply(.Object@excl,1,sum) # so that each row (from place) sums to 1 (accounting for maturity that is)
  Idist[ind]<-Idist[ind]*.Object@excl[indPR]*.Object@nareas/sumP[indP] # apply the regional exclusion by population
  MIdist[ind]<-MIdist[ind]*.Object@excl[indPR]*.Object@nareas/sumP[indP]  # apply the regional exclusion by population

  for(i in 1:100){for(mm in 1:4){
    Idist<-domov(Ntemp=Idist,movtemp=.Object@mov[,,,mm,,])
    MIdist<-domov(MIdist,.Object@Mmov[,,,mm,,])
  }}  
  
  .Object@Idist<-Idist
  .Object@MIdist<-MIdist
  # Set up arrays ---------------
  nsim<-OMd@nsim
  npop<-OMd@npop
  nages<-OMd@nages
  nyears<-OMd@nyears
  nsubyears<-OMd@nsubyears
  nareas<-OMd@nareas
  nfleets<-OMd@nfleets
  mdim<-c(nsim,npop,nages,nyears,nsubyears,nareas) 
  fdim<-c(nsim,npop,nfleets,nages,nyears,nsubyears,nareas)
  .Object@SSB<-.Object@NSB<-.Object@SSN<-.Object@NSN<-.Object@N<-.Object@B<-array(NA,mdim)
  .Object@CAA<-.Object@FM<-array(NA,fdim)
  .Object@R0<-gettempvar3(OMd@R0,OMd@nsim,OMd@npop)
  .Object@Recsubyr<-OMd@Recsubyr
   
  .Object@targpop<-OMd@targpop

  cat("Optimizing for user-specified depletion")
  cat("\n")
  
  sfExport(list=c("popdyn","domov2"))
  qopt<-t(sfSapply(1:nsim,getF4dep,.Object))
  #sapply(1,getF4dep,.Object)

  totF<-exp(qopt[,1])
  relF<-matrix(qopt[,3:(1+nfleets)],ncol=nfleets-1)
  relF<-cbind(relF,1-apply(relF,1,sum))
  .Object@q<-totF*relF
  
  cat("Calculating MSY reference points")
  cat("\n")

  MSYrefs<-t(sfSapply(1:nsim,getMSYrefs,.Object,nyears=50))
  #sapply(1,getMSYrefs,.Object)
  #names(MSYrefs)<-c("MSY","BMSY","VBMSY","SSBMSY","UMSY","SSBMSY_B0")
  .Object@MSY<-MSYrefs[,1]
  .Object@BMSY<-MSYrefs[,2]
  .Object@VBMSY<-MSYrefs[,3]
  .Object@SSBMSY<-MSYrefs[,4]
  .Object@UMSY<-MSYrefs[,5]
  .Object@FMSYa<-MSYrefs[,6]
  .Object@SSBMSY_SSB0<-MSYrefs[,7]
  
  .Object@seed<-OMd@seed
  .Object

})


             

# Observation model definition object -------------------------------------------------------------------------------------------------------------------  
setClass("Obs",representation(Name="character",                           
               Ccv="numeric",Cbcv="numeric",                                  # Observation error and bias in total annual catches
               nCAAobs="numeric",nCALobs="numeric", Lcv="numeric",            # Number of annual catch at age (CAA) and catch-at-length (CAL) observations 
               Ibeta="numeric", Icv="numeric",                                # Hyperstability parameter I^beta and observation error in relative abundance indices
               Mbcv="numeric",                                              # Bias in observation of natural mortality rate
               Kbcv="numeric",t0bcv="numeric",Linfbcv="numeric",                # Bias in estimation of growth parameters
               LFCbcv="numeric", LFSbcv="numeric",                            # Bias in observation of length at first capture (LFC) and length at full selection (LFS)
               FMSYbcv="numeric",FMSY_Mbcv="numeric",BMSY_B0bcv="numeric",      # Bias in observaton of FMSY, ratio of FMSY/M, BMSY/B0
               ageMbcv="numeric",                                           # Bias in observation of age at 50% maturity and 
               Dbcv="numeric",Dcv="numeric",                                  # Bias and imprecision in observation of current stock depletion
               Btbcv="numeric",Btcv="numeric",                                # Bias and imprecision in observation of current stock biomass
               Ftbcv="numeric",Ftcv="numeric",                            # Bias and imprecision in observation of current fishing mortality rate
               hbcv="numeric",                                              # Bias in observation of steepness
               Recbcv="numeric",IMSYbcv="numeric",                           # Bias in observation of recent recrutiment, target CPUE (CPUE @ MSY)     
               MSYbcv="numeric",BMSYbcv="numeric"                          # Bias in observation of target catch and biomass (MSY and BMSY)
)) 

setClass("MSE",representation(
  
               # Description
               Name="character",Date="character",Author="character",
               Notes="character",PrimarySource="character",                           
               
               # Dimensions
               nsim="integer",npop="integer",nages="integer",             # MSE dimensions
               nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
               proyears="integer", nMPs="integer", targpop="integer",     # Proyears, number of management procedures
                              
               # Observation model 
               Cimp="numeric",Cb="numeric",Cerr="array",
               Iimp="numeric",Ibeta="numeric",Ierr="array",
               nCAAobs="numeric",nCALobs="numeric",Lcv="numeric",
               Mb="numeric",Kb="numeric",t0b="numeric",Linfb="numeric",
               LFCb="numeric",LFSb="numeric",
               FMSYb="numeric",FMSY_Mb="numeric",BMSY_B0b="numeric",
               ageMb="numeric", 
               Dimp="numeric", Db="numeric",Derr="array",
               Btimp="numeric", Btb="numeric",Bterr="array",
               Ftimp="numeric", Ftb="numeric",Fterr="array",
               
               hb="numeric",
               Recbcv="numeric",
               IMSYb="numeric", MSYb="numeric", BMSYb="numeric",
                          
               # Management quantities
               C="array",
               D="array",
               B_BMSY="array",
               F_FMSY="array",
               B="array",
               SSB="array",
               TAC="array",
                              
               # Performance metrics
               Perf="data.frame",
               POF="array",
               Y="array",
               AAVY="array",
               PB10="array",
               PB50="array",
               PB100="array",
               
               MPs="character"
             
 )) 

setMethod("initialize", "MSE", function(.Object,OM,Obs,MPs="UMSY",interval=3,IE="Umax"){
   
  # Auto-correlation in recrutiment deviations is currently disabled
  set.seed(OM@seed)
  if(class(OM)!='OM'){
    print(paste('Could not run MSE:',deparse(substitute(OMd)),'not of class OM'))
    stop()
  }
  if(class(Obs)!='Obs'){
    print(paste('Could not run MSE:',deparse(substitute(Obs)),'not of class Obs'))
    stop()
  }
  if(class(get(MPs[1]))!='ABT_MP'){
    print(paste('Could not run MSE:',deparse(substitute(MPs[1])),'not of class ABT_MP'))
    stop()
  }
  if(class(get(IE))!='ABT_IE'){
    print(paste('Could not run MSE:',deparse(substitute(IE)),'not of class ABT_IE'))
    stop()
  }
  
  # copy over dimensions ------
  dimslots<-slotNames(OMd)[1:12]
  for(i in 1:12)slot(.Object,dimslots[i])<-slot(OMd,dimslots[i])
  # .Object})
  #  .Object<-new('MSE',OM,Obs)
  # -------------------------------------------------------------------------
 
  cat("Constructing arrays")
  cat("\n")
  flush.console()  
  
  # Dimensions  S P A Y M R
  nsim<-OM@nsim
  npop<-OM@npop
  nyears<-OM@nyears
  proyears<-OM@proyears
  nages<-OM@nages
  nsubyears<-OM@nsubyears
  nareas<-OM@nareas
  nfleets<-OM@nfleets
  targpop<-as.integer(OM@targpop)
  .Object@targpop<-targpop
  allyears<-nyears+proyears
  nMPs<-length(MPs)
  .Object@nMPs<-nMPs
  
  # Define arrays -----------------------------------------------------------
  
  # Management variables
  .Object@C<-.Object@D<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@TAC<-.Object@F_FMSY<-.Object@B_BMSY<-array(NA,c(nMPs,nsim,allyears))
  
  # Run historical simulation ----------------------------------------------
 
  surv=tomt(exp(-apply(OM@M[,,1:nages,1],2:1,cumsum)))
  agearray<-array(rep(1:nages,each=npop),c(npop,nages))
 
  Len_age<-Wt_age<-array(NA,c(nsim,npop,nages,allyears))
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:allyears))
  Len_age[ind]<-OM@Linf[ind[,c(1,2,4)]]*(1-exp(-OM@K[ind[,c(1,2,4)]]*(agearray[ind[,2:3]]-OM@t0[ind[,2]])))
  
  Wt_age[ind]<-OM@a[ind[,2]]*Len_age^OM@b[ind[,2]]
  SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears+1,nsubyears,nareas)) # only need aggregated catch for these purposes
  SSBA<-array(NA,c(nsim,npop,allyears))
  FD<-array(NA,c(nsim,nfleets,allyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  FM<-VB<-C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))
  
  mref<-c(2,3,4,1)
  y<-1
  m<-1
  
  RF<-array(NA,c(nsim,nfleets,nages,nyears))                             # Relative fishing mortality rate for calculation of vulnerable biomass
  ind<-as.matrix(expand.grid(1:nsim, 1:nfleets,1:nages,1:nyears))
  RF[ind]<-OM@q[ind[,1:2]]/OM@nsubyears*OM@sel[ind[,1:3]]*OM@E[ind[,c(1,2,4)]]
  SFAYMR<-as.matrix(expand.grid(1:nsim, 1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  SFAY<-SFAYMR[,1:4]
  
  SPAYMR<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas))     # Set up some array indexes
  SPA<-SPAYMR[,1:3]
  SP<-SPAYMR[,1:2]
  SA<-SPAYMR[,c(1,3)]
  SPAR<-SPAYMR[,c(1:3,6)]
  SPAY<-SPAYMR[,1:4]
  SPAM<-SPAYMR[,c(1:3,5)]
  
  SSN[SPAYMR]<-surv[SPA]*OM@R0[SP]*OM@MIdist[SPAR]                                 # Calculate initial spawning stock numbers
  NSN[SPAYMR]<-surv[SPA]*OM@R0[SP]*OM@Idist[SPAR]                                  # Calculate initial non spawning numbers
  SSB[SPAYMR]<-SSN[SPAYMR]*Wt_age[SPAY]                                     # Calculate spawning stock biomass
  SSB0<-apply(SSB[,,,y,m,],1:2,sum)
  SSBpR<-SSB0/OM@R0                            # Calculate spawning stock biomass per recruit
  
  bR<-log(5*OM@h)/(0.8*SSB0)                                     # Ricker SR params
  aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params
  
  SPAYMRF2<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas,1:nfleets))
  SF2<-SPAYMRF2[,c(1,7)]
  SFA2<-SPAYMRF2[,c(1,7,3)]
  SPRFA2<-SPAYMRF2[,c(1,2,6,7,3)]
  SPFR2<-SPAYMRF2[,c(1,2,7,6)]
  
  cat("Re-running historical simulations")
  cat("\n")
  for(y in 1:nyears){
    cat(".")
    sof<-apply(array(OM@E[,,y]*OM@q,c(nsim,nfleets,nages))*OM@sel,c(1,3),sum)
    sof<-sof/apply(sof,1,max)
    SPAYMR[,4]<-y
    SPAY<-SPAYMR[,1:4]
    SPAYMRF2[,4]<-y
    SPAY2<-SPAYMRF2[,1:4]
    SFAY2<-SPAYMRF2[,c(1,7,3,4)]
    
    for(m in 1:nsubyears){
     
      SPAYMR[,5]<-m
      SPAM<-SPAYMR[,c(1:3,5)]
      SPAYMRF2[,5]<-m
      SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
      SPAYMR2<-SPAYMRF2[,1:6]
      
      VB[SPAYMRF2]<-(NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2]*RF[SFAY2]                    # Calculate prop to vunerable biomass
      VBA[SPAYMR]<-(NSN[SPAYMR]+SSN[SPAYMR])*Wt_age[SPAY]*sof[SA]                          # Calculate actual vunerable biomass
            
      FD[,,y,m,]<-(apply(VB[,,,y,m,,],c(1,5,4),sum)^array(OM@Spat_targ,c(nsim,nfleets,nareas)))/
        array(apply(apply(VB[,,,y,m,,],c(1,5,4),sum)^array(OM@Spat_targ,c(nsim,nfleets,nareas)),1:2,mean),c(nsim,nfleets,nareas)) # distribute E x qs x sel by area according to spatial targetting parameters
      FM[SPAYMRF2]<-RF[SFAY2]*FD[SFYMR2]
      Ftot<-apply(FM[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+OM@M[SPAY]/nsubyears
      
      # harvest fish
      #C[,,,y,m,]<-(SSN[,,y,m,]+NSN[,,y,m,])*(1-exp(-Z[,,y,m,]))*(FM[,,,y,m,]/Z[,,y,m,])
      C[SPAYMRF2]<-(SSN[SPAYMR2]+NSN[SPAYMR2])*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2])
      
      SSN[,,,y,m,]<-SSN[,,,y,m,]*exp(-Z[,,,y,m,])
      NSN[,,,y,m,]<-NSN[,,,y,m,]*exp(-Z[,,,y,m,])
      
      # move fish
      SSN[,,,y,m,]<-domov(SSN[,,,y,m,],OM@Mmov[,,,m,,])
      NSN[,,,y,m,]<-domov(NSN[,,,y,m,],OM@mov[,,,m,,])
            
      #  age individuals
      for(pp in 1:npop){
        if(OM@Recsubyr[pp]==m){
          # age fish
          SSBA[,pp,y]<-apply(SSN[,pp,,y,m,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),1,sum)
          SSBdist<-apply(SSN[,pp,,y,m,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
          TN<-NSN[,pp,1:(nages-1),y,m,]+SSN[,pp,1:(nages-1),y,m,]
          
          # Maturity is refreshed which is dumb: kind of removes the point in modelling movement of modelling mature and non mature fish separtely
          SSN[,pp,2:nages,y,m,]<-TN*array(OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
          NSN[,pp,2:nages,y,m,]<-TN*array(1-OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
          
          # recruit fish
          if(OM@SRrel[pp]==1){    # Beverton-Holt recruitment
            rec<-OM@Recdevs[,pp,y]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y])) 
          }else{              # Most transparent form of the Ricker uses alpha and beta params
            rec<-OM@Recdevs[,pp,y]*aR[,pp]*SSBA[,y]*exp(-bR[,pp]*SSBA[,pp,y])
          }            
          SSN[,pp,1,y,m,]<-rec*OM@mat[,pp,1,y]*SSBdist
          NSN[,pp,1,y,m,]<-rec*(1-OM@mat[,pp,1,y])*SSBdist
        } # if its the right subyear
      } # end of pop
      
      # Send to the next year      
      if(m==nsubyears){
        SSN[,,,y+1,1,]<-SSN[,,,y,nsubyears,]
        NSN[,,,y+1,1,]<-NSN[,,,y,nsubyears,]
      }else{
        SSN[,,,y,m+1,]<-SSN[,,,y,m,]
        NSN[,,,y,m+1,]<-NSN[,,,y,m,]
      }
    } # end of subyear  
  } # end of year  
    
  Bcur<-apply(SSN[,,,nyears,4,]*
              array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)
  D<-Bcur/SSB0
  
  # Generate observation errors ---------------------------------------------

  .Object@Cimp<-runif(nsim,Obs@Ccv[1],Obs@Ccv[2])
  .Object@Cb<-trlnorm(nsim,1,Obs@Cbcv)
  .Object@Cerr<-array(trlnorm(nsim*allyears,rep(.Object@Cb,allyears),rep(.Object@Cimp,allyears)),c(nsim,allyears)) 

  .Object@Iimp<-runif(nsim,Obs@Icv[1],Obs@Icv[2])
  .Object@Ierr<-array(trlnorm(nsim*allyears,1,rep(.Object@Iimp,allyears)),c(nsim,allyears)) 
  .Object@Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))

  .Object@Btimp<-runif(nsim,Obs@Btcv[1],Obs@Btcv[2])
  .Object@Btb<-trlnorm(nsim,1,Obs@Btbcv)
  .Object@Bterr<-array(trlnorm(nsim*allyears,rep(.Object@Btb,allyears),rep(.Object@Btimp,allyears)),c(nsim,allyears)) 
  
  .Object@Mb<-trlnorm(nsim,1,Obs@Mbcv)
  .Object@Kb<-trlnorm(nsim,1,Obs@Kbcv)
  .Object@Linfb<-trlnorm(nsim,1,Obs@Linfbcv)
  .Object@t0b<-rep(1,nsim)

  .Object@MSYb<-trlnorm(nsim,1,Obs@MSYbcv)
  .Object@BMSYb<-trlnorm(nsim,1,Obs@BMSYbcv)
  .Object@IMSYb<-trlnorm(nsim,1,Obs@IMSYbcv)
  .Object@FMSYb<-trlnorm(nsim,1,Obs@FMSYbcv)
  .Object@FMSY_Mb<-trlnorm(nsim,1,Obs@FMSY_Mbcv)
  
  .Object@nCAAobs<-ceiling(runif(nsim,Obs@nCAAobs[1],Obs@nCAAobs[2]))
  
  .Object@ageMb<-trlnorm(nsim,1,Obs@ageMbcv)

  # Run projections ------------------------------------------------
  cat("\n")
  cat("Running projections")
  cat("\n")
  
  sfExport(list=c("XSA","DD","DD_R","UMSY","tiny"))
  upyrs<-nyears+(0:(floor(OM@proyears/interval)-1))*interval  # the years in which there are updates (every three years)
  
  sof<-apply(array(OM@E[,,nyears]*OM@q,c(nsim,nfleets,nages))*OM@sel,c(1,3),sum)
  sof<-sof/apply(sof,1,max)
  SFAY1<-SFAY2
  Find<-as.matrix(expand.grid(1:nsim,1:npop,1:nareas,1:nfleets))[,c(1,2,4,3)]
  FindSF<-Find[,c(1,3)]
  FindSPR<-Find[,c(1,2,4)]
  SPFR3<-as.matrix(expand.grid(1:nsim,1:npop,1:nfleets,1:nareas))
  SPR3<-SPFR3[,c(1,2,4)]
  testC<-array(NA,c(nsim,npop,nfleets,nareas))
  CAdist<-array(NA,c(nsim,npop,nareas,nfleets,nages))
  CAA<-sampCatch(apply(C[,,,1:(nyears-1),,,],c(1,3,4),sum),.Object@nCAAobs)
  nCALbins<-30
  CAL_bins<-seq(0,max(OM@Linf),length.out=nCALbins)
  CAL_bins<-c(CAL_bins,CAL_bins[nCALbins]*2)
  CAL<-makeCAL(CAA,Linf=OM@Linf[,1,1:nyears],K=OM@K[,1,1:nyears],t0=OM@t0[1],CAL_bins)
  
  for(MP in 1:nMPs){
    cat(paste(MP,"/",nMPs," Running MSE for ",MPs[MP],sep=""))  # print a progress report
    cat("\n")
    flush.console()                                                  # update the console
    TAC<-rep(NA,nsim)  # refresh the MP store of TAC among simulations
    for(y in nyears:(nyears+proyears)){
      cat(".")
      if(y%in%upyrs){# Operate MP S P A Y M R
        
        # Simulate imperfect information ----------------------------------------------
        Iobs<-apply(SSBA[,,1:(y-1)],c(1,3),sum)^.Object@Ibeta
        Iobs<-Iobs*.Object@Ierr[,1:(y-1)]
        Iobs<-Iobs/apply(Iobs,1,mean)
       
        if(y!=nyears){
          nuy<-(upyrs[match(y,upyrs)-1]):(y-1)
          nCAA<-sampCatch(apply(C[,,,nuy,,,],c(1,3,4),sum),.Object@nCAAobs)
          CAA<-abind(CAA,nCAA,along=3)
          CAL<-abind(CAL,makeCAL(nCAA,Linf=OM@Linf[,1,nuy],K=OM@K[,1,nuy],t0=OM@t0[1],CAL_bins),along=3)
        }
           
        #SPAYMRF
        pset<-list("Cobs"=apply(C[,,,1:(y-1),,,]*array(Wt_age[,,,1:(y-1)],c(nsim,npop,nages,y-1,nsubyears,nareas,nfleets)),c(1,4),sum)*.Object@Cerr[,1:(y-1)],
                   "Iobs"=Iobs,
                   "K"=OM@K[,1,y-1]*.Object@Kb,
                   "Linf"=OM@Linf[,1,y-1]*.Object@Kb,
                   "t0"=rep(OM@t0[1],nsim),
                   "M"=OM@M[,1,,(y-1)]*.Object@Mb,
                   "Bt"=apply(VBA[,,,(y-1),4,],1,sum)*.Object@Bterr[,(y-1)],
                   "MSY"=OM@MSY*.Object@MSYb,
                   "BMSY"=OM@BMSY*.Object@BMSYb,
                   "UMSY"=OM@UMSY*.Object@FMSYb,
                   "a"=rep(OM@a,nsim),
                   "b"=rep(OM@b,nsim),
                   "nages"=OM@nages,
                   "ageM"=OM@ageM[,1,(y-1)]*.Object@ageMb,
                   "Mat"=OM@mat[,1,,1:(y-1)],
                   "Bt_PI"=apply(VBA[,,,(y-1),4,],1,sum),
                   "UMSY_PI"=OM@UMSY,
                   "CAA"=CAA,
                   "CAL"=CAL,
                   "CAL_bins"=CAL_bins,
                   "MPrec"=TAC
                   )
        assign("pset",pset,envir=globalenv()) # debugging
        sfExport("pset")
        if(MPs[MP]=="XSA")TAC<-sapply(1:nsim,get(MPs[MP]),pset)
        if(MPs[MP]!="XSA")TAC<-sfSapply(1:nsim,get(MPs[MP]),pset)
        #print(TAC)
      }
      .Object@TAC[MP,,y]<-TAC
      for(mm in 1:nsubyears){
    
        SPAYMR[,4]<-y
        SPAYMR[,5]<-mm
        SPAY<-SPAYMR[,1:4]
        SPAM<-SPAYMR[,c(1:3,5)]
    
        SPAYMRF2[,4]<-y
        SPAYMRF2[,5]<-mm
        SPAY2<-SPAYMRF2[,1:4]
        SFAY2<-SPAYMRF2[,c(1,7,3,4)]
        SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
        SPAYMR2<-SPAYMRF2[,1:6]
    
        #VB[SPAYMRF2]<-(NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2]*RF[SFAY1]                    # Calculate prop to vunerable biomass
        
        VBA[SPAYMR]<-(NSN[SPAYMR]+SSN[SPAYMR])*Wt_age[SPAY]*sof[SA]                          # Calculate actual vulnerable biomass
        
        #Ftot<-TAC/apply(VBA[,,,y,m,],1,sum)
        
        #Fdist<-(apply(VB[,,,y,m,,],c(1,4),sum)^array(OM@Spat_targ,c(nsim,npop,nages,nareas,nfleets)))/
         # array(apply(apply(VB[,,,y,m,,],c(1,4),sum)^array(OM@Spat_targ,c(nsim,npop,nages,nareas,nfleets)),1,mean),c(nsim,nareas)) # distribute E x qs x sel by area according to spatial targetting parameters
        #Fdist<-Fdist/(max(Fdist)/OM@UMSY)
                  
        VBs<-apply(VBA[,,,y,mm,],c(1,2,4),sum)
        Fdist[Find]<-VBs[FindSPR]^OM@Spat_targ[FindSF]
        Fdist<-OM@UMSY*Fdist/array(apply(Fdist,1:3,sum),dim(Fdist))
        Btemp<-apply((NSN[,,,y,mm,]+SSN[,,,y,mm,])*array(Wt_age[,,,y],c(nsim,npop,nages,nareas)),c(1,2,4),sum)
        testC[SPFR3]<-(1-exp(-Fdist[SPFR3]))*Btemp[SPR3]
              
        Up<-1-exp(-Fdist)
        Crat<-(TAC/4)/apply(testC,1,sum)
        testU<-Crat*Up
        Fp<-(-log(1-(do.call(IE,list(testU)))))
        testC[SPFR3]<-(1-exp(-Fp[SPFR3]))*Btemp[SPR3]
        
        CAdist[SPRFA2]<-(NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2]*OM@sel[SFA2]
        CAdist<-CAdist/array(apply(CAdist,1:4,sum,na.rm=T),dim(CAdist))
        
        C[SPAYMRF2]<-testC[SPFR2]*CAdist[SPRFA2]
        C[SPAYMRF2][is.na(C[SPAYMRF2])]<-0
        C[SPAYMRF2]<-C[SPAYMRF2]/Wt_age[SPAY2]
        # test: cbind(apply(C[,,,y,m,,],1,sum), TAC/4)
        #FM[SPAYMRF2]<--log(1-(C[SPAYMRF2]/((NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2])))
        Up<-array(C[SPAYMRF2]/(NSN[SPAYMR2]+SSN[SPAYMR2]),c(nsim,npop,nages,nareas,nfleets))
        Up[is.na(Up)|Up<0.00001]<-0.00001
        Up[Up>0.9]<-0.9
        FM[SPAYMRF2]<--log(1-Up)
        #FM[SPAYMRF2][is.na(FM[SPAYMRF2])]<-0
        Ftot<-apply(FM[,,,y,mm,,],1:4,sum)
        Z[SPAYMR]<-Ftot[SPAR]+OM@M[SPAY]/nsubyears
    
        # harvest fish
        SSN[,,,y,mm,]<-SSN[,,,y,mm,]*exp(-Z[,,,y,mm,])
        NSN[,,,y,mm,]<-NSN[,,,y,mm,]*exp(-Z[,,,y,mm,])
    
        # move fish
        SSN[,,,y,mm,]<-domov(SSN[,,,y,mm,],OM@Mmov[,,,mm,,])
        NSN[,,,y,mm,]<-domov(NSN[,,,y,mm,],OM@mov[,,,mm,,])
    
        #  age individuals
        for(pp in 1:npop){
          if(OM@Recsubyr[pp]==mm){
            # age fish
            SSBA[,pp,y]<-apply(SSN[,pp,,y,mm,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),1,sum)
            SSBdist<-apply(SSN[,pp,,y,mm,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
            TN<-NSN[,pp,1:(nages-1),y,mm,]+SSN[,pp,1:(nages-1),y,mm,]
        
            # Maturity is refreshed which is dumb: kind of removes the point in modelling movement of modelling mature and non mature fish separtely
            SSN[,pp,2:nages,y,mm,]<-TN*array(OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
            NSN[,pp,2:nages,y,mm,]<-TN*array(1-OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
        
            # recruit fish
            if(OM@SRrel[pp]==1){    # Beverton-Holt recruitment
              rec<-OM@Recdevs[,pp,y]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y])) 
            }else{              # Most transparent form of the Ricker uses alpha and beta params
              rec<-OM@Recdevs[,pp,y]*aR[,pp]*SSBA[,pp,y]*exp(-bR[,pp]*SSBA[,pp,y])
            }            
            SSN[,pp,1,y,mm,]<-rec*OM@mat[,pp,1,y]*SSBdist
            NSN[,pp,1,y,mm,]<-rec*(1-OM@mat[,pp,1,y])*SSBdist
          } # if its the right subyear
        } # end of pop
      
      
        # Send to the next year      
        if(mm==nsubyears){
          SSN[,,,y+1,1,]<-SSN[,,,y,nsubyears,]
          NSN[,,,y+1,1,]<-NSN[,,,y,nsubyears,]
        }else{
          SSN[,,,y,mm+1,]<-SSN[,,,y,mm,]
          NSN[,,,y,mm+1,]<-NSN[,,,y,mm,]
        }
      } # end of subyear  
      
      
    } # end of year  
    
    # Store results
   
    .Object@C[MP,,,]<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets)),c(1,2,4),sum)
    SSB<-apply(SSN[,,,1:allyears,4,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
    .Object@D[MP,,,]<-SSB/apply(SSB0,1,sum)
    B<-apply((SSN[,,,1:allyears,4,]+NSN[,,,1:allyears,4,])*array(Wt_age,c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
    #Bthen<-apply((SSN[,,,1,4,]+NSN[,,,1,4,])*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
    .Object@B_BMSY[MP,,]<-apply(array(B[,targpop,],dim=c(nsim,length(targpop),allyears)),c(1,3),sum)/OM@BMSY
    U<-apply(array(.Object@C[MP,,targpop,],c(nsim,length(targpop),allyears)),c(1,3),sum)/
      apply(array(VBA[,targpop,,1:allyears,4,],c(nsim,length(targpop),nages,allyears,nareas)),c(1,4),sum)
    .Object@F_FMSY[MP,,]<-U/OM@UMSY
    
    cat("\n")
  } # end of MP
  
  .Object@MPs<-MPs
  
  .Object
  
})

cv<-function(x)  sd(x)/mean(x)
sdconv<-function(m,sd)(log(1+((sd^2)/(m^2))))^0.5        # get log normal standard deviation from transformed space mean and standard deviation
mconv<-function(m,sd)log(m)-0.5*log(1+((sd^2)/(m^2)))    # get log normal mean from transformed space mean and standard deviation
alphaconv<-function(m,sd)m*(((m*(1-m))/(sd^2))-1)
betaconv<-function(m,sd)(1-m)*(((m*(1-m))/(sd^2))-1)
trlnorm<-function(reps,mu,cv)return(rlnorm(reps,mconv(mu,mu*cv),sdconv(mu,mu*cv)))

sampCatch<-function(Csamp,nSamp){
  out<-array(NA,dim(Csamp)) 
  nsim<-dim(Csamp)[1]
  nages<-dim(Csamp)[2]
  nyears<-dim(Csamp)[3]
  for(ss in 1:nsim){
    for(yy in 1:nyears){
      
      Csampo<-Csamp[ss,,yy]
      #assign("Csampot",Csampo,envir=globalenv()) # debugging
      #assign("nsampt",nSamp[ss],envir=globalenv()) # debugging
      if(sum(Csampo)==0)Csampo<-rep(1/nages,nages)
      out[ss,,yy]<-ceiling(rmultinom(1,size=nSamp[ss],Csampo)*sum(Csampo)/nSamp[ss])
      
  }}
  out
}

makeCAL<-function(CAA,Linf,K,t0,CAL_bins,CALsd=0.05){
  ny<-dim(CAA)[3]
  na<-dim(CAA)[2]
  ns<-dim(CAA)[1]
  CALmu<--0.5*CALsd^2 
  nCALbins<-length(CAL_bins)-1
  CAL<-array(NA,dim=c(ns,nCALbins,ny))
  for(i in 1:ns){
    for(j in 1:ny){
      ages<-rep(1:na,CAA[i,,j])+runif(sum(CAA[i,,j]),-0.5,0.5)
      lengths<-Linf[i,j]*(1-exp(-K[i,j]*(ages-t0)))*exp(rnorm(sum(CAA[i,,j]),CALmu,CALsd))
      CAL[i,,j]<-hist(lengths,CAL_bins,plot=F)$counts
    }
  }
  CAL
} 

#install.packages("grImport")
#library(grImport)
