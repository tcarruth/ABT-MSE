# ===================================================================================================================
# ==== MSE source code ==============================================================================================
# ===================================================================================================================
cat("Installing and loading libraries")
cat("\n")
if(!require('snowfall'))install.packages('snowfall',repos='http://cran.stat.sfu.ca')
if(!require('maps'))install.packages('maps',repos='http://cran.stat.sfu.ca')
if(!require('mapdata'))install.packages('mapdata',repos='http://cran.stat.sfu.ca')
if(!require('wordcloud'))install.packages('wordcloud',repos='http://cran.stat.sfu.ca')
if(!require('abind'))install.packages('abind',repos='http://cran.stat.sfu.ca')

library(snowfall)
library(maps)
library(mapdata)
library(wordcloud)
library(abind)

#library(FLCore)
#library(FLAssess)
#library(FLXSA)
cat("\n")
cat("Loading objects")
cat("\n")
source("Source/Objects.r")
cat("Loading methods")
cat("\n")
source("Source/Methods.r")
cat("Loading management procedures")
cat("\n")
source("Source/MPs.r")
cat("Loading implementation error models")
cat("\n")
source("Source/IE_models.r")
cat("Loading diagnostics")
cat("\n")
source("Source/Diagnostics.r")

tiny=1E-15

#load("Data/AreaDefs")
#load("Data/AreaNames")

gettempvar<-function(targ,targsd,targgrad,nyears,nsim){   # creates a time series per simulation that has gradient grad and random normal walk wiht sigma
  mutemp<--0.5*targsd^2
  temp<-array(1,dim=c(nsim,nyears))
  for(i in 2:nyears){
    temp[,i]<-temp[,i]*exp(rnorm(nsim,mutemp,targsd))
  }
  yarray<-array(rep((1:nyears)-1,each=nsim),dim=c(nsim,nyears))
  temp<-temp*(1+targgrad/100)^yarray
  targ*temp/apply(temp,1,mean)
}

gettempvar2<-function(mur,sdr,gradr,nsim,npop,nyears){   # creates a time series per simulation that has gradient grad and random normal walk wiht sigma
  
  out<-new('list')
  out1<-array(1,dim=c(nsim,npop,nyears))
  targ<-array(NA,c(nsim,npop))
  targsd<-array(NA,c(nsim,npop))
  targgrad<-array(NA,c(nsim,npop))
  for(pp in 1:npop){
    targ[,pp]<-runif(nsim,mur[pp,1],mur[pp,2])
    targsd[,pp]<-runif(nsim,sdr[pp,1],sdr[pp,2])
    targgrad[,pp]<-runif(nsim,gradr[pp,1],gradr[pp,2])
    mutemp<--0.5*targsd[,pp]^2
    temp<-array(1,dim=c(nsim,nyears))
    for(i in 2:nyears)temp[,i]<-temp[,i]*exp(rnorm(nsim,mutemp,targsd[,pp]))
    yarray<-array(rep((1:nyears)-1,each=nsim),dim=c(nsim,nyears))
    temp<-temp*(1+targgrad[,pp]/100)^yarray
    out1[,pp,]<-targ[,pp]*temp/apply(temp,1,mean)
  }
  out[[1]]<-targ
  out[[2]]<-targsd
  out[[3]]<-targgrad
  out[[4]]<-out1
  out
  
}

gettempvar3<-function(bounds,nsim,npop) array(runif(nsim*npop,rep(t(bounds)[1,],each=nsim),rep(t(bounds)[2,],each=nsim)),c(nsim,npop))


densnormasc<-function(sd1,age_05,mody){
  (0.05-(dnorm(age_05,mody,sd1)/dnorm(mody,mody,sd1)))^2
}

getsdasc<-function(sm,age05,mod){
  optimize(densnormasc,interval=c(0.5,100),age_05=age05[sm],mody=mod[sm])$minimum
}

densnormdesc<-function(sd2,V_maxage,maxy,mody){
  (V_maxage-(dnorm(maxy,mody,sd2)/dnorm(mody,mody,sd2)))^2
}

getsddesc<-function(sm,Vmaxage,maxage,mod){
  optimize(densnormdesc,interval=c(0.5,10000),V_maxage=Vmaxage[sm],maxy=maxage,mody=mod[sm])$minimum
}

getDNvulnS<-function(mod,age05,Vmaxage,maxage,nsim){
  sd_asc<-sapply(1:nsim,getsdasc,age05=age05,mod=mod)
  sd_desc<-sapply(1:nsim,getsddesc,Vmaxage=Vmaxage,maxage=maxage,mod=mod)
  V<-array(NA,dim=c(nsim,maxage))
  for(i in 1:nsim){
    V[i,1:ceiling(mod[i])]<-dnorm(1:ceiling(mod[i]),mod[i],sd_asc[i])
    V[i,(1+ceiling(mod[i])):maxage]<-dnorm((1+ceiling(mod[i])):maxage,mod[i],sd_desc[i])
    V[i,(1+ceiling(mod[i])):maxage]<-V[i,(1+ceiling(mod[i])):maxage]/V[i,1+ceiling(mod[i])]#/V[i,floor(mod[i])+1]
    V[i,1:ceiling(mod[i])]<-V[i,1:ceiling(mod[i])]/dnorm(mod[i],mod[i],sd_asc[i])#,mod[i],sd_asc[i])#V[i,floor(mod[i])]
    
  }
  #outy<-new('list')
  #outy[[1]]<-V
  #outy[[2]]<-mod-1.18*sd_asc
  V
}

getFhist<-function(nsim,Esd,nyears,dFmin,dFmax,bb){
  
  ne<-nsim*3                                                         # Number of simulated effort datasets
  dEfinal<-runif(ne,dFmin,dFmax)#(exp(rnorm(ne,mean=demu,sd=desd))-1)*6               # Sample the final gradient in effort
  a<-(dEfinal-bb)/nyears                                         # Derive slope to get there from intercept
  a<-array(a,dim=c(ne,nyears))                                  # Slope array
  bb<-array(bb,dim=c(ne,nyears))                                  # Intercept array
  x<-array(rep(1:nyears,each=ne),dim=c(ne,nyears))              # Year array
  dE<-a*x+bb                                                     # Change in effort
  E<-array(NA,dim=c(ne,nyears))                                 # Define total effort array
  E[,1]<-dE[,1]
  for(y in 2:nyears){
    E[,y]<-apply(dE[,1:y],1,sum)
  }
  E<-E/array(apply(E,1,mean),dim=c(ne,nyears))                  # Standardise Effort to average 1
  cond<-apply(E,1,min)>0
  pos<-(1:ne)[cond]
  pos<-pos[1:nsim]
  #environment("dEfinal")<-asNamespace('DLMtool')#assign("dFfinal",dEfinal[pos],envir=.GlobalEnv)
  
  E<-E[pos,]                                 # Sample only those without negative effort
  Emu<--0.5*Esd^2
  Eerr<-array(exp(rnorm(nyears*nsim,rep(Emu,nyears),rep(Esd,nyears))),c(nsim,nyears))
  outy<-new('list')
  outy[[1]]<-E*Eerr
  outy[[2]]<-dEfinal[pos]
  outy
}

simmov<-function(mov,movvar,movsd,movgrad,nsim,npop,nages,nyears,nsubyears,nareas){
  
  #nsim<-OMd@nsim
  #mov<-OMd@mov
  #movvar<-.Object@movvar
  #movsd<-.Object@movsd
  #movgrad<-.Object@movgrad 
  #npop<-.Object@npop
  #nages<-.Object@nages
  #nyears<-.Object@nyears
  #nsubyears<-.Object@nsubyears
  #nareas<-.Object@nareas
  
  vara<-array(rnorm(nsim*npop*nages*nsubyears*nareas*nareas,0,rep(movvar,nages*nsubyears*nareas*nareas)),dim=c(nsim,npop,nages,nsubyears,nareas,nareas))
  sda<-array(rnorm(nsim*prod(dim(mov)),0,rep(movsd,nages*nyears*nsubyears*nareas*nareas)),dim=c(nsim,npop,nages,nyears,nsubyears,nareas,nareas))
  smov<-array(NA,c(nsim,dim(mov)))
  tmov<-log(mov/(1-mov)) # logit transform
  tmov[tmov==Inf]<-0
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nareas))
  indv<-ind[,c(1,2,3,5,6,7)]
  indm<-ind[,2:7]
  smov[ind]<-tmov[indm]+vara[indv]+sda[ind]
  ssum<-apply(exp(smov),1:6,sum)
  exp(smov)/array(ssum,c(dim(ssum),nareas))

}


simmov2<-function(mov,movvar,nsim,npop,nages,nsubyears,nareas){
  
  #mov<-array(NA,c(npop,nages,nsubyears,nareas,nareas))
  vara<-array(rnorm(nsim*npop*nages*nsubyears*nareas*nareas,0,rep(movvar,nages*nsubyears*nareas*nareas)),dim=c(nsim,npop,nages,nsubyears,nareas,nareas))
  smov<-array(NA,c(nsim,dim(mov)))
  tmov<-log(mov/(1-mov)) # logit transform
  tmov[tmov==Inf]<-0
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nsubyears,1:nareas,1:nareas))
  indm<-ind[,2:6]
  smov[ind]<-tmov[indm]+vara[ind]
  ssum<-apply(exp(smov),1:5,sum)
  exp(smov)/array(ssum,c(dim(ssum),nareas))
  
}


domov<-function(Ntemp,movtemp){ # S P A R  x  S P A R R
  #Ntemp<-Idist
  #movtemp<-.Object@mov[,,,1,1,,]
  nareas<-dim(movtemp)[5]
  apply(array(Ntemp, c(dim(Ntemp),nareas))*movtemp,c(1,2,3,5),sum)
}

domov2<-function(Ntemp,movtemp){ # P A R  x  P A R R
  #Ntemp<-Idist
  #movtemp<-.Object@mov[,,,1,1,,]
  nareas<-dim(movtemp)[4]
  apply(array(Ntemp, c(dim(Ntemp),nareas))*movtemp,c(1,2,4),sum)
}


getF4dep<-function(ss,.Object,toly=1e-1){
  #system.time({
  #method="L-BFGS-B",lower=c(log(0.01),rep(-4,.Object@nfleets-1)),  upper=c(log(2),rep(4,.Object@nfleets-1)),
   
  test<-optimize(popdyn,log(c(0.01,4)),mode=3,npop=.Object@npop,nages=.Object@nages,nyears=.Object@nyears,
              nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
              R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
              Idist=.Object@Idist[ss,,,], MIdist=.Object@MIdist[ss,,,], 
              t0=.Object@t0, Linf=.Object@Linf[ss,,], K=.Object@K[ss,,], a=.Object@a, b=.Object@b,
              sel=.Object@sel[ss,,], E=.Object@E[ss,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
              Mmov=.Object@Mmov[ss,,,,,], Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
              Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=.Object@D[ss,.Object@targpop],
              ratF=.Object@Frat,MSYyear=NA,tol=toly)
  #})
  test2<-popdyn(test$minimum,mode=6,npop=.Object@npop,nages=.Object@nages,nyears=.Object@nyears,
                nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
                R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
                Idist=.Object@Idist[ss,,,], MIdist=.Object@MIdist[ss,,,], 
                t0=.Object@t0, Linf=.Object@Linf[ss,,], K=.Object@K[ss,,], a=.Object@a, b=.Object@b,
                sel=.Object@sel[ss,,], E=.Object@E[ss,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
                Mmov=.Object@Mmov[ss,,,,,], Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
                Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=.Object@D[ss,.Object@targpop],
                ratF=.Object@Frat,MSYyear=NA)
  
  c(test$minimum,test2)
}

getMSYrefs<-function(ss,.Object,nyears=40,toly=1e-1){
  #system.time({
  #method="L-BFGS-B",lower=c(log(0.01),rep(-4,.Object@nfleets-1)),  upper=c(log(2),,rep(4,.Object@nfleets-1))
  test<-optimize(popdyn,log(c(0.0001,0.3)),mode=1,npop=.Object@npop,nages=.Object@nages,nyears=nyears,
              nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
              R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
              Idist=.Object@Idist[ss,,,], MIdist=.Object@MIdist[ss,,,], 
              t0=.Object@t0, Linf=.Object@Linf[ss,,], K=.Object@K[ss,,], a=.Object@a, b=.Object@b,
              sel=.Object@sel[ss,,], E=.Object@E[ss,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
              Mmov=.Object@Mmov[ss,,,,,], Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
              Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=NA,ratF=.Object@Frat,MSYyear=.Object@nyears,tol=toly)
  #})
  
  popdyn(test$minimum,mode=4,npop=.Object@npop,nages=.Object@nages,nyears=.Object@nyears,
         nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
         R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
         Idist=.Object@Idist[ss,,,], MIdist=.Object@MIdist[ss,,,], 
         t0=.Object@t0, Linf=.Object@Linf[ss,,], K=.Object@K[ss,,], a=.Object@a, b=.Object@b,
         sel=.Object@sel[ss,,], E=.Object@E[ss,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
         Mmov=.Object@Mmov[ss,,,,,], Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
         Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=NA,ratF=.Object@Frat,MSYyear=.Object@nyears)
}

popdyn<-function(par,.Object,mode,npop,nages,nyears,nsubyears,nareas,nfleets,R0,targD,M,mat,Idist, MIdist,
                 t0, Linf, K, a, b, sel, E, Spat_targ, mov, Mmov, Recsubyr, h, Recdevs,SRrel,
                 targpop=NA,targdep=0.2,ratF=0.5,MSYyear=1){
  
  totF<-exp(par)
  #ratF<-exp(par[2:length(par)])/(1+sum(exp(par)[2:length(par)]))
  qs<-(totF/nsubyears)*c(ratF,1-sum(ratF))  # make the optimizer find equivalent annual F (to avoid mistakes and aid in initializaton)
  
  surv=t(exp(-apply(M[,1:nages,1],1,cumsum)))
  agearray<-array(rep(1:nages,each=npop),c(npop,nages))
  #Nfrac<-surv*mat[,,1]
  Len_age<-Wt_age<-array(NA,c(npop,nages,nyears))
  ind<-indo<-as.matrix(expand.grid(1:npop,1:nages,1:nyears))
  if(mode%in%c(1,2,4,5)){# if MSY refs
    indo[,3]<-rep(MSYyear,npop*nages*nyears)
    M<-array(M[indo],c(npop,nages,nyears))
    mat<-array(mat[indo],c(npop,nages,nyears))
    Recdevs<-array(1,c(npop,nyears))
  }
  Len_age[ind]<-Linf[indo[,c(1,3)]]*(1-exp(-K[indo[,c(1,3)]]*(agearray[indo[,1:2]]-t0[indo[,1]])))
 
  Wt_age<-a*Len_age^b
  SSN<-NSN<-SSB<-Z<-array(NA,c(npop,nages,nyears+1,nsubyears,nareas)) # only need aggregated catch for these purposes
  FD<-array(NA,c(nfleets,nyears,nsubyears,nareas))              # Fishing distribution
  FM<-VB<-C<-array(NA,c(npop,nages,nyears,nsubyears,nareas,nfleets))
  
  mref<-c(2,3,4,1)
  y<-1
  m<-1
  
  RF<-array(NA,c(nfleets,nages,nyears))                         # Relative fishing mortality rate for calculation of vulnerable biomass
  ind<-indo<-as.matrix(expand.grid(1:nfleets,1:nages,1:nyears))
  if(mode%in%c(1,2,4,5))indo[,3]<-rep(MSYyear,nfleets*nages*nyears) # if MSY refs
  RF[ind]<-qs[indo[,1]]*sel[indo[,1:2]]*E[indo[,c(1,3)]]
  FAYMR<-as.matrix(expand.grid(1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  FAY<-FAYMR[,1:3]
  
  PAYMR<-as.matrix(expand.grid(1:npop,1:nages,y,m,1:nareas))    # Set up some array indexes
  PA<-PAYMR[,1:2]
  P<-PAYMR[,1]
  PAR<-PAYMR[,c(1,2,5)]
  PAY<-PAYMR[,1:3]
  PAM<-PAYMR[,c(1,2,4)]
  
  SSN[PAYMR]<-surv[PA]*R0[P]*MIdist[PAR]                        # Calculate initial spawning stock numbers
  NSN[PAYMR]<-surv[PA]*R0[P]*Idist[PAR]                         # Calculate initial non spawning numbers
  SSB[PAYMR]<-SSN[PAYMR]*Wt_age[PAY]                            # Calculate spawning stock biomass
  SSB0<-apply(SSB[,,y,m,],1,sum)
  SSBpR<-SSB0/R0                            # Calculate spawning stock biomass per recruit
  
  bR<-log(5*h)/(0.8*SSB0)                                     # Ricker SR params
  aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params
  
  if(mode%in%c(1,2,4,5)){ # Start MSY optimization at 40% B0
    SSN[PAYMR]<-SSN[PAYMR]*0.3
    NSN[PAYMR]<-NSN[PAYMR]*0.3
    SSB[PAYMR]<-SSB[PAYMR]*0.3
  }
  
  PAYMRF2<-as.matrix(expand.grid(1:npop,1:nages,y,m,1:nareas,1:nfleets))
  F2<-PAYMRF2[,6]
  PAY2<-PAYMRF2[,c(1,2,3)]
  FAY2<-PAYMRF2[,c(6,2,3)]
  FYMR2<-PAYMRF2[,c(6,3,4,5)]
  PAYMR2<-PAYMRF2[,1:5]
  
  for(y in 1:nyears){
    PAYMR[,3]<-y
    PAY<-PAYMR[,1:3]
    PAYMRF2[,3]<-y
    PAY2<-PAYMRF2[,c(1,2,3)]
    FAY2<-PAYMRF2[,c(6,2,3)]
    
    for(m in 1:nsubyears){
      
      PAYMR[,4]<-m
      PAM<-PAYMR[,c(1,2,4)]
      PAYMRF2[,4]<-m
      FYMR2<-PAYMRF2[,c(6,3,4,5)]
      PAYMR2<-PAYMRF2[,1:5]
            
      VB[PAYMRF2]<-(NSN[PAYMR2]+SSN[PAYMR2])*Wt_age[PAY2]*RF[FAY2]                    # Calculate vunerable biomass
      FD[,y,m,]<-(apply(VB[,,y,m,,],c(4,3),sum)^Spat_targ)/
        apply(apply(VB[,,y,m,,],c(4,3),sum)^Spat_targ,1,mean) # distribute E x qs x sel by area according to spatial targetting parameters
      FM[PAYMRF2]<-RF[FAY2]*FD[FYMR2]
      Ftot<-apply(FM[,,y,m,,],c(1,2,3),sum)
      Z[PAYMR]<-Ftot[PAR]+M[PAY]/nsubyears
      
      # harvest fish
      #C[,,,y,m,]<-(SSN[,,y,m,]+NSN[,,y,m,])*(1-exp(-Z[,,y,m,]))*(FM[,,,y,m,]/Z[,,y,m,])
      C[PAYMRF2]<-(SSN[PAYMR2]+NSN[PAYMR2])*(1-exp(-Z[PAYMR2]))*(FM[PAYMRF2]/Z[PAYMR2])
      SSN[,,y,m,]<-SSN[,,y,m,]*exp(-Z[,,y,m,])
      NSN[,,y,m,]<-NSN[,,y,m,]*exp(-Z[,,y,m,])
      
      # move fish
      if(mode%in%c(1,2,4,5)){# if MSY refs
        SSN[,,y,m,]<-domov2(SSN[,,y,m,],Mmov[,,m,,])
        NSN[,,y,m,]<-domov2(NSN[,,y,m,],mov[,,m,,])
      }else{
        SSN[,,y,m,]<-domov2(SSN[,,y,m,],Mmov[,,m,,])
        NSN[,,y,m,]<-domov2(NSN[,,y,m,],mov[,,m,,])
      }
      
      #  age individuals
      for(pp in 1:npop){
        if(Recsubyr[pp]==m){
          # age fish
          SSB<-sum(SSN[pp,,y,m,]*array(Wt_age[pp,,y],dim=c(nages,nareas)))
          SSBdist<-apply(SSN[pp,,y,m,]*array(Wt_age[pp,,y],dim=c(nages,nareas)),2,sum)/SSB
          TN<-NSN[pp,1:(nages-1),y,m,]+SSN[pp,1:(nages-1),y,m,]
          
          # Maturity is refreshed which is dumb: kind of removes the point in modelling movement of modelling mature and non mature fish separtely
          SSN[pp,2:nages,y,m,]<-TN*mat[pp,2:nages,y]
          NSN[pp,2:nages,y,m,]<-TN*(1-mat[pp,2:nages,y])
          
          # recruit fish
          if(SRrel[pp]==1){    # Beverton-Holt recruitment
            rec<-Recdevs[pp,y]*((0.8*R0[pp]*h[pp]*SSB)/(0.2*SSBpR[pp]*R0[pp]*(1-h[pp])+(h[pp]-0.2)*SSB)) 
            SSN[pp,1,y,m,]<-rec*mat[pp,1,y]*SSBdist
            NSN[pp,1,y,m,]<-rec*(1-mat[pp,1,y])*SSBdist
          }else{              # Most transparent form of the Ricker uses alpha and beta params
            rec<-Recdevs[pp,y]*aR[pp]*SSB*exp(-bR[pp]*SSB)
            SSN[pp,1,y,m,]<-rec*mat[pp,1,y]*SSBdist
            NSN[pp,1,y,m,]<-rec*(1-mat[pp,1,y])*SSBdist
          }
          
        } # if its the right subyear
      } # end of pop
      
      # Send to the next year      
      if(m==nsubyears){
        SSN[,,y+1,1,]<-SSN[,,y,nsubyears,]
        NSN[,,y+1,1,]<-NSN[,,y,nsubyears,]
      }else{
        SSN[,,y,m+1,]<-SSN[,,y,m,]
        NSN[,,y,m+1,]<-NSN[,,y,m,]
      }
    } # end of subyear  
  } # end of year  
  
  if(mode==1)return(-log(sum(
    array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
    array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))))
  if(mode==2)return(-log(sum(
    array(C[,,nyears,,,],c(npop,nages,nsubyears,nareas,nfleets))*
    array(Wt_age[,,nyears],c(npop,nages,nsubyears,nareas,nfleets)))))
  if(mode==3){
    Bcur<-sum(array(SSN[targpop,,nyears,4,],c(length(targpop),nages,nareas))*
              array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
    D<-Bcur/sum(SSB0[targpop])
    print("---")
    print(par)
    print(exp(par))
    print(D)
    #Crat<-apply(C[targpop,,nyears,,,]*array(Wt_age[targpop,,nyears],c(nages,nsubyears,nareas,nfleets)),4,sum)
    #Crat<-(Crat/sum(Crat))[1:(nfleets-1)]
    #print(paste(D,Crat))
    obj<-((log(D)-log(mean(targdep)))^2)
    #for(i in 1:(nfleets-1))obj<-obj+(log(Crat[i])-log(targC[i]))^2
    return(obj)
  }
  if(mode==4){
    MSY<-sum(array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
             array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))
    BMSY<-sum(
      array((SSN[targpop,,nyears,1,]+NSN[targpop,,nyears,1,]),c(length(targpop),nages,nareas))*
      array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
    
    sof<-apply(E[,MSYyear]*sel*qs*4,2,sum)
    FMSYa<-max(sof)
    sof<-sof/max(sof)
    VBMSY<-sum(array(rep(sof,each=length(targpop)),c(length(targpop),nages,nareas))*
      array((SSN[targpop,,nyears,1,]+NSN[targpop,,nyears,1,]),c(length(targpop),nages,nareas))*
        array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
    
    SSBMSY<-sum(array(SSN[targpop,,nyears,1,],c(length(targpop),nages,nareas))*array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
    UMSY<-MSY/VBMSY
    SSBMSY_B0<-SSBMSY/sum(SSB0[targpop])
    return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSYa,SSBMSY_B0))
  }
  if(mode==5){
    MSY<-sum(array(C[,,nyears,,,],c(npop,nages,nsubyears,nareas,nfleets))*
            array(Wt_age[,,nyears],c(npop,nages,nsubyears,nareas,nfleets)))
    BMSY<-sum(
          array((SSN[,,nyears,1,]+NSN[,,nyears,1,]),c(npop,nages,nareas))*
          array(Wt_age[,,nyears],c(npop,nages,nareas)))
    sof<-apply(E[,MSYyear]*sel*qs*4,2,sum)
    FMSYa<-max(sof)
    sof<-sof/max(sof)
    VBMSY<-sum(array(rep(sof,each=npop),c(npop,nages,nareas))*
                 array((SSN[,,nyears,1,]+NSN[,,nyears,1,]),c(npop,nages,nareas))*
                 array(Wt_age[,,nyears],c(npop,nages,nareas)))
     
    SSBMSY<-sum(array(SSN[,,nyears,4,],c(npop,nages,nareas))*
                array(Wt_age[,,nyears],c(npop,nages,nareas)))
    UMSY<-MSY/VBMSY
    SSBMSY_B0<-SSBMSY/sum(SSB0)
    return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSYa,SSBBMSY_B0))
  }
  if(mode==6){
    Bcur<-sum(array(SSN[targpop,,nyears,4,],c(length(targpop),nages,nareas))*
              array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
    D<-Bcur/sum(SSB0[targpop])
    #Crat<-apply(array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
    #              array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)),5,sum)
    #Crat<-(Crat/sum(Crat))[1:(nfleets-1)]
    #print(paste(D,Crat))
    return(c(D,ratF))
  }
}#


dummyfunc<-function(){
  ss<-1
  par<-log(0.01)
  mode<-3 
  npop<-.Object@npop
  nages<-.Object@nages
  nyears<-.Object@nyears
  #nyears<-.Object100
  nsubyears<-.Object@nsubyears
  nareas<-.Object@nareas
  nfleets<-.Object@nfleets
  R0<-.Object@R0[ss,]
  targD<-.Object@D[ss,]
  M=.Object@M[ss,,,]
  mat=.Object@mat[ss,,,]
  Idist=.Object@Idist[ss,,,]
  MIdist=.Object@MIdist[ss,,,]
  t0=.Object@t0
  Linf=.Object@Linf[ss,,]
  K=.Object@K[ss,,]
  a=.Object@a
  b=.Object@b
  sel=.Object@sel[ss,,]
  E=.Object@E[ss,,]
  Spat_targ=.Object@Spat_targ[ss,]
  mov<-.Object@mov[ss,,,,,]
  Mmov<-.Object@Mmov[ss,,,,,]
  Recsubyr<-.Object@Recsubyr
  h<-.Object@h[ss,]
  Recdevs=.Object@Recdevs[ss,,]
  SRrel=.Object@SRrel
  targpop<-1:3
  targdep<-0.1
  MSYyear<-43
  
  ratF=0.5
}

invent_mov<-function(gravs, visc,notmat,excl,nages,nyears){
  movs<-new('list') # mov and Mmov (non-mature and Mature movement)
  npop<-dim(gravs)[1]
  nareas<-dim(gravs)[3]
  nsubyears<-dim(gravs)[2]
  mov<-array(NA,c(npop,nages,nyears,nsubyears,nareas,nareas))
  ind<-as.matrix(expand.grid(1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nareas))
  mov[ind]<-gravs[ind[,c(1,4,6)]]
  mov[ind[,c(1,2,3,4,5,5)]]<-mov[ind[,c(1,2,3,4,5,5)]]+visc[ind[,c(1,4)]]
  mov<-exp(mov)
  mov[ind]<-mov[ind]*excl[ind[,c(1,6)]]
  Mmov<-mov/array(apply(mov,1:5,sum),dim(mov))
  mov[ind]<-mov[ind]*notmat[ind[,c(1,4,6)]]
  mov<-mov/array(apply(mov,1:5,sum),dim(mov))
  movs[[1]]<-mov
  movs[[2]]<-Mmov
  movs
}
invent_mov2<-function(gravs, visc,notmat,excl,nages){
  movs<-new('list') # mov and Mmov (non-mature and Mature movement)
  npop<-dim(gravs)[1]
  nareas<-dim(gravs)[3]
  nsubyears<-dim(gravs)[2]
  mov<-array(NA,c(npop,nages,nsubyears,nareas,nareas))
  ind<-as.matrix(expand.grid(1:npop,1:nages,1:nsubyears,1:nareas,1:nareas))
  mov[ind]<-gravs[ind[,c(1,3,5)]]
  mov[ind[,c(1,2,3,4,4)]]<-mov[ind[,c(1,2,3,4,4)]]+visc[ind[,c(1,3)]]
  mov<-exp(mov)
  mov[ind]<-mov[ind]*excl[ind[,c(1,5)]]
  Mmov<-mov/array(apply(mov,1:4,sum),dim(mov))
  mov[ind]<-mov[ind]*notmat[ind[,c(1,3,5)]]
  mov<-mov/array(apply(mov,1:4,sum),dim(mov))
  movs[[1]]<-mov
  movs[[2]]<-Mmov
  movs
}

tomt<-function(arr){
  dim<-new('list')
  dims<-dim(arr)
  ndims<-length(dims)
  for(i in 1:ndims)dim[[i]]<-1:dims[i]
  ind<-as.matrix(expand.grid(dim))
  out<-array(NA,dims[ndims:1])
  out[ind[,ndims:1]]<-arr[ind]
  out
}

sdensplot<-function(dens,areadefs,ncolgrad=200,colpal='heat'){
 
  xlimy<-c(-95,40)
  ylimy<-c(-25,60)
  plot(xlimy,ylimy,col="white",axes=F,xlab="",ylab="")
  nbins<-ncolgrad
  bins<-seq(min(dens),max(dens),length.out=nbins+1)
  y<-ceiling((dens-min(dens))/(max(dens)-min(dens))*nbins)
  y[y==0]<-1
  if(colpal=="heat")cols<-heat.colors(ncolgrad,alpha=1)[ncolgrad:1]
  if(colpal=="gray")cols<-gray.colors(ncolgrad, start = 0.25, end = 0.985, gamma = 2.2, alpha = NULL)[ncolgrad:1]
    
  for(i in 1:length(OMd@Area_names)){
    polygon(OMd@Area_defs[[i]],col=cols[y[i]],border='white',density=NA)
    #text(mean(OMd@Area_defs[[i]]$x),mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='white',font=2,cex=0.8)         
  }  
  map(xlim=xlimy,ylim=ylimy,add=T,fill=T,col="light grey")
}  

exportFLR<-function(){
  assign("FLIndex", FLIndex, envir=globalenv())
  assign("FLQuant",FLQuant,envir=globalenv())
  assign("FLStock",FLStock,envir=globalenv())
  assign("FLXSA.control",FLXSA.control,envir=globalenv())
  assign("FLIndices",FLIndices,envir=globalenv())
  assign("FLXSA",FLXSA,envir=globalenv())
  sfExport(list=c("FLIndex","FLQuant","FLStock","FLXSA.control","FLIndices","FLXSA","getrefs","getMSYrefs2"))
}
