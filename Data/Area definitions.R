
# ================================================================================
# === ABT MSE area definitions ===================================================
# ================================================================================

# Set working directory

setwd("F:/ABT-MSE/")
ploty=F

# -- Names ------------------------------------------------------
AreaNames<-c("GOM","WAtl","WAtlw","GSL","EAtl","EAtlw",
             "Med","WMed","CMed","EMed","BS","Braz","WAfr","WCMed")

# -- Lons / Lats ------------------------------------------------

AreaDefs<-new('list')
# GOM
AreaDefs[[1]]<-data.frame(x=c(-100,-100,-83,-55,-55,-65,-65,-75,-75,-82,-82),
                          y=c( 31,  20,  10, 10, 20, 20, 25, 25, 30, 30, 31))
# WAtl
AreaDefs[[2]]<-data.frame(x=c(-82,-82,-75,-75,-65,-65,-55,-55,-45,-45,-58,-58,-52,-62,-64, -75),
                          y=c(31,  30, 30, 25, 25, 20, 20, 10, 10, 60, 60, 50, 46, 45, 45.5,40))
# WAtlw  Western Atlantic-wide WAtl + GSL
AreaDefs[[3]]<-data.frame(x=c(-82,-82,-75,-75,-65,-65,-55,-55,-45,-45,-58, -58,-75),
                          y=c( 31, 30, 30, 25, 25, 20, 20, 10, 10, 60, 60,  50, 50))
# GSL
AreaDefs[[4]]<-data.frame(x=c(-70,-62,-52,-58,-68),
                          y=c( 48, 45, 46, 50, 50))
# EAtl
AreaDefs[[5]]<-data.frame(x=c(-45,-6, 0, 8, 8,-45),
                          y=c(36, 36,43,52,70,70)) 
# EAtlw
AreaDefs[[6]]<-data.frame(x=c(-45,-10,-6, 0, 8, 8,-45),
                          y=c(10,  10, 36,43,52,70,70)) 
# Med   (WMed + CMed+ EMed)
AreaDefs[[7]]<-data.frame(x=c(-6,0, 37,37,27,  22,5),
                          y=c(36,29,29,38,40.2,47,47))
# WMed
AreaDefs[[8]]<-data.frame(x=c(-6,0, 8, 11,15,17, 15,7),
                          y=c(36,29,29,37,38,39.5,41,49))
# CMed
AreaDefs[[9]]<-data.frame(x=c(8, 11,15,17,  15,9, 22,28,28),
                          y=c(29,37,38,39.5,41,47,47,39,29))
# EMed
AreaDefs[[10]]<-data.frame(x=c(28,38,38,28),
                           y=c(29,29,39,39))
# BS Black Sea
AreaDefs[[11]]<-data.frame(x=c(27,43,43,27),
                           y=c(40.2,40.2,50,50))
# Braz Brazil
AreaDefs[[12]]<-data.frame(x=c(-70,-70,-25,-25,-40),
                           y=c( 10,-30,-30, 0,  10))
# WAfr West Africa
AreaDefs[[13]]<-data.frame(x=c(-25,-40,-45,-45,-6, 30,-25),
                           y=c( 0,  10, 10, 36, 36,-20,-20))
# WCMed West and Central Med
AreaDefs[[14]]<-data.frame(x=c(-6,0, 28,28,27,  22,5),
                          y=c(36,29,29,38,40.2,47,47))

save(AreaNames,file=paste(getwd(),"/Data/AreaNames",sep=""))
save(AreaDefs,file=paste(getwd(),"/Data/AreaDefs",sep=""))


if(ploty){
  
  library(maps)
  library(mapdata)
  cols<-rep(c("#ff000040","#00ff0040","#0000ff40","#00000040","#ff00ff40"),4)

  map(xlim=c(-100,50),ylim=c(-50,60))
  abline(v=(-20:20)*10,col='grey')
  abline(h=(-20:20)*10,col='grey')
  abline(v=0,col="red")
  abline(h=0,col="red")

  for(i in 1:length(AreaNames)){
    polygon(AreaDefs[[i]],col=cols[i])
    text(mean(AreaDefs[[i]]$x),mean(AreaDefs[[i]]$y),AreaNames[i],col='white',font=2,cex=0.8)         
  }
  
}

plotareas<-function(OMd){
  cols<-rep(c("#ff000040","#00ff0040","#0000ff40","#00000040","#ff00ff40"),4)
  map(xlim=c(-100,45),ylim=c(-35,70))
  abline(v=(-20:20)*10,col='grey')
  abline(h=(-20:20)*10,col='grey')
  abline(v=0,col="red")
  abline(h=0,col="red")
  
  for(i in 1:length(OMd@Area_names)){
    polygon(OMd@Area_defs[[i]],col=cols[i])
    text(mean(OMd@Area_defs[[i]]$x),mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='white',font=2,cex=0.8)         
  }
}

