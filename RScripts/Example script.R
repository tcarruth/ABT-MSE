
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# --- Object-Oriented Management Strategy Evaluation using parallel processing  ------------

# --- Tom Carruthers   UBC
# --- Laurie Kell      ICCAT        
# --- Campbell Davies  CSIRO  

# Version alpha (preliminary)
# 27th November 2014


# Prerequisites ===================================================================================

rm(list=ls(all=TRUE))                  # Remove all existing objects from environment
set.seed(1)                            # Ensure reproducible results by setting random seed
setwd("F:/ABT-MSE/")                   # Set the working directory
source("Source/MSE_source.r")          # Load the source code
sfInit(parallel=T,cpus=8)              # Initiate the cluster


# Define Operating model ==========================================================================

load("Objects/SCRS_SH2")               # Load an operating model definition (OMd) object
OMd@nsim<-as.integer(8)                # For demonstration do a small number of simulations
plot(OMd)                              # Plot the spatial definition of areas


# Create an Operating Model =======================================================================

OM<-new('OM',OMd)                      # Initialize a new operating model (OM) object
plot(OM)                               # Plot the spatial distribution of mature and immature fish


# Load Observation model ==========================================================================

load("Objects/Good_Obs")               # Load the precise and unbiased observation model ('Good')


# Undertake closed-loop simulation ================================================================

tmse<-new('MSE',OM,Obs,MPs<-c("DD","DD4010","UMSY","UMSY_PI"),interval=3,IE="Umax")


# Summarize results ===============================================================================

plot(tmse)                             # Plot results
summary(tmse)                          # Tabulate results






