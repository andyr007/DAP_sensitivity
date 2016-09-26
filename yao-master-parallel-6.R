######################################################################################################
##    MASTER SCRIPT TO PERFORM ALL SENSITIVY ANALYSES AT ALL SITES, COMBINE THE RESULTS AND GRAPH   ##
##                               *** ONE SCRIPT TO RULE THEM ALL ***                                ##
######################################################################################################
######################################################################################################

### REMOVE ANYTHING AND EVERYTHING THAT MAY BE ALREADY IN WORKSPACE
rm(list = ls())

### RESET GRAPHICAL PARAMETER SETTINGS TO DEFAULTS AND SET FORMULAE
par(mfrow=c(1,1), mar=c(5, 4, 2, 2) + 0.1)

### LOAD LIBRARIES
library(RCurl)
library(plyr)
library(reshape2)
library(Rcpp)
library(ggplot2)
library(grid)
library(jsonlite)

dodge = position_dodge(width=0.9)

### SET BASE WORKING DIRECTORY
setwd("~/Dropbox/R Projects/")

### LOAD IN FUNCTIONS FROM NUMEROUS FILES

source("RFunctions.R")
source("USDA DAP/Sensitivities/LHC-sens-functions.r")
source("USDA DAP/Sensitivities/LHC-sens-constants.r")

setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")

library(parallel)
no_cores = detectCores()

### RUNNING ALL SPINUPS AND BASELINE SIMS

ptm = proc.time()

if(Sys.info()["sysname"]=="Windows"){
  library(doParallel)
  cl=makeCluster(no_cores)
  registerDoParallel(cl)
} else{
  library(doMC)
  registerDoMC(no_cores)
}
library(foreach)

filelist1=list("linux-base1.R", "linux-base2.R", "linux-base3.R", "linux-base4.R", 
               "linux-base5.R", "linux-base6.R", "linux-base7.R", "linux-base8.R")
returnComputation = 
  foreach(x=filelist1) %dopar%{
    source(x)
  }

proc.time() - ptm
first = as.matrix(proc.time()-ptm)
time = Sys.time()

setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")
write.csv(first, file = "first.csv")
write.csv(time, file = "time1.csv")

### RUNNING SENSITIVITY ANALYSES

ptm = proc.time()

if(Sys.info()["sysname"]=="Windows"){
  library(doParallel)
  cl=makeCluster(6)
  registerDoParallel(cl)
} else{
  library(doMC)
  registerDoMC(6)
}
library(foreach)

filelist2=list("linux6-sens1.R", "linux6-sens2.R", "linux6-sens3.R", "linux6-sens4.R",
               "linux6-sens5.R", "linux6-sens6.R")
returnComputation = 
  foreach(x=filelist2) %dopar%{
    source(x)
  }

proc.time() - ptm
second = as.matrix(proc.time()-ptm)
time = Sys.time()

setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")
write.csv(second, file = "second.csv")
write.csv(time, file = "time2.csv")

#################################################
### COMBINE ALL SENSITIVITY OUTPUTS AND GRAPH ###
#################################################
###

setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")
source("DDC-sens-yao-ste-plot.r")
setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")
source("DDC-sens-yao-str-plot.r")
setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")
source("DDC-sens-yao-wal-plot.r")

p.ste.sens     # Tile plot of sensitivity analysis of single year for SOC
p.str.sens     # Tile plot of sensitivity analysis of single year for SOC
p.wal.sens     # Tile plot of sensitivity analysis of single year for SOC

setwd("~/Dropbox/R Projects/USDA DAP/Sensitivities/")
ggsave(p.ste.sens, file="sterling05_SOC_sens_YAO50runs.pdf", width=300, height=250, units="mm")
ggsave(p.str.sens, file="stratton05_SOC_sens_YAO50runs.pdf", width=300, height=250, units="mm")
ggsave(p.wal.sens, file="walsh05_SOC_sens_YAO50runs.pdf", width=300, height=250, units="mm")
