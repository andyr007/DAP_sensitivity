##### FIRST GROUP OF SENSITIVITY RUNS
# This is to house sensitivity runs of 7 or 8 sites to be run on one core of 6

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


# First Site --------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCF"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCF"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCF"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCF"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sum.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sum.silt
sand.val = str.sum.sand
bulk.val = str.sum.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sum.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sum_wcf1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Second Site --------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCMF"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCMF"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCMF"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCMF"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sum.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sum.silt
sand.val = str.sum.sand
bulk.val = str.sum.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sum.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sum_wcmf1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Third Site --------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_OPP"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_OPP"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_OPP"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_OPP"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sum.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sum.silt
sand.val = str.sum.sand
bulk.val = str.sum.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sum.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sum_opp1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Fourth Site -------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_Grass"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_Grass"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_Grass"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_Grass"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sum.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sum.silt
sand.val = str.sum.sand
bulk.val = str.sum.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sum.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sum_grass1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Fifth Site --------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WF"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WF"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WF"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WF"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sid.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sid.silt
sand.val = str.sid.sand
bulk.val = str.sid.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sid.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sid_wf1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Sixth Site -------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCF"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCF"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCF"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCF"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sid.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sid.silt
sand.val = str.sid.sand
bulk.val = str.sid.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sid.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sid_wcf1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Seventh Site --------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCMF"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCMF"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCMF"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCMF"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sid.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sid.silt
sand.val = str.sid.sand
bulk.val = str.sid.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sid.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sid_wcmf1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)
# Eighth Site -------------------------------------------------------------

#====================================================================
#### START MAIN PROGRAM:
#====================================================================
# Settings for the simulation runs:
#====================================================================
path = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_OPP"	# path of the modelfolder
climpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_OPP"	# path of the climate data
soilpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_OPP"	# path of the soil data
modelpath = "/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_OPP"	# path of the model
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")
start_year = styr
end_year = enyr
growth_fac = 0.45 # this is the growth factor (only necessary to get the yield)
runs = sens.sims
#====================================================================

setwd(path)

# Set seed for reproducible random LHC sample
set.seed(2)

# Set min and max for variable ranges. Ranges are plus and minus 0.2 * value
clay.val = str.sid.clay
clay.min = clay.val - (clay.val * 0.2)
clay.max = clay.val + (clay.val * 0.2)
silt.val = str.sid.silt
sand.val = str.sid.sand
bulk.val = str.sid.bulk
bulk.min = bulk.val - (bulk.val * 0.2)
bulk.max = bulk.val + (bulk.val * 0.2)
ph.val = str.sid.ph
ph.min = ph.val - (ph.val * 0.2)
ph.max = ph.val + (ph.val * 0.2)

# Build LHC sample
lcube = randomLHS(runs, 5)

# Set variables names on lcube
colnames(lcube) = c("temp", "prec", "clay", "bulk", "ph")

# Convert soil variables to values to use as run inputs
lcube[, "clay"] = clay.min + ((clay.max - clay.min) * lcube[, "clay"])
lcube[, "bulk"] = bulk.min + ((bulk.max - bulk.min) * lcube[, "bulk"])
lcube[, "ph"] = ph.min + ((ph.max - ph.min) * lcube[, "ph"])

# Reverse sign on random selection of half of the samples for temp and prec
# in order to get plus and minus 1 degree or 1 mm

lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "temp"]
lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"] = 0 - lcube[sample(1:nrow(lcube), nrow(lcube) / 2), "prec"]

# DayCent precipitation units are cm, so divide prec by 10
lcube[, "prec"] = lcube[, "prec"] / 10

# Soil values in lcube are used as-is. Weather variables are added to data.
# Soil texture inputs must sum to 1. Keep silt constant, and vary sand with clay.

lcube = cbind(lcube, "silt" = silt.val, "sand" = 1 - (lcube[, "clay"] + silt.val))


# change the climate file in folder
climfile = paste("climate_base.txt", sep="")
CLIM = read.table(climfile,
                  header = FALSE,
                  sep = "\t",
                  colClasses = c(rep("integer", 4), rep("numeric", 3), rep("NULL", 2)))
CLIM_BASE = read.table(climfile, header = FALSE, sep="\t", fill=TRUE)
climout = paste(climpath, "climate.wth", sep = "/")

# Write initial climate input file
write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)

# change the soil file in folder
soilfile = paste("soilfile_base.txt", sep = "")
SOIL_list = read.table(soilfile)
SOIL = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
SOIL_BASE = matrix(as.numeric(unlist(SOIL_list)), nrow = nrow(SOIL_list), ncol = 13)
soilout = paste(soilpath, "soils.in", sep = "/")

# Write initial soil input file
write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)

# Set results file header
header = c("sim",
           "NPP",
           "NEE",
           "YIELD",
           "RESP",
           "Active",
           "Slow",
           "Passive",
           "SOC",
           "N2O",
           "NO",
           "N2",
           "CH4",
           "Tmax",
           "Tmin",
           "prec",
           "pH",
           "BD",
           "clay")

# temporary fix to just rerun global simulation
testvars = c("global", "temp", "prec", "clay", "bulk", "ph")

# Set up simulation loop
for (sim_id in testvars) {
  
  if (sim_id == "global") {
    cat("Running global Monte Carlo simulation\n")
  } else {
    cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "")
  }
  
  # Set name for results file
  outfile = paste("sensitivity-", sim_id, ".txt", sep = "")
  
  # Open results file and set to write mode
  dataout = file(outfile, "w")
  
  # Start writing results to table
  write.table(t(header),
              dataout,
              sep = "       ",
              append = FALSE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  # Start main Monte Carlo simulation loop
  for (run_num in 1:runs){ 
    
    # Set temperature input
    if (sim_id == "temp") {
      CLIM[, 5] = CLIM_BASE[, 5]
      CLIM[, 6] = CLIM_BASE[, 6]
    } else {
      CLIM[, 5] = CLIM_BASE[, 5] + lcube[run_num, "temp"]
      CLIM[, 6] = CLIM_BASE[, 6] + lcube[run_num, "temp"]
    }
    
    Tmax = max(CLIM[, 5])
    Tmin = min(CLIM[, 6])
    
    # Set precipitation input
    if (sim_id == "prec") {
      CLIM[, 7] = CLIM_BASE[, 7]
    } else {
      CLIM[, 7] = CLIM_BASE[ ,7] + lcube[run_num, "prec"]
    }
    
    # Convert negative values to zero, for zero precipitation
    CLIM[CLIM[, 7] < 0, 7] = 0
    
    prectot = sum(CLIM[, 7])
    
    # Set soil inputs
    if (sim_id == "ph") {
      SOIL[, 13] = SOIL_BASE[, 13]
    } else {
      SOIL[, 13] = lcube[run_num, "ph"]
    }
    
    ph = SOIL[1, 13]
    
    if (sim_id == "bulk") {
      SOIL[, 3] = SOIL_BASE[, 3]
    } else {
      SOIL[, 3] = lcube[run_num, "bulk"]
    }
    
    bulk = SOIL[1, 3]
    
    if (sim_id == "clay") {
      SOIL[, 9] = SOIL_BASE[, 9]
      SOIL[, 8] = SOIL_BASE[, 8]
    } else {
      SOIL[, 9] = lcube[run_num, "clay"]
      SOIL[, 8] = lcube[run_num, "sand"]
    }
    
    clay = SOIL[1, 9]
    
    # Write updated climate input file
    write.table(CLIM, climout, row.names = FALSE, col.names = FALSE)
    
    # Write updated soil input file
    write.table(SOIL, soilout, row.names = FALSE, col.names = FALSE)
    
    setwd(modelpath)
    system("./str_sid_opp1.sh")
    
    file_in = "dc_sip.csv"
    file_harv = "harvest.csv"
    file_ghg = "tgmonth.out"
    
    summary_out = get_data1(path = modelpath,
                            file = file_in,
                            growth_fac = growth_fac)
    #harvest<-get_harvest(modelpath,file_harv,start_year,end_year)
    ghg = get_ghg1(path = modelpath,
                   file = file_ghg)
    
    #summary<-c(r,summary,harvest,ghg)
    summary_out = c(run_num, summary_out, ghg, Tmax, Tmin, prectot, ph, bulk, clay)
    # summary_out = c(summary_out, max(T_max), min(T_min), sum(prec), pH[1], BD[1], clay, sand)
    write.table(t(summary_out),
                dataout,
                sep = "\t",
                append = FALSE,
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    
    cat("Run number ", run_num, " of ", runs, "\n",
        "Variable ", grep(sim_id, testvars), " of ", length(testvars), "\n",
        sep = "")
    
  }	# End of Monte Carlo simulation loop
  
  # Close output file
  close(dataout)
  cat("Successful Monte Carlo simulation!\n")
  
}	# End of simulation loop

# End routine

Sys.sleep(0.5)