# This Program will run the DailyDaycent model in all the NCUs and extract output of the model in the required format
# Jagadeesh Yeluriapti and Matthias Kuhnert 
# Univeristy of Aberdeen, Dt: 27 Sep 2010

# Stripped back for use with Rscript.exe

library(lhs)

#====================================================================
#====================================================================
##### SUBROUTINES:
annual_average = function(VAR1, years, start_year, len) {

	vec = which(years %in% start_year)
	VAR = as.vector(mean(VAR1[vec]))
	select_years = as.vector(start_year)

	for(i in 2:len) {
		year = start_year + i - 1
		vec = which(years %in% year)
		VAR = c(VAR,mean(VAR1[vec]))
		select_years = c(select_years, year)
	}

	summary = cbind(select_years, VAR)
	return(summary)
}



annual_maximum = function(VAR1, years, start_year, len) {

	vec = which(years %in% start_year)
	VAR = as.vector(max(VAR1[vec]))
	select_years = as.vector(start_year)

	for(i in 2:len) {
		year = start_year + i - 1
		vec = which(years %in% year)
		VAR = c(VAR,max(VAR1[vec]))
		select_years = c(select_years, year)
	}

	summary = cbind(select_years, VAR)
	return(summary)
}

annual_sum = function(VAR1, years, start_year, len) {

	vec = which(years %in% start_year)
	VAR = as.vector(sum(VAR1[vec]))
	select_years = as.vector(start_year)

	for(i in 2:len) {
		year = start_year + i - 1
		vec = which(years %in% year)
		VAR = c(VAR,sum(VAR1[vec]))
		select_years = c(select_years, year)
	}

	summary = cbind(select_years, VAR)
	return(summary)
}

get_data = function(path, file, start_year, end_year, growth_fac) {

	setwd(path)
	DATA = read.csv(file, header = TRUE, sep=",")
	years1 = floor(DATA[, 1])
	dm = dim(DATA)
	min_row = min(which(years1 %in% start_year))
	max_row = length(years1)
	NPP1 = DATA[min_row:max_row, 32]
	NEE1 = DATA[min_row:max_row, 33]
	BM1 = DATA[min_row:max_row, 34]
	RESP1 = DATA[min_row:max_row, 22]
	SOC1 = rowSums(DATA[min_row:max_row, 52:56])

	years = floor(DATA[min_row:max_row, 1])
	end_year = years[length(years)]
	len = end_year - start_year + 1
	NPP = annual_sum(NPP1, years, start_year, len)
	NEE = annual_sum(NEE1, years, start_year, len)
	BM = annual_maximum(BM1, years, start_year, len)
	RESP = annual_average(RESP1, years, start_year, len)
	SOC11 = annual_average(SOC1, years, start_year, len)

	YIELD = BM
	YIELD[, 2] = YIELD[, 2] * growth_fac * 2 / 100

	summary = cbind(NPP, NEE, YIELD, RESP, SOC11)
	return(summary)
}

# Modify to work with single year - much simpler
get_data1 = function(path, file, growth_fac) {

	setwd(path)
	DATA = read.csv(file, header = TRUE, sep=",")
	years1 = floor(DATA[, 1])
	dm = dim(DATA)
	# min_row = min(which(years1 %in% start_year))
	# max_row = length(years1)
	NPP1 = DATA[, 32]
	NEE1 = DATA[, 33]
	BM1 = DATA[, 34]
	RESP1 = DATA[, 22]
	SOCA1 = DATA[, 53]
	SOCS1 = DATA[, 55]
	SOCP1 = DATA[, 56]
	SOC1 = rowSums(DATA[, 52:56])

	# years = floor(DATA[min_row:max_row, 1])
	# end_year = years[length(years)]
	# len = end_year - start_year + 1
	NPP = sum(NPP1)
	NEE = sum(NEE1)
	BM = max(BM1)
	RESP = sum(RESP1)
	SOCA = mean(SOCA1)
	SOCS = mean(SOCS1)
	SOCP = mean(SOCP1)
	SOC11 = mean(SOC1)

	YIELD = BM * growth_fac * 2 / 100

	summary = cbind(NPP, NEE, YIELD, RESP, SOCA, SOCS, SOCP, SOC11)
	return(summary)
}

# get_harvest = function(path, file, year) {

	# setwd(path)
	# DATA = read.csv(file, header = TRUE, sep=",")
	# years1 = floor(DATA[, 1])
	# dm = dim(DATA)
	# row = min(which(years1 %in% year))
	# HARV = DATA[row, 7]

	# harvest = c(year, HARV)

	# return(HARV)
# }

get_ghg = function(path, file, year, end_year) {

	setwd(path)
	DATA = read.table(file, header = TRUE)
	years1 = floor(DATA[, 1])
	dm = dim(DATA)
	min_row = min(which(years1 %in% year))
	max_row = length(years1)
	N2O1 = DATA[min_row:max_row, 2]
	NO1 = DATA[min_row:max_row, 3]
	N21 = DATA[min_row:max_row, 4]
	CH41 = DATA[min_row:max_row, 5]

	years = floor(DATA[min_row:max_row, 1])
	end_year = years[length(years)]
	len = end_year - year + 1
	N2O = annual_sum(N2O1, years, year, len)
	NO = annual_sum(NO1, years, year, len)
	N2 = annual_sum(N21, years, year, len)
	CH4 = annual_sum(CH41, years, year, len)

	summary = cbind(N2O, NO, N2, CH4)
	return(summary)
}

# Modify to work with single year - much simpler
get_ghg1 = function(path, file) {

	setwd(path)
	DATA = read.table(file, header = TRUE)
	# years1 = floor(DATA[, 1])
	dm = dim(DATA)
	# min_row = min(which(years1 %in% year))
	# max_row = length(years1)
	N2O1 = DATA[, 2]
	NO1 = DATA[, 3]
	N21 = DATA[, 4]
	CH41 = DATA[, 5]

	# years = floor(DATA[min_row:max_row, 1])
	# end_year = years[length(years)]
	# len = end_year - year + 1
	N2O = sum(N2O1)
	NO = sum(NO1)
	N2 = sum(N21)
	CH4 = sum(CH41)

	summary = cbind(N2O, NO, N2, CH4)
	return(summary)
}

# Get filenames for sensitivity analysis output
get.sensout = function (resdir) {
  sensnames = list.files(path = resdir, pattern = "sensitivity")
  if (length(sensnames) > 6) stop("Too many 'sensitivity' files.")
  return(sensnames)
}

contains_results = function (directory) {
  
  flist = list.files(directory)
  present = "sensitivity-global.txt" %in% flist
  return(present)
  
}

extract_result = function (site) {
  
  results = lapply(site, function (x) read.table(x, header = TRUE))
  resnames = lapply(site, function (x) {
    sublist = strsplit(sub(".txt", "", x), "-")
    fname = unlist(sublist)[length(unlist(sublist))]
    return(fname)
  } )
  names(results) = resnames
  return(results)
  
}

calculate_ci = function (results, output) {
  
  # results is a list of result files
  # output is the name of the output variable of interest
  
  uncert = data.frame(sdev = t(as.data.frame(lapply(results, function (x) sd(x[[output]])))),
                      ci = NA)
  
  globalsd = uncert["global", "sdev"]
  tempsd   = uncert["temp", "sdev"]
  precsd   = uncert["prec", "sdev"]
  claysd   = uncert["clay", "sdev"]
  bulksd   = uncert["bulk", "sdev"]
  phsd     = uncert["ph", "sdev"]
  
  uncert["temp", "ci"] = (globalsd - tempsd) / sum(globalsd - tempsd, globalsd - precsd, globalsd - claysd, globalsd - bulksd, globalsd - phsd)
  uncert["prec", "ci"] = (globalsd - precsd) / sum(globalsd - tempsd, globalsd - precsd, globalsd - claysd, globalsd - bulksd, globalsd - phsd)
  uncert["clay", "ci"] = (globalsd - claysd) / sum(globalsd - tempsd, globalsd - precsd, globalsd - claysd, globalsd - bulksd, globalsd - phsd)
  uncert["bulk", "ci"] = (globalsd - bulksd) / sum(globalsd - tempsd, globalsd - precsd, globalsd - claysd, globalsd - bulksd, globalsd - phsd)
  uncert["ph", "ci"]   = (globalsd - phsd)   / sum(globalsd - tempsd, globalsd - precsd, globalsd - claysd, globalsd - bulksd, globalsd - phsd)
  
  uncert$ci = (abs(uncert$ci) / sum(abs(uncert$ci), na.rm = TRUE)) * 100
  
  return(uncert)
  
}