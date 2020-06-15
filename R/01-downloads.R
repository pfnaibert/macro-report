#####################################
source("../funs/download.R")

#################################################
# GDP

# download
download.cnt.series()
# load file
gdp <- load.gdp()
saveRDS(gdp, "../data/GDP.rds")

#######################################
# SELIC
download.selic()

#######################################
# IBC

download.ibc()
ibc <- readRDS("./data-R/IBC.rds")

#######################################
# SERVICES

download.pms()
ser <- readRDS("./data-R/pms.rds")

#######################################
# COMMERCE

download.pmc()
com <- readRDS("./data-R/pmc.rds")
str(com)

#######################################
# INDUSTRY

download.pimpfbr()
ind <- readRDS("./data-R/ind.rds")
head(ind$SA)

#######################################
# IPCA

download.ipca()
ipca <- readRDS("./data-R/ipca.rds")
ipca <- window(ipca, start = c(1995, 1), end = c(2019, 8) )


#################################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
