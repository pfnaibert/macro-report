#######################################
# PATHS
setwd("~/doing-ryzen/macro-report/")
getwd()
library(jsonlite)

datafolder <- "./data/"

urlmat <- rep(NA, 100)

#######################################
# SIDRA

sidra.urls


#######################################
# SGS

# testes
download.file(sgs.urls[1], paste0(datafolder, "selic.json"))
aa <- fromJSON(paste0(datafolder, "selic.json") )
head(aa)
tail(aa)

#######################################
# Download

head(urlmat)

download.file(url, paste0(datafolder, filename))

#######################################
# load
x  <- read.csv(file=paste0(datafolder, filename), header=FALSE, sep=",")

#######################################
# save
saveRDS(urlmat, paste0(datafolder, "urlmat.rds"))

#######################################
cat("\n ***** FIM DO SCRIPT ***** \n")

