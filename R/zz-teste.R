#!/usr/bin/env Rscript

#######################################
# PATHS
setwd("~/doing-ryzen/macro-report/")
getwd()
library(knitr)

#######################################
# por ipca.file com o nome que tu queres salvar o arquivo
# all no url seleciona todos os dados disponíveis.
# por datafolder na pasta onde tu quer salvar o arquivo

datafolder <- "./data/"
filename   <- "ipca-sidra-1737.csv"
url        <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1737.csv&terr=N&rank=-&query=t/1737/n1/all/v/2266/p/all/d/v2266%2013/l/v%2Bt,,p"

#######################################
# Download

# download.file(url, paste0(datafolder, filename))

#######################################
# load
source("./funs-macro.R")
x  <- read.csv(file=paste0(datafolder, filename), header=FALSE, sep=",")
x1 <- myfun.month(x)
d1 <- names(x1)

# nova base 2002/01 = 100
id.2002 <- grep("2002-01-01", d1);
id4     <- seq(1, id.2002-1);
x2      <- x1[-id4]
x3      <- (x2/x2[1])*100

#######################################
# save
saveRDS(x3, paste0(datafolder, "ipca.rds"))

#######################################
# Transformar
source("./funs-macro.R")

# load
ipca <- readRDS(paste0(datafolder, "ipca.rds"))
nobs <- NROW(ipca)

#######################################
ipca.ret    <- ret.1(ipca)          # ipca ret (var mensal)
ipca.ret.yr <- ret.an(ipca.ret, 12) # ipca ret (var mensal)  anualizada
ipca.ac.12  <- ret.12(ipca)         # ac 12 meses (ret t/t-12)

d1 <- intersect(names(ipca.ret), names(ipca.ret.yr))
d2 <- intersect(names(ipca.ret), names(ipca.ac.12))
d3 <- intersect(d1, d2)

ipca.ret    <- ipca.ret[d3] 
ipca.ret.yr <- ipca.ret.yr[d3]
ipca.ac.12  <- ipca.ac.12[d3]

ipca.mat <- cbind("var"=ipca.ret, "var-an"=ipca.ret.yr, "ac12"=ipca.ac.12)

head(ipca.mat)
tail(ipca.mat)

#   #######################################
#   # ac ano
#   ipca.ac.yr  <- ac.yr(ipca.ret, "2018") # ipca ac ano 2018
#   head(ipca.ac.yr)

#######################################
# save
saveRDS(ipca.mat, paste0(datafolder, "ipca-mat.rds"))

#######################################
cat("\n ***** FIM DO SCRIPT ***** \n")

