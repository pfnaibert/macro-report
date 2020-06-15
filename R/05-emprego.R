#######################################
# rendimentos 
# em reais
filename1  <- "rend-sidra-6389.csv"
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6389.csv&terr=N&rank=-&query=t/6389/n1/all/v/5932/p/all/c11913/96165/l/t%2Bc11913%2Bv,,p"

#######################################
# pessoas ocupadas 
# em milhares
filename2  <- "ocup-sidra-6320.csv"
url2 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6320.csv&terr=N&rank=-&query=t/6320/n1/all/v/4090/p/all/c11913/96165/l/c11913%2Bt%2Bv,,p"

#######################################
# Massa de rendimentos como EMPREGADO
filename3  <- "mass-sidra-6422.csv"
url3 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6422.csv&terr=N&rank=-&query=t/6422/n1/all/v/9408/p/all/l/t%2Bv,,p" # massa como EMPREGADO

#######################################
# load
source("./funs-macro.R")
x1 <- read.csv(file=paste0(datafolder, filename1), header=FALSE, sep=",", skip=5)
x1 <- myfun.month1(x1)

x2 <- read.csv(file=paste0(datafolder, filename2), header=FALSE, sep=",", skip=5)
x2 <- myfun.month1(x2)

# dividir por 10^3 para ficar em milhoes
x3 <- x1*x2/10^3

kable(tail(x3,10))

plot(x3, t="l")

#######################################
# save
saveRDS(x1, paste0(datafolder, "massa.rds"))

#######################################
cat("\n ***** FIM DO SCRIPT ***** \n")

#######################################
#######################################

#######################################
# load
source("./funs-macro.R")
ind  <- readRDS(paste0(datafolder, "massa.rds"))
nobs <- NROW(ind)

#######################################
ind.sum.12 <- sum.12(ind)  # ac 12 meses (ret t/t-12)
var        <- ret.12(ind)  # ret 12 meses (ret t/t-12)
var.ac.12m <- ret.12(ind.sum.12)
ind.ac.yr  <- ac.yr(ind.sum.12, "2018") # 

#######################################
# conferir

cat("\n ***** Indice ***** \n")
kable(tail(x1, 12))

cat("\n ***** Ind ac 12 meses ***** \n")
kable(tail(ind.sum.12, 12), caption="Ind ac 12 meses")

cat("\n ***** var t/t-12 ***** \n")
kable(tail(var, 12))

cat("\n ***** var ac 12 meses ***** \n")
kable(tail(var.ac.12m, 12))

# cat("\n ***** var ac ano ***** \n")
# kable(tail(ind.ac.yr, 12))

#######################################
# concatenar ipcas
inds <- list("index"=ind, "index.yr"= ind.sum.12, "ret"=var, "ret.ac12" = var.ac.12m)
saveRDS(inds, paste0(datafolder, "varejo-list.rds"))

#######################################
cat("\n ***** FIM DO SCRIPT ***** \n")

