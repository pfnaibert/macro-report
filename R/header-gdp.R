#####################################
# load data

source("../funs-macro.R")
gdp <- readRDS("./gdp-data/GDP.rds"); # str(gdp)
pot <- readRDS("./gdp-data/GDP-TREND-UNI.rds"); 
y   <- gdp$real.seas
gap <- fun.gap(y, exp(pot))
