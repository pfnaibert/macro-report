#################################################
# GDP

# packages
library(knitr)
library(stargazer)
library(xtable)
library(dygraphs)

#####################################
# data

source("../funs/download.R")
# download.gdp()
gdp <- load.gdp()

source("../funs/gdp-floats.R")
tab.gdp.year(gdp$ret.ac4q.real, "2012")

#####################################
# TABLES
# Tab GDP growth years
kable(tab.gdp.year(gdp$ret.ac4q.real, "2016"), digits = 1, caption="YEARLY GDP GROWTH")

# TAB GDP LEVEL in trillions of current reais 
source("../funs/gdp-floats.R")
kable(tab.gdp.quarter(gdp), digits=2, caption="GDP in trillions of current reais")

#####################################
# PLOTS

# GDP and log trends
fig.gdp.log(gdp$real.SA, pot)

# GDP and linear trends
fig.gdp.lin(gdp$real.SA, pot)

# GDP GAP

# GAP of GDP in dif
fig.gap.dif(gap$dif)

# GAP of GDP in pct
fig.gap.pct(gap$pct)

# GDP GROWTH

# GDP growth lin est
g <- lin.trend(log(gdp$real.seas))[2]

# PLOT GDP AC 4Q

fig.gdp.ac4(gdp$acret4)

# PLOT GDP INDEX seasonal adjustment t/t-1

fig.gdp.t1(gdp$real.seas, gdp$ret1, "2018:Q4")

# PLOT GDP AC 4Q t/t-4

fig.gdp.t4(gdp$ac, gdp$ret4, "2018:Q4")

#################################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
