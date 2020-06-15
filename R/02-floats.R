#############################################
# packages

library(stargazer)
library(xtable)
library(dygraphs)
library(knitr)
library(xts)
library(mFilter)
library(neverhpfilter)
source("./funs-macro.R")


#############################################
# GDP 
y <- readRDS("./data-R/GDP.rds");
source("./funs-macro.R")

#############################################
# TRENDS of GDP
t1 <- trend.lin(y$real.SA)
t2 <- trend.quad(y$real.SA)
t3 <- trend.hpf(y$real.SA, 1600)
t4 <- trend.hf(y$real.SA)

sd(100*log(y$real.SA/t1$fit) )
sd(100*log(y$real.SA/t2$fit) )
sd(100*log(y$real.SA/t3$fit) )
sd(t4$c1 )
sd(t4$u)

#############################################
# GDP TREND
plot(y$real.SA)
lines(t1$fit, col = "red")
lines(t2$fit, col = "blue")
lines(t3$fit, col = "green")
lines(exp(t4$fit/100), col = "magenta")

# GDP GAP
plot(t1$pct, col = "red")
abline(h=0)
lines(t2$pct, col = "blue")
lines(t3$pct, col = "green")
lines(exp(t4$c1/100)-1, col = "magenta")

# GDP GROWTH
plot(ret1.q(y$real.SA))
abline(h=0)
lines(t1$g, col = "red")
lines(t2$g, col = "blue")
lines(t3$g, col = "green")
lines(t4$g, col = "magenta")

plot(t1$gg, col = "red")
abline(h=0)
lines(t2$gg, col = "blue")
lines(t3$gg, col = "green")
lines(t4$gg, col = "magenta")

#############################################
# industria
ind <- readRDS("./data-R/ind.rds")

kable(t(tab.ind(ind, "2018")), digits = 2)
fig.ind(ind)

#############################################
# ipca
ipca <- readRDS("./data-R/ipca.rds")

# TAB IPCA 
kable(t( tab.ipca(ipca, "2019") ), digits = 2)
kable(t( tab.ipca1(ipca, "2019") ), digits = 2)

# PLOT IPCA 
fig.ipca.index(ipca, "2012:01")
fig.ipca(ipca)

#######################################
# GRAF IPCA nova base = 100
fig.ipca(ipca, "2012:1")

#######################################
# save
saveRDS(ipca.mat, paste0(datafolder, "ipca-mat.rds"))

#######################################
cat("\n ***** FIM DO SCRIPT ***** \n")

#############################################
# gdp tabs
source("./header-gdp.R.R")

#############################################
# Tab GDP growth years
kable(tab.gdp.growth.y(gdp$acret4, "2012", "2018"), digits = 1, caption="YEARLY GDP GROWTH")

#############################################
# TAB GDP in trillions of current reais
kable(tab.gdp.level(gdp$nominal), digits=2, caption = "GDP in trillions of current reais")

#############################################
# TAB GDP GROWTH
kable(tab.gdp.growth(gdp, "2016:Q1", "2019:Q2"), digits=1, caption="GDP GROWTH")

#############################################
# GDP growth lin est
g <- lin.trend(log(gdp$real.seas))[2]

#############################################
# gdp figs

#############################################
# PLOT GDP AC 4Q
fig.gdp.t4(gdp$ac, gdp$ret4, "2018:Q4")

#############################################
# PLOT GDP INDEX seasonal adjustment
fig.gdp.t1(gdp$real.seas, gdp$ret1, "2018:Q4")

#############################################
# PLOT GDP AC 4Q
fig.gdp.ac4(gdp$acret4)

#############################################
# GAP of GDP in dif
fig.gap.dif(gap$dif)

#############################################
# GAP of GDP in pct
fig.gap.pct(gap$pct)

#############################################
# GDP and linear trends
fig.gdp.lin(gdp$real.seas, pot)

#############################################
# GDP and log trends
fig.gdp.log(gdp$real.seas, pot)

#############################################
