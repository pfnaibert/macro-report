####################################################
# GDP FIGS

####################################################
# GDP and log trends

fig.gdp.log <- function(gdp, trend)
{
tmp  <- cbind(log(gdp), trend)
rownames(tmp) <- names(gdp); colnames(tmp) <- c("GDP", "linear", "quad", "HP")

dygraph(tmp, main = "Log of GDP in Reais of 1995 with Seasonal Adjustment and Trends") %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom")
}

#######################################################
# GDP and linear trends

fig.gdp.lin <- function(gdp, trend)
{
tmp  <- cbind(gdp, exp(pot))
rownames(tmp) <- names(y); colnames(tmp) <- c("GDP", "linear", "quad", "HP")

dygraph(tmp, main = "GDP in Reais of 1995 with Seasonal Adjustment and Trends") %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom")
}

#######################################################
# GAP of GDP in dif

fig.gap.dif <- function(data)
{
dygraph(data, main = "GAP of GDP in Reais of 1995") %>%
    dyLimit(0,   color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom")
}

#######################################################
# GAP of GDP in pct

fig.gap.pct <- function(data)
{
dygraph(data, main = "GAP of GDP PCT of ACTUAL GDP ") %>%
    dyLimit(0,   color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom")
}

#######################################################
# PLOT GDP AC 4Q

fig.gdp.ac4 <- function(data)
{
dygraph(data, main = "Index GDP in Reais of 1995 AC 4Q 2018:Q4=100") %>%
    dySeries(label = "AC t/t-4") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarChart() %>%
    dyOptions(includeZero = TRUE)
}

#######################################################
# PLOT GDP INDEX SA

fig.gdp.t1 <- function(gdp, ret, date)
{
tmp <- normalize(gdp, date)
tmp <- cbind(tmp, ret); colnames(tmp) <- c("gdp","t/t-1")

dygraph(tmp,
        main = paste0("Index GDP in Reais of 1995 SA ", date, "=100") ) %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarSeries("t/t-1") %>% 
    dyOptions(includeZero = TRUE)
}

#######################################################
# PLOT GDP AC 4Q

fig.gdp.t4 <- function(gdp, ret, date)
{
tmp <- normalize(gdp, date)
tmp <- cbind(tmp, ret); colnames(tmp) <- c("gdp","t/t-4")

dygraph(tmp,
        main = paste0("Index GDP in Reais of 1995 AC 4Q ", date, "=100") ) %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarSeries("t/t-4") %>% 
    dyOptions(includeZero = TRUE)
}

#######################################################
# FIG IPCA

#######################################################
# PLOT IPCA INDEX nova base = 100

fig.ipca.index <- function(data, date)
{
newdata <- normalize(data, date)

dygraph(newdata, main=paste0("IPCA (",date, "=100)" ) )%>% 
    dySeries(label="IPCA") %>%
    dyEvent("2002-01-1", "Lula", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyOptions(axisTickSize = 5, includeZero = TRUE)
}

#######################################################
# PLOT IPCA VARS
fig.ipca <- function(data)
{
ret1 <- ret1.m(data)
ret2 <- ret12(data)

newdata  <- ts.intersect(ret1, ret2)         # ac 12 meses (ret t/t-12)
rownames(newdata) <- names(ret2)
colnames(newdata) <- c("t/t-1", "t/t-12")

dygraph(newdata, main = "IPCA" ) %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarSeries("t/t-1") %>% 
    dyOptions(includeZero = TRUE)
}

#######################################################
# IPCA TABS

#######################################################
# TAB IPCA

#######################################################
tab.ipca <- function(data, year)
{
ret1 <- ret1.m(data);
ret2 <- ret12(data) 
ret3 <- ac.yr(data, year)

newdata  <- c(tail(ret1, 13), ret3, tail(ret2,1) )
names(newdata) <-  c(names(tail(ret1,13) ), "AC ANO", "AC 12" )
    
return(newdata)
}

#######################################################
tab.ipca1 <- function(data, year)
{
ret1 <- ret1.m(data)
ret2 <- ret12(data) 
ret3 <- ac.yr(data, year)

id1 <- grep(year, names(ret1) )
id2 <- grep(year, names(ret2) )

newdata  <- c(ret1[id1], ret3, tail(ret2[id2],1) )
names(newdata) <-  c(names(ret1[id1] ), "AC ANO", "AC 12" )
    
return(newdata)
}

#######################################################
# PLOT IND
fig.ind <- function(data)
{
data1 <- data$ac12; data2 <- ret12(data$index)

newdata  <- ts.intersect(data1, data2)         # ac 12 meses (ret t/t-12)
rownames(newdata) <- names(data1)
colnames(newdata) <- c("AC12", "t/t-12")

# tmp <- window(newdata, start = c(2008,1), end = c(2012,12))

dygraph(newdata, main = "Industrial Production" ) %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarSeries("t/t-12") %>% 
    dyOptions(includeZero = TRUE)
}

#######################################################
# TAB IND
tab.ind <- function(data, year)
{

ret1 <- ret12(data$index)     # t/t-12
ret2 <- data$ac12             # AC 12 meses
ret3 <- ac.yr(data$index, year)     # AC ano

id <- grep(year, names(ret1) )

newdata  <- c(tail(ret1, 13), tail(ret3,1), tail(ret2,1) )
names(newdata) <-  c(names(tail(ret1,13) ), "AC ANO", "AC 12" )

return(newdata)
}

####################################################
#
