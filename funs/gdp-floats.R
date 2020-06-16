####################################################
# GDP TABS

#######################################################
# TAB GDP year
tab.gdp.year <- function(data, year1)
{
# Tab GDP growth years
tmp   <- data[grep("Q4", names(data))]
names(tmp) <- dates <- substring(names(tmp), 1, 4)
year2 <- tail(dates,1)

# dates subset
id1 <- grep(year1, dates)
id2 <- grep(year2, dates)
tab <- tmp[id1:id2]

return(t(tab))
}

#######################################################
# TAB GDP quarter
tab.gdp.quarter <- function(data)
{
# Tab GDP growth quarters
dates   <- names(data$nominal)
nominal <- as.numeric(data$nominal)
real    <- as.numeric(data$real.NSA)

# dates 

# Nominal Values
nominal.q1   <- tail(nominal,1)/10^6         # current quarter
nominal.q2   <- head(tail(nominal,5),1)/10^6 # current quarter of last year
nominal.q4   <- sum(tail(nominal,4))/10^6    # current sum of the last 4 quarters

real.q1   <- tail(real,1)/10^6         # current quarter
real.q2   <- head(tail(real,5),1)/10^6 # current quarter of last year
real.q4   <- sum(tail(real,4))/10^6    # current sum of the last 4 quarters


#  # bind series
#  tmp <- cbind(data$ret1, c(rep(NA,3), data$ret4), c(rep(NA,6), data$ret.ac4q.real) )
tab <- cbind("Nominal"=c(nominal.q1, nominal.q2, nominal.q1*4, nominal.q4),
                 "Real"=c(real.q1   , real.q2   , real.q1*4   , real.q4) )

id1 <- tail(dates,1)
id2 <- head(tail(dates,5),1)

rownames(tab) <- c(paste(id1), paste(id2), paste(id1,"An"), paste0(id2,"-",id1) )

return(tab)
}
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

