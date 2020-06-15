####################################################
# QUARTER FUNCTIONS

####################################################
# QUARTER DATES
date.quarter <- function(data)
{
T <- NROW(data); dates <- names(data)

# dates as numeric for TS
Y <- as.numeric(substring(dates, 1,4))
Q <- as.numeric(substring(dates, 7,7)) 

# Time Series
newdata <- ts(data, start = c(Y[1], Q[1]), end = c(Y[T], Q[T]), frequency = 4)
return(newdata)
}

####################################################
# ac4Q # QUARTER
sum4 <- function(data)
{
T <- NROW(data); dates <- names(data)
tmp <- rep(NA, (T-3)); names(tmp) <- dates[-seq(1:3)]

for(i in 1:(T-3))
{
    w1     <- seq(i,i+4-1)
    tmp[i] <- sum(data[w1])
}

return(date.quarter(tmp))
}

####################################################
# ret (t/t-1) # QUARTER
ret1.q <- function(data)
{
T <- NROW(data)
tmp1 <- data[-1]; tmp2 <- data[-T]
ret  <- 100*(tmp1/tmp2 - 1)
names(ret[-1]) <- dates

return(date.quarter(ret))
}

####################################################
# ret (t/t-4) # QUARTER
ret4 <- function(data)
{
T    <- NROW(data); dates <- names(data)

id1  <- seq(1,4); id2 <- seq(T-3,T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
ret  <- 100*(tmp1/tmp2 - 1)
names(ret[-4]) <- dates

return(date.quarter(ret))
}

####################################################
# y(t) - y(t-4) # QUARTER
dif4 <- function(data)
{
T    <- NROW(data)

id1  <- seq(1,4); id2 <- seq(T-3,T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
dif  <- tmp1 - tmp2
names(dif[-3]) <- dates

return(date.quarter(dif))
}

####################################################
# MONTH DATES
date.month <- function(data)
{
T <- NROW(data); dates <- names(data)
Y <- as.numeric(substring(dates, 1,4))
M <- as.numeric(substring(dates, 6,7)) 
newdata <- ts(data, start = c(Y[1], Q[1]), end = c(Y[T], Q[T]), frequency = 12)
return(newdata)
}

####################################################
# MONTH
date.month.SIDRA <- function(data)
{
# IPCA DATES (%B %Y) to %Y:%m
# format(Sys.Date(), "%B/%Y")
# format(Sys.Date(), "%Y:%m")
    
# numeric values
data2 <- as.numeric(as.character(data[,-1]))
K     <- NROW(data)

# dates
dates <- data[,1]
m1 <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
m2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for(i in 1:12)
{
dates <- gsub(m1[i], m2[i], dates )
}

M <- substring(dates, 1,2); M1 <- as.numeric(M)
Y <- substring(dates, 4,7); Y1 <- as.numeric(Y) 
names(data2) <- paste0(Y, ":", M)

# newdata
newdata <- ts(data2, start=c(Y1[1], M1[1]), end=c(Y1[K], M1[K]), frequency=12 )

return(newdata)
}

####################################################
date.month.sgs <- function(data)
{
# SGS DATES ("%d/%m/%Y") to  "%Y:%m"
# format(Sys.Date(), "%d/%m/%Y")
# format(Sys.Date(), "%Y:%m")

data1 <- data[,1]
data2 <- as.numeric(data[,2]); T <- NROW(data2)

M <- substring(data1, 4,5); M1 <- as.numeric(M)
Y <- substring(data1, 7,10); Y1 <- as.numeric(Y) 
names(data2) <- paste0(Y, ":", M)

newdata <- ts(data2, start=c(Y1[1], M1[1]), end=c(Y1[T], M1[T]), frequency=12 )

return(newdata)
}

####################################################
# sum 12 meses # MONTH
sum12 <- function(data)
{
T     <- NROW(data); dates <- names(data)
sum.12 <- rep(NA, T-11)

for(i in 1:(T-11))
{
    w1       <- seq(i,i+12-1)
    sum.12[i] <- sum(data[w1])/12
}
names(sum.12) <- dates[-c(1:11)]

return(date.month(sum.12) )
}

####################################################
# ret t/t-12 # MONTH
ret12 <- function(data)
{
T    <- NROW(data); dates <- names(data)

id1  <- seq(1:12); id2  <- seq(T-11, T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
ret  <- 100*(tmp1/tmp2 - 1)
names(ret) <- dates[-id1]

return(date.month(ret) )
}

####################################################
# ret t/t-1 # MONTH
ret1.m <- function(data)
{
T <- NROW(data); dates <- names(data)

tmp1 <- data[-1]; tmp2 <- data[-T]
ret  <- 100*(tmp1/tmp2 - 1)
names(ret) <- dates[-1]

return(date.month(ret) )
}

####################################################
# ret anualizado (ret)^per
ret.an <- function(ret, per)
{
ret.an <- ((1+ret/100)^per - 1)*100
return(ret.an)
}

#######################################################
# AC ANO
ac.yr <- function(index, year)
{
id <- grep(year, names(index) )
return(100*(index[tail(id,1)]/index[(head(id,1)-1)] -1 ) )
}

####################################################
# 
fun.subserie <- function(data, pat)
{
id      <- grep(pat, names(data))
newdata <- data[id]
return(newdata)
}

####################################################
coef.lin <- function(data)
{
# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T)

# But the regression IS in LOG    
reg <- lm(log(data)~t1)
return(coef(reg))
}

####################################################
trend.lin <- function(data)
{
# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T); 

# But the regression IS in LOG    
reg   <- lm(log(data)~t1)    # log linear

# answers are NOT in LOG    
t1 <- exp(date.quarter(reg$fitted.values))
c1 <- data-t1
c2 <- c1/t1

# Growth Gap
g  <- ret1.q(t1)
gg <- ret1.q(data) - g

return(list("fit"=t1, "dif"=c1, "pct"=c2, "g"=g, "gg"=gg)) 
}

####################################################
trend.quad <- function(data)
{

# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T); t2 <- t1^2

# But the regression IS in LOG    
reg <- lm(log(data)~t1+t2) # quad

# answers are NOT in LOG    
t1 <- exp(date.quarter(reg$fitted.values))
c1 <- data-t1
c2 <- c1/t1

# Growth Gap
g  <- ret1.q(t1)
gg <- ret1.q(data) - g

return(list("fit"=t1, "dif"=c1, "pct"=c2, "g"=g, "gg"=gg)) 
}

#############################################
# HF # Hamilton Filter
trend.hf <- function(data, h=8, p=4)

{
# data is NOT in LOG
T <- NROW(data); y <- 100*log(data)

# But the regression IS in LOG    
y.00 <- y[(1+h+p-1):(T-0)] # y(t+h) or y(t)

x <- matrix(NA, nrow = length(y.00), ncol = p)
for(i in 1:p)
{
x[,i] <- y[i:(T-h-(p-i))]  # y(t) or y(t-h-(p-i)) for i=1, ..., p
}

reg <- lm(y.00 ~ x)

# answers are NOT in LOG    
t1 <- date.quarter(reg$fitted.values)
c1 <- date.quarter(reg$residuals)

#
u <- date.quarter(y.00 - x[,p]) # y(t) - y(t-h)

# Growth Gap
g  <- ret1.q(exp(t1/100))
gg <- ret1.q(data) - g

return(list("fit"=t1, "c1"=c1, "u"=u, "g"=g, "gg"=gg)) 
}

####################################################
trend.hpf <- function(data, lambda)
{
require(mFilter)

# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T); t2 <- t1^2

# But the filter IS in LOG    
hpf <- hpfilter(log(data), freq = lambda)
names(hpf$trend) <- names(data)

# answers are NOT in LOG    
t1 <- exp(hpf$trend)
c1 <- data-t1
c2 <- c1/t1

# Growth Gap
g  <- ret1.q(t1)
gg <- ret1.q(data) - g

return(list("fit"=t1, "dif"=c1, "pct"=c2, "g"=g, "gg"=gg)) 
}

####################################################
normalize <- function(data, date)
{
T <- NROW(data)
den <- rep(data[date], T)
newdata <- 100*data/den
return(newdata)
}

####################################################
standard <- function(data)
{
return((data-mean(data))/sd(data))
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

####################################################
# GDP TABS

#######################################################
# TAB NOMINAL GDP
tab.gdp.level <- function(data)
{
# Nominal GDP in trillions of current reais 

# subset dates
dates <- names(data)
id.2018 <- grep("2018", dates)
id.2019 <- grep("2019", dates)

# Nominal
yq1   <- tail(data,1)/10^6       # last quarter
yq4   <- sum(tail(data,4))/10^6  # last 4 quarters
y2018 <- sum(data[id.2018])/10^6 # 2018 
y2019 <- sum(data[id.2019])/10^6 # 2019 so far

# bind values
tab1 <- c(y2018, yq4, yq1, 4*yq1, y2019, 2*y2019)
names(tab1) <- c("2018", "4 quarters", "last quarter", "last quarter x4","2019", "2019 x2")

return(t(tab1))
}

#######################################################
# TAB GDP Growth

tab.gdp.growth <- function(data, date1, date2)
{
# Tab GDP growth quarters

# dates 
dates <- names(data$ret1)
id1   <- grep(date1, dates)
id2   <- grep(date2, dates)

# bind series
tmp <- cbind(data$ret1, data$ret4, data$acret4)
colnames(tmp) <- c("t/t-1", "t/t-4", "ac t/t-4")
rownames(tmp) <- dates

# subset    
tab <- tmp[id1:id2,]
# tab <- window(tmp, start = date1, end = date2)

return(tab)
}

#######################################################
# TAB GDP Growth

tab.gdp.growth.y <- function(data, year1, year2)
{
# Tab GDP growth years

tmp        <- data[grep("Q4", names(data))]
names(tmp) <- substring(names(tmp), 1, 4)

# dates subset
id1 <- grep(year1, names(tmp))
id2 <- grep(year2, names(tmp))
tab <- tmp[id1:id2]

return(t(tab))
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
