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

